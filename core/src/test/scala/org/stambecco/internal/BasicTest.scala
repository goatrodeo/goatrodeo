/*
 * Copyright 2009 WorldWide Conferencing, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package org.stambecco
package internal

import org.specs._
import org.specs.runner._
import org.specs.Sugar._

import net.liftweb.util._
import Helpers._
import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.actor._

import DontTestMe._

object Sink {
  def hold[F](f: => F): F = synchronized{
    val ret = f
    wait()
    ret
  }

  def release() = synchronized{notifyAll()}
}

class BasicSpec extends SpecificationWithJUnit {
  SimpleWorkMaster.touch
  
  "WorkerImpl" can {
    "figure out messages" in {
      for (i <- 1 to 45) WorkerMgr.find(SimpleId(i))

      WorkerImpl.calculateDispatchMap(new SimpleWorker(new SimpleId(4), null)).size must be >= 2
    }

    "Messages fire correctly" in {
      val inst = WorkerMgr.find(SimpleId(4)).open_!.asInstanceOf[SimpleWorker]
      val i2 : Worker[SimpleId] = inst

      Sink.hold {
        i2 ! Moo(42)
      }

      inst.mooRes must_== 42
    }

    
    "Messages with callbacks fire correctly" in {
      var dog: Long = 0
      val inst: Worker[SimpleId] =  WorkerMgr.find(SimpleId(4)).open_!

      Sink.hold {
        OutsideOfWorker on inst ask MooFu("42") complete {foo => dog = foo.open_!}
      }

      Thread.sleep(10)
      dog must_== 42L
    }

    "Messages with callbacks fire correctly (future)" in {
      var dog: Long = 0
      val inst: Worker[SimpleId] =  WorkerMgr.find(SimpleId(4)).open_!

      for {
        r <- OutsideOfWorker on inst ask MooFu("42") future ;
        v <- r
      } dog = v

      dog must_== 42L
    }

    "Messages with callbacks (alt syntax) fire correctly" in {
      var dog: Long = 0
      val inst: Worker[SimpleId] =  WorkerMgr.find(SimpleId(4)).open_!

      Sink.hold {
        OutsideOfWorker on inst ask MooFu2("42") complete {foo => dog = foo.open_!}
      }
      
      dog must_== 42L
    }

    "Messages with callbacks (alt syntax) fire correctly (future)" in {
      var dog: Long = 0
      val inst: Worker[SimpleId] =  WorkerMgr.find(SimpleId(4)).open_!

      for {
        r <- OutsideOfWorker on inst ask MooFu2("42") future;
        v <- r
      } dog = v

      dog must_== 42L
    }

    "Messages with callbacks (alt syntax) fire correctly (continue)" in {
      var dog: Long = 0
      val inst: Worker[SimpleId] =  WorkerMgr.find(SimpleId(4)).open_!

      Sink.hold {
        for {
          r <- OutsideOfWorker on inst ask MooFu2("42") continue;
          v <- r
        } {
          dog = v
          LAScheduler.execute(() => Sink.release())
        }
      }

      dog must_== 42L
    }

    
    "Timeouts fire correctly" in {
      var gotTimeout = false
      val inst: Worker[SimpleId] =  WorkerMgr.find(SimpleId(4)).open_!

      OutsideOfWorker on inst withTimeout 20.millis ask
      MooDelay("42") complete {
        case ParamFailure(_, _, _, TimeExpired(_)) => gotTimeout = true
        case _ =>
      }

      Thread.sleep(500)  // 100 was too short, failed sometimes (hseeberger)
      gotTimeout must_== true
    }

    "Timeouts fire correctly (future)" in {
      var gotTimeout = false
      val inst: Worker[SimpleId] =  WorkerMgr.find(SimpleId(4)).open_!

      (OutsideOfWorker on inst withTimeout 20.millis ask
       MooDelay("42") future).foreach {
        case ParamFailure(_, _, _, TimeExpired(_)) => gotTimeout = true
        case _ =>
      }
      
      gotTimeout must_== true
    }

    "Transactions commit properly" in {
      val myGuy: Box[SimpleWorker] =
        for {
          worker <- WorkerMgr.find(SimpleId(17))
        } yield {
          worker ! StartTrans(SimpleId(18))
          worker.asInstanceOf[SimpleWorker]
        }

      Thread.sleep(500) // 200 was too short, failed sometimes (hseeberger)

      myGuy.isDefined must_== true
      myGuy.open_!.mooRes must_== 99
      myGuy.open_!.mooRes2 must_== 78
    }

    "Transactions rollback properly" in {
      val myGuy: Box[SimpleWorker] =
        for {
          worker <- WorkerMgr.find(SimpleId(17))
        } yield {
          worker ! StartTrans2(SimpleId(18))
          worker.asInstanceOf[SimpleWorker]
        }


      Thread.sleep(500) // 100 was too short, failed sometimes (hseeberger)

      myGuy.isDefined must_== true
      myGuy.open_!.mooRes must_== -1
    }

    "Transactions chain" in {
      val myGuy: Box[SimpleWorker] =
        for {
          worker <- WorkerMgr.find(SimpleId(4))
        } yield {
          val w =  worker.asInstanceOf[SimpleWorker]
          w.mooRes = 0
          Sink.hold {
            worker ! NextGuy(1)
          }
          w
        }

      myGuy.isDefined must_== true
      myGuy.open_!.mooRes must_== 32
    }

    "Transactions with response" in {
      var dog: Long = 0
      OutsideOfWorker on WorkerMgr.find(SimpleId(4)).open_! ask
      PerformTransfers(List(TransferDef(5, SimpleId(8), SimpleId(4)),
                            TransferDef(6, SimpleId(9), SimpleId(4)),
                            TransferDef(7, SimpleId(10), SimpleId(4)))) complete {foo =>
        dog = foo.open_!}

      Thread.sleep(500) // 100 was too short, failed sometimes (hseeberger)

      dog must_== 22L
    }

    "Transactions with response on rollback" in {
      var dog: Box[QLong] = Full(33L)
      OutsideOfWorker on WorkerMgr.find(SimpleId(4)).open_! ask
      PerformTransfers(List(TransferDef(15, SimpleId(8), SimpleId(4)),
                            TransferDef(5, SimpleId(9), SimpleId(4)),
                            TransferDef(5, SimpleId(10), SimpleId(4))))  complete {foo =>
        dog = foo}

      Thread.sleep(500) // 100 was too short, failed sometimes (hseeberger)

      dog match {
        case ParamFailure(_, _, _, (id: WorkerId, msg: String)) => true must_== true

        case _ => true must_== false
      }

      var d2: Box[QLong] = Empty
      (OutsideOfWorker on
       WorkerMgr.find(SimpleId(4)).open_! ask GetBalance() future).foreach{foo => d2 = foo}

      d2 must_== Full(QLong(22))
    }

    

    "Marking handler as not reentrant works" in {
      var orgBalance: Box[QLong] = Empty

      OutsideOfWorker on WorkerMgr.find(SimpleId(4)).open_! ask GetBalance() complete
      {foo => orgBalance = foo}

      var dog: Box[QLong] = Full(33L)
      (OutsideOfWorker on WorkerMgr.find(SimpleId(4)).open_! ask
       PerformTransfers(List(TransferDef(2, SimpleId(8), SimpleId(4)),
                             TransferDef(3, SimpleId(9), SimpleId(4)),
                             TransferDef(4, SimpleId(9), SimpleId(4)),
                             TransferDef(5, SimpleId(10), SimpleId(4))))
       future).foreach {
        case ParamFailure(_, _, _, (id: WorkerId, msg: String)) => true must_== true

        case _ => true must_== false
      }

      var d2: Box[QLong] = Empty

      (OutsideOfWorker on WorkerMgr.find(SimpleId(4)).open_! ask
       GetBalance() future).foreach {foo =>
        d2 = foo}

      d2 must_== orgBalance
    }
  }

}

trait SimpleMsg extends QBase

case class Moo(num: Int) extends SimpleMsg
case class Moo2(num: Int) extends SimpleMsg

case class MooFu(str: String) extends SimpleMsg with MsgWithResponse[QLong]
case class MooFu2(str: String) extends SimpleMsg with MsgWithResponse[QLong]
case class MooDelay(str: String) extends SimpleMsg with MsgWithResponse[QLong]

case class GetBalance() extends SimpleMsg with MsgWithResponse[QLong]

case class StartTrans(other: SimpleId) extends SimpleMsg
case class StartTrans2(other: SimpleId) extends SimpleMsg
case class CauseRollback(ignore: Int) extends SimpleMsg

case class NextGuy(id: Int) extends SimpleMsg

case class TransferDef(amount: Long, from: SimpleId, to: SimpleId) extends QBase

case class PerformTransfers(xacts: List[TransferDef]) extends SimpleMsg with MsgWithResponse[QLong]

case class SetupTransfer(transfer: TransferDef) extends SimpleMsg
case class DoTransfer(transfer: TransferDef) extends SimpleMsg

final case class SimpleId(id: Long) extends WorkerId {
  type MyType = SimpleId
  type MsgType = SimpleMsg
  type IdType = Long
  def myType = manifest[SimpleId]

  def uniqueNameForFile: String = "simple_"+id
}

object SimpleWorkMaster extends LocalWorkMaster[SimpleId] {
  WorkerMgr.registerWorkMaster(this)

  private var created: Set[SimpleId] = Set()

  def idType = manifest[SimpleId]
  /**
   * The number of worker instances of this type in memory at one time
   */
  protected[this] def numberInMemory: Int = 50

  override protected def created(key: SimpleId) {
    super.created(key)
    created += key
  }

  protected def canUnfreeze(key: SimpleId) = created.contains(key)

  protected def canCreate(key: SimpleId): Boolean = true

  protected def instantiate(key: SimpleId): WorkerType = new SimpleWorker(key, connectionManagerForId)

  def touch = true
}

class SimpleWorker(id: SimpleId, calcFunc: SimpleId => ConnectionManager) extends
WorkerImpl[SimpleId, SimpleMsg](id, calcFunc) with OtherPlace {
  var mooRes = 0
  var mooRes2 = 0
  private var balance: Long = id.id

  def doMoo(in: Moo): Unit = {
    mooRes = in.num
    Sink.release()
  }

  def doMoo2(in: Moo2): Unit = {}

  def doMooFu(in: MooFu) = {
    try {
      QLong(in.str.toLong)
    } finally {
      Sink.release()
    }
  }
  def doStartTrans(msg: StartTrans) {
    for {
      other <- WorkerMgr.find(msg.other)
    } {
      transaction on other complete {mooRes = 99} postCommit 
      {mooRes2 = 78} postRollback {(a, b) => mooRes = -1} send Moo(33)
    }
  }

  def doGetBalance(in: GetBalance): QLong = {
    Sink.release()
    balance
  }

  def doAltMooFu(in: MooFu2, f: Box[QLong] => Unit) {
    f(Full(in.str.toLong))
    Sink.release()
  }

  def doMultiTrans(msg: PerformTransfers, f: Box[QLong] => Unit) {
    transaction postCommit {
      f(Full(QLong(balance)))
    } postRollback {(a,b) =>
      TestRecord.findAll().foreach{r => balance = r.balance}
      f(ParamFailure("rollback", Empty, Empty, (a, b)))} setup

    for {
      transfer <- msg.xacts
      fromWorker <- WorkerMgr.find(transfer.from)
    } transaction on fromWorker send SetupTransfer(transfer)
  }

  def doSetup(msg: SetupTransfer) {
    transaction postRollback {
      (workerid, msg) => TestRecord.findAll().foreach{r => balance = r.balance}
    } update

    transaction notReentrant "doSetup called second time with "+msg

    if (balance < msg.transfer.amount) 
      transaction rollback("Insufficient balance: "+balance+
                           " need "+msg.transfer.amount)

    balance -= msg.transfer.amount
    TestRecord.findAll().foreach(_.balance(balance).save)

    for {
      toWorker <- WorkerMgr.find(msg.transfer.to)
    } {
      transaction on toWorker send DoTransfer(msg.transfer)
    }

  }

  def doTransfer(msg: DoTransfer) {
    transaction postRollback {
      (a, b) => TestRecord.findAll().foreach{r => balance = r.balance}
    } update
    
    balance += msg.transfer.amount
    TestRecord.findAll().foreach(_.balance(balance).save)
  }

  def doStartTrans2(msg: StartTrans2) {
    for {
      other <- WorkerMgr.find(msg.other)
    } {
      transaction on other complete {mooRes = 99} postCommit 
      {mooRes2 = 78} postRollback {
        (a,b) => mooRes = -1} send CauseRollback(0)
    }
  }

  def doRollback(msg: CauseRollback) {
    transaction rollback("No good reason")
  }

  def doNextGuy(msg: NextGuy) {
    transaction complete {
      mooRes += 32
                         
      if (id.id == 4L) {
        Sink.release()
      }
    } setup

    if (msg.id < 40) {
      for {
        next <- WorkerMgr.find(SimpleId(msg.id))
      } transaction on next send NextGuy(msg.id + 1)
    }
  }

  override protected[this] def workerUnfrozen() {
    super.workerUnfrozen()
    def foo(a: => AnyRef) {}

    Schemifier.schemify(true, foo _, TestRecord)
    TestRecord.find() match {
      case Full(_) =>
      case _ => TestRecord.create.balance(balance).save
    }
  }

}

object DontTestMe {
  class TestRecord extends LongKeyedMapper[TestRecord] with IdPK {
    object balance extends MappedLong(this)
    def getSingleton = TestRecord
  }

  object TestRecord extends TestRecord with LongKeyedMetaMapper[TestRecord]
}

trait OtherPlace {
  def doDelay(in: MooDelay) = {
    Thread.sleep(1000)
    QLong(in.str.toLong)
  }
}
