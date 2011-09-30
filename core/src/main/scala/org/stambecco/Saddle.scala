/*
 * Rodeo Lib
 *
 * Copyright 2010 WorldWide Conferencing, LLC
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

import internal._

import java.sql.Connection
import java.util.concurrent.ScheduledFuture
import net.liftweb.actor.{ LAScheduler, SpecializedLiftActor }
import net.liftweb.common.{ Box, Empty, Full, ParamFailure }
import net.liftweb.mapper.{ ConnectionIdentifier, ConnectionManager, DB, DefaultConnectionIdentifier }
import net.liftweb.util.ActorPing
import net.liftweb.util.Helpers._

/**
 * If you are outside the scope of a Worker, you can still ask a Worker a
 * question.
 * @api
 */
object OutsideOfWorker {
  /**
   * invoke this method passing the worker that you want to ask as a parameter
   */
  def on[NIdType <: WorkerId](worker: Worker[NIdType]): PartialAskBuilder[NIdType] = {
    val send = new AskInvoker[NIdType] {
      def apply[T <: QBase, MT <: NIdType#MsgType with MsgWithResponse[T]](success: Box[T] => Unit, ab: AskBuilderImpl[T, NIdType, MT]) {
        val syncObj = new Object
        val ret = new Function1[Box[T], Unit] {
          var fired = false
          var turnMeOff: Box[ScheduledFuture[Unit]] = Empty

          def apply(msg: Box[T]): Unit = syncObj.synchronized {
            if (!fired) {
              fired = true
              turnMeOff.foreach(a => tryo(a.cancel(false)))
              LAScheduler.execute(() => success(msg))
            }
          }
        }

        ret.turnMeOff = ab.timeout.map(d => ActorPing.schedule(() => {syncObj.synchronized{if (!ret.fired) {
                  ret.fired = true
                  LAScheduler.execute(() => success(ParamFailure("timeout", Empty, Empty, TimeExpired(d))))
                }}}, d))

        worker sendMessageWithAction MessageWithAction[NIdType, T, MT](ab.msg, ret)
      }
    }

    PartialAskBuilderImpl[NIdType](send, worker, Full(10 seconds))
  }
}

/**
 * TODO
 * @api
 */
abstract class WorkerImpl[IdType <: WorkerId: Manifest,
                          MsgType <: IdType#MsgType: Manifest]
(val id: IdType,
 private val conMgrFunc: IdType => ConnectionManager) extends Worker[IdType] {
  lazy val receive: PartialFunction[MsgType, Unit] = WorkerImpl.calculateDispatch(this)

  def idManifest: Manifest[IdType] = implicitly

  def msgManifest: Manifest[MsgType] = implicitly

  private type MyRealIdType = IdType

  private var callBackFunc: Box[Box[QBase] => Unit] = Empty

  @volatile
  private var currentTransaction: Box[TransactionId] = Empty

  private var onCommit: List[() => Unit] = Nil
  private var postCommit: List[() => Unit] = Nil
  private var onRollback: List[(WorkerId, String) => Unit] = Nil
  private var transMap: Map[String, Any] = Map()
  private var sentList: List[(WorkerId, Worker[_])] = Nil
  private var rollbackTimer: Box[(Long, ScheduledFuture[Unit])] = Empty
  private var messageSendFuncs: List[() => Unit] = Nil
  private var transactionSendCount: Int = 0
  private var messageClass: String = ""
  private var xaConnMgr: Box[ConnectionManager with DoFinalBit] = Empty

  private var deadTransactions: Map[TransactionId, Long] = Map()

  private trait DoFinalBit {
    def actualCommit()
    def actualRollback()
  }

  /**
   * Returned from the transaction method.  This class (and TransactionBuilder2)
   * capture the information related to the transaction.
   */
  protected[this] trait TransactionBuilder1 {
    /**
     * If the transaction statement is related to another worker, the on
     * method allows you to specify the other worker
     */
    def on[IdType <: WorkerId](other: Worker[IdType]): TransactionBuilder2[IdType]

    /**
     * Rolls the current transaction back.  The msg parameter is a String message
     * to send to the associated postRollback functions
     */
    def rollback(msg: String): Nothing

    /**
     * Mark the particular handler in the worker instance as non-reentrant.
     * The second time notReentrant is encountered for a given message
     * for a given Worker, the transaction will be rolled back.
     */
    def notReentrant(msg: => String): Unit

    /**
     * Add the information provided in this call to the current transaction.
     * update is useful for defining what to do on transaction
     * completion/rollback
     */
    def update: Unit

    /**
     * Set information related to transaction completion.  This call will
     * only be evaluated if there is no other transaction information
     * (postRollback, complete, postCommit) already defined.
     */
    def setup: Unit

    /**
     * Define the action to take when this transaction successfully
     * completes.  This is called before the transaction is committed
     * to the backing RDBMS store.
     */
    def complete(f: => Unit): TransactionBuilder1

    /**
     * Define the action to take when this transaction successfully
     * completes.  This is called after the transaction is committed
     * to the backing RDBMS store.
     */
    def postCommit(f: => Unit): TransactionBuilder1

    /**
     * Define the action to take when this transaction is
     * rolled back.  This is called after the transaction is rolled back
     * from the backing RDBMS store.  The WorkerId parameter is
     * the worker that triggered the rollback and the String is the
     * message passed during the rollback.
     */
    def postRollback(f: (WorkerId, String) => Unit): TransactionBuilder1

    /**
     * Define the timeout for the transaction.  The timeout
     * can only be defined during the first message send.  Any
     * additional timeout definitions will be ignored
     */
    def withTimeout(timespan: TimeSpan): TransactionBuilder1
  }

  protected[this] trait TransactionBuilder2[IdType <: WorkerId] {
    /**
     * Define the action to take when this transaction successfully
     * completes.  This is called before the transaction is committed
     * to the backing RDBMS store.
     */
    def complete(f: => Unit): TransactionBuilder2[IdType]

    /**
     * Define the action to take when this transaction successfully
     * completes.  This is called after the transaction is committed
     * to the backing RDBMS store.
     */
    def postCommit(f: => Unit): TransactionBuilder2[IdType]

    /**
     * Define the action to take when this transaction is
     * rolled back.  This is called after the transaction is rolled back
     * from the backing RDBMS store.  The WorkerId parameter is
     * the worker that triggered the rollback and the String is the
     * message passed during the rollback.
     */
    def postRollback(f: (WorkerId, String) => Unit): TransactionBuilder2[IdType]

    /**
     * Define the timeout for the transaction.  The timeout
     * can only be defined during the first message send.  Any
     * additional timeout definitions will be ignored
     */
    def withTimeout(timespan: TimeSpan): TransactionBuilder2[IdType]

    /**
     * Send the transactional message to the recipient
     */
    def send[MT <: IdType#MsgType](msg: MT): Unit
  }

  private final case class TB1Impl(sucFunc: List[() => Unit],
                                   pcFunc: List[() => Unit],
                                   rbFunc: List[(WorkerId, String) => Unit],
                                   timeout: Box[TimeSpan] = Empty) extends TransactionBuilder1 {
    def on[IdType <: WorkerId](other: Worker[IdType]): TransactionBuilder2[IdType] =
      testThatWereInActor(TB2Impl(other, sucFunc, pcFunc, rbFunc, timeout openOr 10.seconds))


    def setup: Unit = {
      val isNew = currentTransaction.isEmpty
      if (isNew || (onCommit.isEmpty && WorkerImpl.this.postCommit.isEmpty &&
                    onRollback.isEmpty && rollbackTimer.isEmpty)) {
        val tid = currentTransaction openOr {val newId = TransactionId.generate;
                                             currentTransaction = Full(newId);
                                             newId}

        onCommit :::= sucFunc
        WorkerImpl.this.postCommit :::= pcFunc
        onRollback :::= rbFunc

        for (to <- timeout) {
          val when = to.later
          rollbackTimer = Full((when.getTime, ActorPing.schedule(
                () => WorkerImpl.this.outOfBandMessage(RollbackTransaction(tid, id, "Timeout")),
                to
              )))
        }
      }
    }

    def update: Unit = {
      val isNew = currentTransaction.isEmpty
      val tid = currentTransaction openOr {val newId = TransactionId.generate;
                                           currentTransaction = Full(newId);
                                           newId}

      onCommit :::= sucFunc
      WorkerImpl.this.postCommit :::= pcFunc
      onRollback :::= rbFunc

      for (to <- timeout if !rollbackTimer.isDefined) {
	val when = to.later
	rollbackTimer = Full((when.getTime, ActorPing.schedule(
              () => WorkerImpl.this.outOfBandMessage(RollbackTransaction(tid, id, "Timeout")),
              to
            )))
      }
    }

    private def setAndAssert(key: String, value: Any, test: Box[Any] => Boolean, msg: => String): Unit = {
      val curValue: Box[Any] = transMap.get(key)
      transMap += key -> value
      if (test(curValue) == false) rollback(msg)
    }

    def notReentrant(msg: => String): Unit = setAndAssert("$$$_stambecco__$$"+messageClass, 1, _.isEmpty,
                                                          msg)

    def rollback(msg: String): Nothing = throw new RollItBack(msg)
    def complete(f: => Unit): TransactionBuilder1 = testThatWereInActor(copy(sucFunc = sucFunc ::: List(() => f)))
    def postCommit(f: => Unit): TransactionBuilder1 = testThatWereInActor(copy(pcFunc = pcFunc ::: List(() => f)))
    def postRollback(f: (WorkerId, String) => Unit): TransactionBuilder1 = testThatWereInActor(copy(rbFunc =  f :: rbFunc))
    def withTimeout(timespan: TimeSpan): TransactionBuilder1 = testThatWereInActor(copy(timeout = Full(timespan)))
  }

  private final case class TB2Impl[IdType <: WorkerId](
    target: Worker[IdType],
    sucFunc: List[() => Unit],
    pcFunc: List[() => Unit],
    rbFunc: List[(WorkerId, String) => Unit],
    timeout: TimeSpan = 10 seconds) extends TransactionBuilder2[IdType] {
    def complete(f: => Unit): TransactionBuilder2[IdType] = copy(sucFunc = sucFunc ::: List(() => f))
    def postCommit(f: => Unit): TransactionBuilder2[IdType] = copy(pcFunc = pcFunc ::: List(() => f))
    def postRollback(f: (WorkerId, String) => Unit): TransactionBuilder2[IdType] = copy(rbFunc = f :: rbFunc)
    def withTimeout(timespan: TimeSpan): TransactionBuilder2[IdType] = copy(timeout = timespan)
    def send[MT <: IdType#MsgType](msg: MT): Unit = {
      val isNew = currentTransaction.isEmpty
      val tid = currentTransaction openOr {val newId = TransactionId.generate;
                                           currentTransaction = Full(newId);

                                           newId}

      onCommit :::= sucFunc
      WorkerImpl.this.postCommit :::= pcFunc
      onRollback :::= rbFunc

      transactionSendCount += 1

      if (rollbackTimer.isEmpty) {
	val when = timeout.later
	rollbackTimer = Full((when.getTime, ActorPing.schedule(
              () => WorkerImpl.this.outOfBandMessage(RollbackTransaction(tid, id, "Timeout")),
              timeout
            )))
      }

      messageSendFuncs ::= (() => {
          if (xaInfo.isEmpty) {
            WorkerImpl.this.setupConnection()
            xaInfo = List(XAInfo(receivedFrom = Empty))
          }

          val guid = TransactionNodeGUID.generate
          xaInfo = xaInfo match {
            case x :: xs => x.copy(sentTo = x.sentTo + (guid -> (target.id, target))) :: xs
            case x => x
          }

          sentList ::= (target.id, target)
          target.outOfBandMessage(TransactionalMessage[IdType,
                                                       MT,
                                                       MyRealIdType](tid,
                                                                     guid,
                                                                     msg,
                                                                     rollbackTimer.open_!._1,
                                                                     id))
        })
    }
  }

  private final case class XAInfo(receivedFrom: Box[(TransactionNodeGUID, WorkerId, Worker[_])],
                                  serviced: Boolean = false,
                                  sentTo: Map[TransactionNodeGUID, (WorkerId, Worker[_])] = Map()) {
  }

  private var xaInfo: List[XAInfo] = Nil

  def inTransaction_? : Boolean = currentTransaction.isDefined

  private def setupConnection() {
    conMgrFunc match {
      case null =>
      case f => val cm = f(id)

        cm.newConnection(DefaultConnectionIdentifier) match {
          case Full(rawConn) =>
            val wrapped = new ConnectionWrapper(rawConn) {
              override def commit() = {} // no commit for you
              override def rollback() = {
                Thread.dumpStack
                super.rollback()
              }
            }

            xaConnMgr = Full(new ConnectionManager with DoFinalBit {
                def actualCommit() {
                  rawConn.commit()
                  cm.releaseConnection(rawConn)
                }
                def actualRollback() {
                  rawConn.rollback()
                  cm.releaseConnection(rawConn)
                }
                def newConnection(name: ConnectionIdentifier): Box[Connection] = name match {
                  case DefaultConnectionIdentifier => Full(wrapped)
                  case ci => cm.newConnection(ci)
                }
                def releaseConnection(conn: Connection) = if (conn ne wrapped)
                  cm.releaseConnection(conn)
              })

          case _ =>
        }


    }
  }

  private lazy val myActor = new SpecializedLiftActor[Any] {

    private def ef: PartialFunction[Any, Unit] = {
      case ExecFunc(f) => f()
    }

    protected def messageHandler = ef orElse receive.asInstanceOf[PartialFunction[Any, Unit]]


    private[this] def checkCommitOrBegin() {
      xaInfo = xaInfo.map {
        case x if !x.serviced && x.sentTo.size == 0 =>
          for {
            transaction <- currentTransaction
            (guid, _, worker) <- x.receivedFrom
          } worker outOfBandMessage BeginCommit(transaction, guid)

          x.copy(serviced = true)

        case x => x
      }

      if (xaInfo.foldLeft(true) {(a, b) => a && (b.serviced)} && xaInfo.find(_.receivedFrom.isEmpty).isDefined) {
        doCommit()
      }
    }

    private[this] def doCommit() {
      onCommit.foreach(f => tryo(f()))
      xaConnMgr.foreach(_.actualCommit())
      xaConnMgr = Empty
      postCommit.foreach(f => tryo(f()))
      for {
        tid <- currentTransaction
        worker <- sentList.map(_._2)
      } worker outOfBandMessage CommitTransaction(tid)
      cleanupCurrentTransaction()
    }

    private[this] def doLocalRollback(workerId: WorkerId, msg: String) {
      xaConnMgr = Empty
      onRollback.foreach(f => tryo(f(workerId, msg)))
      xaConnMgr.foreach(_.actualRollback())

      for {
        tid <- currentTransaction
      } {
        for {
          (_, worker) <- sentList
        } worker outOfBandMessage RollbackTransaction(tid, workerId, msg)

        for {
          xa <- xaInfo
          (_, _, worker) <-  xa.receivedFrom
        } worker outOfBandMessage RollbackTransaction(tid, workerId, msg)
      }
      cleanupCurrentTransaction()
    }

    private[this] def cleanupCurrentTransaction() {
      for {
        (_, sc) <- rollbackTimer
      } tryo(sc.cancel(false))

      currentTransaction match {
        case Full(t) => deadTransactions += t -> millis // TODO -- age these if the map grows too big
        case _ =>
      }

      currentTransaction = Empty
      onCommit = Nil
      postCommit = Nil
      onRollback = Nil
      transMap = Map()
      xaInfo = Nil
      sentList = Nil
      transactionSendCount = 0
      messageSendFuncs = Nil
      xaConnMgr = Empty
    }

    override protected def testTranslate(f: Any => Boolean)(v: Any): Boolean = {
      val inXAction = currentTransaction.isDefined
      val sendCnt = transactionSendCount
      object ValidateTID {
        def unapply(in: TransactionId): Option[TransactionId] = {
          if (!inXAction || Full(in) == currentTransaction ||
              deadTransactions.contains(in)) Some(in) else None
        }
      }

      v match {
        case WorkerCreated() => true
        case WorkerUnfrozen() => true
        case WorkerWillFreeze(_) if (!inXAction) => true
        case WorkerWillDestroy() if (!inXAction) => true

        case CommitTransaction(ValidateTID(tid)) => true

        case BeginCommit(ValidateTID(_), _) => true

        case RollbackTransaction(ValidateTID(tid), _, _) => true

        case TransactionalMessage(ValidateTID(tid),_, msg, _, _) =>
          f(msg)

        case MessageWithAction(msg,  _) if (!inXAction) => f(msg)
        case v if (!inXAction) => f(v)
        case _ => false
      }
    }

    override protected def execTranslate(f: Any => Unit)(v: Any) {
      try {
        val inXAction = currentTransaction.isDefined
        val sendCnt = transactionSendCount
        messageSendFuncs = Nil
        messageClass = v.asInstanceOf[Object].getClass.getName

        try {
          v match {

            case WorkerCreated() => tryo(workerCreated())
            case WorkerUnfrozen() => tryo(workerUnfrozen())
            case WorkerWillFreeze(f) => tryo(workerWillFreeze()) ; f()
            case WorkerWillDestroy() => tryo(workerWillDestroy())


            case BeginCommit(tid, guid) =>
              if (Full(tid) == currentTransaction) {
                xaInfo = xaInfo.map {
                  case x if x.sentTo.contains(guid) =>
                    x.copy(sentTo = x.sentTo - guid)
                  case x => x
                }
                checkCommitOrBegin()
              }

            case CommitTransaction(tid) =>
              if (Full(tid) == currentTransaction) doCommit()

            case RollbackTransaction(tid, who, msg) =>
              if (Full(tid) == currentTransaction) doLocalRollback(who, msg)

            case TransactionalMessage(tid, guid, msg, timeout, from) =>
              if (!deadTransactions.contains(tid)) {
                if (!inXAction) {
                  setupConnection()
                  currentTransaction = Full(tid)
                  rollbackTimer = Full((timeout, ActorPing.schedule(
                        () => WorkerImpl.this.outOfBandMessage(RollbackTransaction(tid, id, "Timeout")),
                        (timeout - millis) + 200L))) // timeout 200ms after master
                }

                for {
                  sender <- WorkerMgr.find(from)
                } {
                  xaInfo = XAInfo(receivedFrom = Full( (guid, from, sender) )) :: xaInfo
                }

                f(msg)
              }

            case MessageWithAction(msg, future) =>
              callBackFunc = Full(future)
              try {
                f(msg)
              } finally {
                callBackFunc = Empty
              }
            case v => f(v)
          }
        } catch {
          case ef: RollItBack =>
            if (currentTransaction.isDefined) doLocalRollback(id, ef.msg)


          case e if currentTransaction.isDefined =>
            if (currentTransaction.isDefined) doLocalRollback(id, e.getClass.getName+": "+e.getMessage)
            throw e
        }

        messageSendFuncs.foreach(_())



        if (currentTransaction.isDefined && sendCnt == transactionSendCount) {
          checkCommitOrBegin()
        }

      } finally {
        messageClass = ""
        messageSendFuncs = Nil
      }
    }

    private def supAround[T](f: => T): T = super.around(f)

    protected override def around[T](f: => T) =
      WorkerImpl.currentWorker.doWith(WorkerImpl.this) {
        xaConnMgr match {
          case Full(cm) =>
            DB.doWithConnectionManagers(DefaultConnectionIdentifier -> cm) {
              supAround(f)
            }

          case _ => conMgrFunc match {
              case null => supAround(f)
              case cmf =>
                DB.doWithConnectionManagers(DefaultConnectionIdentifier -> cmf(id)) {
                  supAround(f)
                }
            }
        }
      }
  }

  /**
   * Send a message to the worker
   */
  def !(msg: IdType#MsgType): Unit = myActor ! msg

  protected[this] def on[NIdType <: WorkerId](worker: Worker[NIdType]): PartialAskBuilder[NIdType] = {
    val send = new AskInvoker[NIdType] {
      def apply[T <: QBase, MT <: NIdType#MsgType with MsgWithResponse[T]](success: Box[T] => Unit, ab: AskBuilderImpl[T, NIdType, MT])  {
        val ret = new Function1[Box[T], Unit] {
          var fired = false
          var turnMeOff: Box[ScheduledFuture[Unit]] = Empty
          def apply(msg: Box[T]): Unit = synchronized {
            if (!fired) {
              fired = true
              turnMeOff.foreach(a => tryo(a.cancel(false)))
              myActor ! ExecFunc(() => tryo(success(msg)))
            }
          }
        }

        ret.turnMeOff = ab.timeout.map(d => ActorPing.schedule(() => {ret.synchronized{if (!ret.fired) {
                  ret.fired = true
                  myActor ! ExecFunc(() => tryo(success(ParamFailure("timeout", Empty, Empty, TimeExpired(d)))))
                }}}, d))

        worker sendMessageWithAction MessageWithAction[NIdType, T, MT](ab.msg, ret)
      }
    }

    PartialAskBuilderImpl[NIdType](send, worker, Full(10 seconds))
  }

  /**
   * Send a message to along with a callback
   */
  private[stambecco] def sendMessageWithAction[RetType <: QBase,
                                         MsgType <: IdType#MsgType with
                                         MsgWithResponse[RetType]](msgWithAction: MessageWithAction[IdType, RetType, MsgType]): Unit = {
    myActor ! msgWithAction
  }


  private[stambecco] def outOfBandMessage(oobm: OutOfBandMessage) {
    myActor ! oobm
  }

  /**
   * Start information related to the transaction
   */
  protected[this] def transaction: TransactionBuilder1 = testThatWereInActor(TB1Impl(Nil, Nil, Nil))


  private class RollItBack(val msg: String) extends Throwable

  private def testThatWereInActor[T](f: => T): T = f
  //if (WorkerImpl.currentWorker eq WorkerImpl.this) f
  //else throw new Exception("FIXME ")



  private[stambecco] def buildReplyFunc(): Box[QBase] => Unit = {
    val curCB = callBackFunc
    (v: Box[QBase]) => curCB match {
      case Full(cb) => try {
          cb(v)
        }  catch {
          case e: Exception => e.printStackTrace
        }
      case _ =>
    }
  }

  private[stambecco] def _reply(x: Box[QBase]): Unit = {
    callBackFunc match {
      case Full(cb) => try {
          cb(x)
        }  catch {
          case e: Exception => e.printStackTrace
        }
        callBackFunc = Empty

      case _ =>
    }
  }

  protected def workerCreated(): Unit = {}
  protected def workerUnfrozen(): Unit = {}
  protected def workerWillFreeze(): Unit = {}
  protected def workerWillDestroy(): Unit = {}
}
