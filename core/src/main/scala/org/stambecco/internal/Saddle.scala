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
package internal

import java.lang.reflect.{ Method, Modifier, ParameterizedType, Type => JType }
import java.util.concurrent.{ ConcurrentHashMap => CHash }
import net.liftweb.actor._
import net.liftweb.common._
import net.liftweb.util.ThreadGlobal
import net.liftweb.util.Helpers._
import scala.collection.mutable.ListBuffer

private[stambecco] final case class MessageWithAction[IdType <: WorkerId,
                                           RespType <: QBase,
                                           MT <: IdType#MsgType with
                                           MsgWithResponse[RespType]]
(msg: MT, action: Box[RespType] => Unit)

private[stambecco] final case class ExecFunc(f: () => Unit)

/**
 * Part of the DSL to build Ask/Answer
 */
private[stambecco] sealed trait PartialAskBuilder[IdType <: WorkerId] {
  /**
   * Set the timeout for the question
   */
  def withTimeout(timespan: TimeSpan): PartialAskBuilder[IdType]

  /**
   * The message to ask the Worker
   */
  def ask[T <: QBase](msg: IdType#MsgType with MsgWithResponse[T]): AskBuilder[T, IdType, IdType#MsgType with MsgWithResponse[T]]
}

/**
 * The second part of the DSL to build the Ask/Answer
 */
private[stambecco] sealed trait AskBuilder[T <: QBase, IdType <: WorkerId, MT <: MsgWithResponse[T] with IdType#MsgType] {
  /**
   * Set the timeout for the question
   */
  def withTimeout(timespan: TimeSpan): AskBuilder[T, IdType, MT]

  /**
   * The function to execute on completion of the question.
   */
  def complete(f: Box[T] => Unit): Unit

  /**
   * Alternatively, you can ask for a Future.  The future is a Responder
   * that you can use in a for comprehension.
   */
  def future: Responder[Box[T]] with ValueExtractor[Box[T]]

  /**
   * Return a continuation that allows you to execute code when the
   * answer becomes available, without blocking the current thread.
   */
  def continue: Responder[Box[T]]
}

private[stambecco] trait AskInvoker[IdType <: WorkerId] {
  def apply[T <: QBase, MT <: IdType#MsgType with MsgWithResponse[T]](complete: Box[T] => Unit, builder: AskBuilderImpl[T, IdType, MT])
}


private[stambecco] case class PartialAskBuilderImpl[IdType <: WorkerId](askFunc: AskInvoker[IdType],
                                                             what: Worker[IdType], timeout: Box[TimeSpan]) extends
PartialAskBuilder[IdType] {
  def withTimeout(timespan: TimeSpan): PartialAskBuilder[IdType] = copy(timeout = Full(timespan))
  def ask[T <: QBase](msg: IdType#MsgType with MsgWithResponse[T]): AskBuilder[T, IdType, IdType#MsgType with MsgWithResponse[T]] =
    AskBuilderImpl[T, IdType, IdType#MsgType with MsgWithResponse[T]](askFunc, what, timeout, msg)

}

private[stambecco] case class AskBuilderImpl[T <: QBase,
                                  IdType <: WorkerId,
                                  MT <: IdType#MsgType with MsgWithResponse[T]](askFunc:  AskInvoker[IdType],
                                                                                what: Worker[IdType],
                                                                                timeout: Box[TimeSpan],
                                                                                msg: MT) extends
AskBuilder[T, IdType, MT] {
  def withTimeout(timespan: TimeSpan): AskBuilder[T, IdType, MT] = copy(timeout = Full(timespan))
  def complete(f: Box[T] => Unit): Unit = askFunc(f, this)
  def future: Responder[Box[T]] with ValueExtractor[Box[T]] = {
    var done = false
    val lockObj = new Object
    var resp: Box[T] = Empty

    askFunc((f: Box[T]) => lockObj.synchronized{resp = f; done = true; lockObj.notifyAll()}, this)

    new Responder[Box[T]] with ValueExtractor[Box[T]] {
      def respond(f: Box[T] => Unit) = {
        val b = lockObj.synchronized {
          while (!done) lockObj.wait(100)
          resp
        }

        f(b)
      }
    }
  }

  /**
   * Return a continuation that allows you to execute code when the
   * answer becomes available, without blocking the current thread.
   */
  def continue: Responder[Box[T]] = {
    var done = false
    var funcList: List[() => Unit] = Nil
    val lockObj = new Object
    var resp: Box[T] = Empty

    askFunc((f: Box[T]) => {
        val lst = lockObj.synchronized{if (!done) {resp = f; done = true; funcList} else Nil}
        lst.foreach(f => LAScheduler.execute(f))
      } , this)

    new Responder[Box[T]] {
      def respond(f: Box[T] => Unit) = {
        val b: () => Unit = lockObj.synchronized {
          val tf = () => f(resp)
          if (done) tf
          else {
            funcList ::= tf
            () => {}
          }
        }

        b.apply()
      }
    }
  }
}

/*
 * Transaction: target Worker, Message, on Commit, on Rollback, timeout
 * transaction
 */
private[stambecco] object WorkerImpl {
  val currentWorker = new ThreadGlobal[WorkerImpl[_, _]]

  type IdSomeType = T forSome {type T <: WorkerId}

  private val dispatch: CHash[(Class[_], String),
                              Map[Class[_],
                                  (WorkerImpl[_, _],
                                   _) => Unit]] = new CHash

  private class FuncHandler[IdType <: WorkerId,
                            MsgType <: IdType#MsgType](
    inst: WorkerImpl[IdType, MsgType],
    funcs: Map[Class[_],
               (WorkerImpl[IdType, MsgType], MsgType) => Unit ]) extends
  PartialFunction[MsgType, Unit] {
    def isDefinedAt(msg: MsgType) = funcs.isDefinedAt(msg.getClass)
    def apply(msg: MsgType) = funcs(msg.getClass).apply(inst, msg)
  }

  private def correntFunc(param2: JType, retType: JType): Boolean = {
    def testIs(pt: ParameterizedType, clz: Class[_]): Boolean = pt.getRawType match {
      case c: Class[_] => clz.isAssignableFrom(c)
      case _ => false
    }
    param2 match {
      case pt: ParameterizedType if testIs(pt, classOf[Function1[_, _]]) =>
        pt.getActualTypeArguments.toList match {
          case (pt: ParameterizedType) :: _ if testIs(pt, classOf[Box[_]]) =>
            pt.getActualTypeArguments.head == retType
          
          case _ => false
        }
      case _ =>
        false
    }
  }

  private def findResponseType(c: JType): Box[JType] =
    c match {
      case null => Empty
      case pt: ParameterizedType if pt.getRawType == classOf[MsgWithResponse[_]] =>
        Full(pt.getActualTypeArguments.head)
      case c: Class[_] =>
        Box(c.getGenericInterfaces.flatMap(findResponseType).headOption) or findResponseType(c.getGenericSuperclass)
      case _ => Empty
    }

  private def getAllMethods(cls: Class[_])(filter: Method => Boolean): List[Method] = {
    val ret: ListBuffer[Method] = new ListBuffer()

    def doAClass(localCls: Class[_]) {
      localCls match {
        case null => ()
        case clz => for {
            m <- clz.getDeclaredMethods() if filter(m)
          } {
            ret += m
          }
          doAClass(clz.getSuperclass())
      }
    }

    doAClass(cls)

    ret.toList
  }

  def calculateDispatchMap[IdType <: WorkerId, MsgType <: IdType#MsgType](inst: WorkerImpl[IdType, MsgType]):
  Map[Class[_], Function2[WorkerImpl[IdType, MsgType], MsgType, Unit]] = {

    val methods = getAllMethods(inst.getClass){m =>
      (m.getName.startsWith("handle") ||
       m.getName.startsWith("do") ||
       m.getName.startsWith("perform")) &&
      ((m.getModifiers & Modifier.PUBLIC) != 0) &&
      (m.getGenericParameterTypes.length == 1 ||
       m.getGenericParameterTypes.length == 2)  &&
      inst.msgManifest.erasure.isAssignableFrom(m.getParameterTypes.head )
    }

    def calcFunc(method: Method): Function2[WorkerImpl[IdType, MsgType], MsgType, Unit] = {
      val gpt = method.getGenericParameterTypes()

      findResponseType(gpt.head) match {
        case Full(clz) if gpt.length == 2 && correntFunc(gpt(1), clz) =>
          (inst: WorkerImpl[IdType, MsgType], param: MsgType) => {
            val ret = try {
              method.invoke(inst, param.asInstanceOf[Object], inst.buildReplyFunc().asInstanceOf[Object])
            } catch {
              case ite: java.lang.reflect.InvocationTargetException =>
                throw ite.getCause
            }
            ()
          }

        case Full(clz) if clz == method.getGenericReturnType =>
          (inst: WorkerImpl[IdType, MsgType], param: MsgType) => {
            val ret = try {
              method.invoke(inst, param.asInstanceOf[Object])
            } catch {
              case ite: java.lang.reflect.InvocationTargetException =>
                throw ite.getCause
            }
            inst._reply(Full(ret.asInstanceOf[QBase]))
            ()
          }

        case _ =>
          (inst: WorkerImpl[IdType, MsgType], param: MsgType) => {
            try {
              method.invoke(inst, param.asInstanceOf[Object])
            } catch {
              case ite: java.lang.reflect.InvocationTargetException =>
                throw ite.getCause
            }
            ()
          }
      }
    }

    val all = methods.map(method =>
      method.getParameterTypes().head -> calcFunc(method))
      

    val ret: Map[Class[_], (WorkerImpl[IdType, MsgType], MsgType) => Unit] =
      Map(all :_*)

    ret
  }

  def calculateDispatch[IdType <: WorkerId, MsgType <: IdType#MsgType](inst: WorkerImpl[IdType, MsgType]): PartialFunction[MsgType, Unit] = {
    val key: (Class[_], String) = (inst.getClass, inst.idManifest.toString)

    val map = dispatch.get(key) match {
      case null =>
        synchronized {
          dispatch.get(key) match {
            case null =>
              val map  = calculateDispatchMap(inst).asInstanceOf[Map[Class[_], Function2[WorkerImpl[_, _], _, Unit]] ]
              dispatch.put(key, map)

              map

            case map => map
          }
        }
      case map => map
    }
    
    new FuncHandler(inst, map.asInstanceOf[Map[Class[_], (WorkerImpl[IdType, MsgType], MsgType) => Unit]])
  }
}

private[stambecco] trait WorkMaster[IdType <: WorkerId] {
  def idType: Manifest[IdType]
  def find(key: IdType): Box[Worker[IdType]]

  /**
   * This method is called when the work master is loaded into the Workspace
   */
  def loadMaster() {

  }

  /**
   * This method is called when the work master is unloaded from the Workspace
   */
  def unloadMaster() {

  }
}

private[stambecco] trait ValueExtractor[T] {
  self: Responder[T] =>

  def value: T = {
    var ret: T = null.asInstanceOf[T]
    this.foreach(v => ret = v)
    ret
  }
}
