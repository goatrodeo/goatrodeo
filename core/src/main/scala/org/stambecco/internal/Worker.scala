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

import net.liftweb.common.SimpleActor
import net.liftweb.util.Helpers._

private[stambecco] final case class TimeExpired(lenght: Long) extends QBase

private[stambecco] final case class TransactionId(id: String) extends QBase
private[stambecco] final case class TransactionNodeGUID(id: String) extends QBase

private[stambecco] object TransactionNodeGUID {
  def generate = new TransactionNodeGUID(randomString(20))
}

private[stambecco] sealed trait OutOfBandMessage

private[stambecco] final case class WorkerCreated() extends OutOfBandMessage
private[stambecco] final case class WorkerUnfrozen() extends OutOfBandMessage
private[stambecco] final case class WorkerWillFreeze(done: () => Unit) extends OutOfBandMessage
private[stambecco] final case class WorkerWillDestroy() extends OutOfBandMessage

private[stambecco] final case class CommitTransaction(id: TransactionId) extends OutOfBandMessage
private[stambecco] final case class BeginCommit(id: TransactionId, guid: TransactionNodeGUID) extends OutOfBandMessage
private[stambecco] final case class RollbackTransaction(id: TransactionId, who: WorkerId, msg: String) extends OutOfBandMessage
private[stambecco] final case class TransactionalMessage[IdType <: WorkerId,
                                              MsgType <: IdType#MsgType,
                                              SenderIdType <: WorkerId]
(transactionId: TransactionId,
 guid: TransactionNodeGUID,
 message: MsgType,
 timeout: Long,
 from: SenderIdType) extends OutOfBandMessage

private[stambecco] object TransactionId {
  def generate = new TransactionId(randomString(50))
}

/**
 * The interface to a Worker.  A Worker receives messages, performs computations and communicates with
 * other workers
 */
private[stambecco] trait Worker[IdType <: WorkerId] extends SimpleActor[IdType#MsgType] {
  /**
   * The unique ID of the Work
   */
  def id: IdType

  private type MsgType = IdType#MsgType

  /**
   * Send a message to the worker
   */
  def !(msg: MsgType): Unit


  private[stambecco] def sendMessageWithAction[RetType <: QBase,
                                         MsgType <: IdType#MsgType with
                                         MsgWithResponse[RetType]]
  (msgWithAction: MessageWithAction[IdType, RetType, MsgType]): Unit

  private[stambecco] def outOfBandMessage(oobm: OutOfBandMessage)
}
