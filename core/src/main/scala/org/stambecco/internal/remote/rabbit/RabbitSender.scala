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
package remote
package rabbit

import com.rabbitmq.client._
import java.io.ByteArrayOutputStream
import java.io.ObjectOutputStream

import net.liftweb._
import json._
import JsonAST._
import Printer._
import net.liftweb.util._

private[stambecco] final case class RoutedMessage(id: WorkerId, msg: QBase) extends QBase

private[stambecco] abstract class RabbitSender extends MessageSender {
  /**
   * Send a message to the named server, destined for the given Worker
   */
  def send[IdType <: WorkerId](server: String, target: IdType, message: QBase) {
    val ba: Array[Byte] = compact(render(Extraction.decompose(RoutedMessage(target, message)))).getBytes("UTF-8")
    channel.basicPublish("stambecco", "route", null, ba)
  }

    implicit val formats = Serialization.formats(new TypeHints {
    val hints: List[Class[_]] = Nil

    /** Return hint for given type.
     */
    def hintFor(clazz: Class[_]): String = clazz.getName

    /** Return type for given hint.
     */
    def classFor(hint: String): Option[Class[_]] = Some(Thread.currentThread.getContextClassLoader.loadClass(hint))

    override def containsHint_?(clazz: Class[_]) = true
    override val deserialize: PartialFunction[(String, JObject), Any] = Map()
    override val serialize: PartialFunction[Any, JObject] = Map()
  })

   

  private lazy val factory = {
     val params = new ConnectionParameters
  // All of the params, exchanges, and queues are all just example data.
  params.setUsername(Props.get("rabbit.user") openOr "guest")
  params.setPassword(Props.get("rabbit.password") openOr "guest")
  params.setVirtualHost(Props.get("rabbit.virtual_host") openOr "/")
  params.setRequestedHeartbeat(0)
  new ConnectionFactory(params)
  }

  private lazy val conn = factory.newConnection(Props.get("rabbit.host") openOr "localhost",
                                                Props.getInt("rabbit.port") openOr 5672)
  private lazy val channel = conn.createChannel()
}
