/*
 * Rodeo Lib
 *
 * Copyright 2009-2010 WorldWide Conferencing, LLC
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

import net.liftweb.common.Box
import net.liftweb.json.{ Extraction, JsonParser, Serialization, TypeHints }
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._
import net.liftweb.util.Helpers._

/**
 * TODO
 * @api
 */
object QBase {

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

  implicit def sToQ(in: String) = QString(in)
  implicit def bToQ(in: Boolean) = QBoolean(in)
  implicit def lToQ(in: Long) = QLong(in)
  implicit def iToQ(in: Int) = QInt(in)

  implicit def toByteArray(in: QBase): Array[Byte] = in.serialize.getBytes("UTF-8")
  implicit def fromString(in: String): Box[QBase] =  tryo{Extraction.extract[QBase](JsonParser.parse(in))}
  implicit def fromByteArray(in: Array[Byte]): Box[QBase] = fromString(new String(in, "UTF-8"))
}

/**
 * TODO
 * @api
 */
object QBaseT {
  implicit def unbox[T](in: QBaseT[T]): T = in.is
}

/**
 * TODO
 * @api
 */
@serializable
trait QBase {

  def serialize: String = compact(render(asJson))

  def asJson: JValue = Extraction.decompose(this)(QBase.formats)
}

/**
 * TODO
 * @api
 */
trait QBaseT[T] extends QBase {
  def is: T
}

/**
 * TODO
 * @api
 */
final case class QString(is: String) extends QBaseT[String]

/**
 * TODO
 * @api
 */
final case class QInt(is: Int) extends QBaseT[Int]

/**
 * TODO
 * @api
 */
final case class QLong(is: Long) extends QBaseT[Long]

/**
 * TODO
 * @api
 */
final case class QBoolean(is: Boolean) extends QBaseT[Boolean]

/**
 * TODO
 * @api
 */
final case class QByte(is: Byte) extends QBaseT[Byte]

/**
 * TODO
 * @api
 */
final case class QChar(is: Char) extends QBaseT[Char]

/**
 * TODO
 * @api
 */
final case class QFloat(is: Float) extends QBaseT[Float]

/**
 * TODO
 * @api
 */
final case class QDouble(is: Double) extends QBaseT[Double]
