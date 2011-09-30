/*
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

package com.skittr
package lib

import net.liftweb._
import json._
import util._
import http._
import common._
import model._
import JsonAST._
import Helpers._
import org.stambecco._

object RestDispatch {
  type DPF = LiftRules.DispatchPF

  def init() {
    LiftRules.statelessDispatchTable.append(
      createUser orElse getUser orElse 
      updateUser orElse getTimeline orElse
      getUpdates orElse putMessage orElse
      follow orElse followers orElse following
    )
  }

  private def filterId(json: JObject): JObject =
    JObject(json.obj.filter{
        case JsonAST.JField("id", _) => false
        case _ => true})

  def createUser: DPF = {
    case r @ Req("user" :: Nil, _, PostRequest) =>
      for {
        json <- r.json.asA[JObject] ?~ "Invalid JSON" ~> 400
        testUser = User.decodeFromJSON(filterId(json), true)
        user <- testUser.asValid ~> 400
      } yield  User.encodeAsJSON(user.saveMe)
  }

  def updateUser: DPF = {
    case r @ Req("user" :: User(user) :: Nil, _, PutRequest) =>
      for {
        json <- r.json.asA[JObject] ?~ "Invalid JSON" ~> 400
        updated <- User.updateFromJSON(user, filterId(json)).asValid ~> 400
      } yield User.encodeAsJSON(user.saveMe)
  }

  def getUser: DPF = {
    case Req("user" :: User(user) :: Nil, _, GetRequest) => User.encodeAsJSON(user)
  }

  def getTimeline: DPF = {
    case r @ Req("timeline" :: AsLong(user) :: Nil, _, GetRequest) =>
      for {
        user <- WorkerMgr.find(UserId(user)) ?~ "Worker not found"
        first = r.param("first").asInt openOr 0
        count = r.param("count").asInt openOr 20
        it <- (OutsideOfWorker on user ask GetTimeline(first,count) future).value
      } yield it.asJson
  }


  def getUpdates: DPF = {
    case r @ Req("updates" :: AsLong(user) :: Nil, _, GetRequest) =>
      for {
        user <- WorkerMgr.find(UserId(user))
        first = r.param("first").asInt openOr 0
        count = r.param("count").asInt openOr 20
        it <- (OutsideOfWorker on user ask GetUpdates(first,count) future).value
      } yield it.asJson
  }

  def putMessage: DPF = {
    case r @ Req("message" :: AsLong(user) :: Nil, _, PutRequest) =>
      for {
        user <- WorkerMgr.find(UserId(user)) ?~ "User not found"
        json <- r.json
        JField("messageId", JInt(messageId)) <- json
        JField("timestamp", JInt(timemillis)) <- json
      } yield {
        user ! PostUpdate(messageId.longValue,
                          timemillis.longValue)
        JBool(true)
      }
  }

  def follow: DPF = {
    case r @ Req("follow" :: AsLong(user) :: Nil, _, PutRequest) =>
      for {
        user <- WorkerMgr.find(UserId(user))
        json <- r.json
        JField("who", JInt(num)) <- json
      } yield {
        user ! Follow(UserId(num.longValue))
        JBool(true)
      }
  }

  def following: DPF = {
    case Req("follow" :: AsLong(user) :: Nil, _, GetRequest) =>
      for {
        user <- WorkerMgr.find(UserId(user)) ?~ "User not found"
        it <- (OutsideOfWorker on user ask GetFriends() future).value
      } yield it.asJson
  }

  def followers: DPF = {
    case Req("followers" :: AsLong(user) :: Nil, _, GetRequest) =>
      for {
        user <- WorkerMgr.find(UserId(user)) ?~ "User not found"
        it <- (OutsideOfWorker on user ask GetFollowers() future).value
      } yield it.asJson
  }

  def unfollow: DPF = {
    case r @ Req("unfollow" :: AsLong(user) :: Nil, _, PutRequest) =>
      for {
        user <- WorkerMgr.find(UserId(user)) ?~ "User not found"
        json <- r.json
        (_, whoJson) <- (json \ classOf[JField]).find(_._1 == "who")
        who <- Box.asA[JInt](whoJson)
      } yield {
        user ! Unfollow(UserId(who.num.longValue))
        JBool(true)
      }
  }

  object AsLong {
    def unapply(in: Any): Option[Long] = Helpers.asLong(in)
  }

  implicit def iterableToBox[X](in: Iterable[X]): Box[X] = in.headOption

  implicit def responseFuncBuilder[T](value: T)(implicit cvt: T => LiftResponse): () => Box[LiftResponse] =
    () => handleFailure(Full(value))(cvt)

  implicit def responseFuncBuilder[T](value: Box[T])(implicit cvt: T => LiftResponse): () => Box[LiftResponse] =
    () => handleFailure(value)(cvt)

  implicit def handleFailure[T](value: Box[T])(implicit cvt: T => LiftResponse): Box[LiftResponse] = {
    value match {
      case ParamFailure(msg, _, _, code: Int) =>
        Full(InMemoryResponse(msg.getBytes("UTF-8"), ("Content-Type" -> "text/plain; charset=utf-8") :: Nil, Nil, code))

      case Failure(msg, _, _) => Full(NotFoundResponse(msg))

      case Empty => Empty

      case Full(x) => Full(cvt(x))
    }
  }

  class SuperBox(b: Box[String]) {
    def asInt: Box[Int] = b.flatMap(Helpers.asInt)
  }

  implicit def promoteToSuperBox(in: Box[String]): SuperBox = new SuperBox(in)

  class Jsonify[T <: QBase](a: T) {
    def asJson: JValue = Extraction.decompose(a)(DefaultFormats)
  }

  implicit def promoteToJsonify[T <: QBase](a: T): Jsonify[T] = new Jsonify(a)

  implicit def JsonToResponse(in: JsonAST.JValue): LiftResponse =
    JsonResponse(in)

}
