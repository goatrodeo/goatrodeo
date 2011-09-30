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
package model

import net.liftweb._
import http._
import mapper._
import json._
import common._

class User extends LongKeyedMapper[User] with IdPK {
  def getSingleton = User

  object name extends MappedString(this, 256)
  object created extends MappedBoolean(this) {override def defaultValue = false}
}

object User extends User with LongKeyedMetaMapper[User] {
  def encodeAsJSON(in: User) = encodeAsJSON_!(in)
  def decodeFromJSON(json: JsonAST.JObject,
                     markFieldsAsDirty: Boolean) =
                       decodeFromJSON_!(json, markFieldsAsDirty)

  def updateFromJSON(user: User, json: JsonAST.JObject): User =
    updateFromJSON_!(user, json)
}
