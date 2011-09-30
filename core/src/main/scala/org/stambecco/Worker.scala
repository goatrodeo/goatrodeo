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

/**
 * TODO
 * @api
 */
trait WorkerId extends QBase {
  type MyType <: WorkerId
  type MsgType <: QBase
  type IdType

  def myType: Manifest[MyType]

  def id: IdType

  def uniqueNameForFile: String
}

/**
 * A message that generates a response with a particular type
 * @api
 */
trait MsgWithResponse[T <: QBase] extends QBase
