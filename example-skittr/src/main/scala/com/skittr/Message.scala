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

import org.stambecco._

/*
final case class MessageId(id: Long) extends WorkerId {
  type MyType = MessageId
  type MsgType = MessageMsg
  type IdType = Long

  def myType: Manifest[MyType] = manifest[MessageId]

  def uniqueNameForFile: String = "message_store_"+id
}

sealed trait UserMsg extends QBase

/**
 * Sent to a User telling the user to follow another user
 */
final case class Follow(who: UserId) extends UserMsg

/**
 * Sent from one user to another to inform the recipient that
 * you want to follow them
 */
final case class AddMeAsAFollower(from: UserId) extends UserMsg

/**
 * Sent to a User telling the user to unfollow another user
 */
final case class Unfollow(who: UserId) extends UserMsg

/**
 * Sent from one user to another to inform the recipient that
 * you want to un follow them
 */
final case class RemoveMeAsAFollower(from: UserId) extends UserMsg
*/
