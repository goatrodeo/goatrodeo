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

import net.liftweb._
import mapper._
import common._

/**
 * Define an Id for the User class of worker
 */
final case class UserId(id: Long) extends WorkerId {
  type MyType = UserId // what is my type
  type MsgType = UserMsg // what type of messages does the User Worker accept
  type IdType = Long // what is the type of the User id

  /**
   * Generate a manifest for the UserId type
   */
  def myType: Manifest[MyType] = manifest[UserId]

  /**
   * Based on the UserId, calculate a unique name for a file
   */
  def uniqueNameForFile: String = "user_info_"+id
}

/**
 * All messages sent to a User worker must implement this trait
 */
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


/**
 * The user has posted an update.  Reflect that in the users
 * updates and propgrate the update to followers
 */
final case class PostUpdate(updateId: Long, time: Long) extends UserMsg

/**
 * Sent from the updating User to all its followers
 */
final case class AddUpdateToTimeline(updateId: Long, 
				     from: UserId,
				     time: Long) extends UserMsg

final case class ListOfUsers(users: List[UserId]) extends QBase

final case class GetFollowers() extends UserMsg with 
MsgWithResponse[ListOfUsers]

final case class GetFriends() extends UserMsg with 
MsgWithResponse[ListOfUsers]

final case class MessageInfo(updateId: Long) extends QBase
final case class ListOfMessages(msgs: List[MessageInfo]) extends QBase

final case class GetTimeline(first: Int, count: Int) extends UserMsg with
MsgWithResponse[ListOfMessages]

final case class GetUpdates(first: Int, count: Int) extends UserMsg with
MsgWithResponse[ListOfMessages]

/**
 * A UserWorker models a user in Skitter.  The UserWorker needs to
 * know its UserId and needs to be able to calculate a JDBC connection
 * vendor.
 */
class UserWorker(id: UserId,
                 calcFunc: UserId => ConnectionManager) extends
WorkerImpl[UserId, UserMsg](id, calcFunc) {
  // cache the user's timeline
  private var timeline: Array[Long] = Array()

  // cache the user's most recent updates
  private var updates: Array[Long] = Array()

  /**
   * When the worker is created, populate the correct tables into
   * the UserWorker instance-private relational database
   */
  override protected[this] def workerCreated() {
    super.workerCreated()
    
    def nullLogFunc(a: => AnyRef) {}

    Schemifier.schemify(true, nullLogFunc _, Relationship, Message)
  }

  /**
   * When the UserWorker is unfrozen (materialized into an address
   * space), read the data to be cached
   */
  override protected[this] def workerUnfrozen() {
    timeline = Message.findAll(OrderBy(Message.postTime, Descending),
			       MaxRows(20)).map(_.messageId.is).toArray

    updates = Message.findAll(By(Message.postedBy, id.id),
			      OrderBy(Message.postTime, Descending),
			      MaxRows(20)).map(_.messageId.is).toArray
  }

  /**
   * handle the Follow message
   */
  def doFollow(msg: Follow) {
    // if we're not already following, create a row in our local
    // store indicating that we're following
    if (Relationship.find(By(Relationship.who, msg.who.id),
                          By(Relationship.isFollowing, true)).isEmpty) {
      Relationship.create.who(msg.who.id).isFollowing(true).save
    }

    // tell the other guy that we're following
    for (other <- WorkerMgr.find(msg.who)) other ! AddMeAsAFollower(id)
  }

  def doAddAFollower(msg: AddMeAsAFollower) {
    if (Relationship.find(By(Relationship.who, msg.from.id),
                          By(Relationship.isFollowing, false)).isEmpty) {
      Relationship.create.who(msg.from.id).isFollowing(false).save
    }
  }

  def doUnfollow(msg: Unfollow) {
    Relationship.findAll(By(Relationship.who, msg.who.id),
			 By(Relationship.isFollowing, true)).foreach(_.delete_!)

    for (other <- WorkerMgr.find(msg.who)) other ! RemoveMeAsAFollower(id)
  }

  def doRemoveAFollower(msg: RemoveMeAsAFollower) {
    Relationship.findAll(By(Relationship.who, msg.from.id),
			 By(Relationship.isFollowing, false)).foreach(_.delete_!)
  }

  def doUpdate(msg: PostUpdate) {
    updates = (msg.updateId :: updates.toList).take(20).toArray
    
    val toSend = AddUpdateToTimeline(msg.updateId, id, msg.time)

    this ! toSend

    Relationship.findMap(By(Relationship.isFollowing, true)) {
      row =>
      for (other <- WorkerMgr.find(UserId(row.who))) other ! toSend

      Empty
    }
  }

  def doAddUpdate(msg: AddUpdateToTimeline) {
    timeline = (msg.updateId :: timeline.toList).toArray

    Message.create.messageId(msg.updateId).
    postTime(msg.time).postedBy(msg.from.id).save
  }

  def doGetFollowers(msg: GetFollowers): ListOfUsers =
    getRelated(false)

  private def getRelated(isFollowing: Boolean): ListOfUsers =
    ListOfUsers(
      Relationship.findMap(By(Relationship.isFollowing, isFollowing)) {
	row => Full(UserId(row.who))
      })

  def doGetFriends(msg: GetFriends): ListOfUsers =
    getRelated(true)

  private def getMessages(mine: Boolean,
                          first: Int, count: Int): ListOfMessages = {
    val set = if (mine) updates else timeline

    if (first == 0 && count <= set.length)
      ListOfMessages(set.toList.take(count).map(MessageInfo.apply))
    else {
      val qp = List[QueryParam[Message]](OrderBy(Message.postTime, Descending),
                                         MaxRows(count),
                                         StartAt(first))

      ListOfMessages(Message.findMap(
          (if (mine) List(By(Message.postedBy, id.id)) else Nil) ::: qp :_*) {
	  row =>
          Full(MessageInfo(row.messageId))
	})
    }
  }

  def doGetTimeline(msg: GetTimeline): ListOfMessages = 
    getMessages(false, msg.first, msg.count)

  def doGetUpdates(msg: GetUpdates, answer: Box[ListOfMessages] => Unit) =
    answer(Full(getMessages(true, msg.first, msg.count)))
}

class Relationship extends LongKeyedMapper[Relationship] with IdPK {
  def getSingleton = Relationship

  object who extends MappedLong(this) {
    override def dbIndexed_? = true
  }

  object isFollowing extends MappedBoolean(this)
}

object Relationship extends Relationship with 
LongKeyedMetaMapper[Relationship]

class Message extends LongKeyedMapper[Message] with IdPK {
  def getSingleton = Message

  object messageId extends MappedLong(this)
  object postTime extends MappedLong(this)
  object postedBy extends MappedLong(this)
}

object Message extends Message with LongKeyedMetaMapper[Message] {
  private def createFunc(table: String,cols: List[String]): String =
    "CREATE INDEX msg_time_desc ON "+table+" ("+cols.head+" DESC)"

  override def dbIndexes = GenericIndex(createFunc _, 
					IHaveValidatedThisSQL("dpp",
							      "2010/02/16"),
					postTime) :: super.dbIndexes
}
