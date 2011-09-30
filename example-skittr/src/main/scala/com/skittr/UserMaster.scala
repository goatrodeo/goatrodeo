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

import com.skittr.model._

object UserMaster extends LocalWorkMaster[UserId] {

  def instantiate(id: UserId) = new UserWorker(id, connectionManagerForId)

  override def created(id: UserId) {
    super.created(id)
    User.findAll(By(User.id, id.id)).foreach(_.created(true).save)
  }

    def canUnfreeze(id: UserId) = User.find(By(User.id, id.id),
                                            By(User.created, true)).isDefined

  def canCreate(id: UserId) = User.find(By(User.id, id.id)).isDefined

  def numberInMemory = 500

  def idType = manifest[UserId]
}

/*
class UserTable extends LongKeyedMapper[UserTable] with IdPK {
  def getSingleton = UserTable
}

object UserTable extends UserTable with LongKeyedMetaMapper[UserTable]
*/
