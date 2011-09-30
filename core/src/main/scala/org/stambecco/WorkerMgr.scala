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

import internal._

import java.util.concurrent.{ConcurrentHashMap => CHash}
import net.liftweb.common.Box

/**
 * TODO
 * @api
 */
object WorkerMgr {
  private val workMasters: CHash[String, WorkMaster[_]] = new CHash()

  def find[IdType <: WorkerId](id: IdType): Box[Worker[IdType]] = {
    val man = id.myType
    for {
      master <- Box !! workMasters.get(man.toString).asInstanceOf[WorkMaster[IdType]]
      worker <- master.find(id)
    } yield worker
  }

  def registerWorkMaster[IdType <: WorkerId](master: WorkMaster[IdType]) {
    workMasters.put(master.idType.toString, master)
  }
}
