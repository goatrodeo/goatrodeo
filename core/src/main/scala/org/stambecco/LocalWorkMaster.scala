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

import java.sql.{ Connection, DriverManager }
import net.liftweb.common.{ Box, Empty, Full, LRUMap }
import net.liftweb.mapper.{ ConnectionIdentifier, ConnectionManager, DefaultConnectionIdentifier }
import net.liftweb.util.Props
import net.liftweb.util.Helpers._
import scala.collection.mutable.HashMap

/**
 * TODO
 * @api
 */
trait LocalWorkMaster[IdType <: WorkerId] extends WorkMaster[IdType] {
  /**
   * The internal type of the worker.  May have more methods, etc.
   */
  protected[this] type WorkerType = WorkerImpl[IdType, IdType#MsgType]
  private val instances: LRUMap[IdType, WorkerType] = new LRUMap(numberInMemory, Empty, outOfCache _) {
    /**
     * Override this method to implement a test to see if a particular
     * element can be expired from the cache
     */
    override protected def canExpire(k: IdType, v: WorkerType): Boolean = {
      !v.inTransaction_?
    }
  }

  private val connMgr: HashMap[IdType, Connection] = new HashMap()

  protected[this] def outOfCache(key: IdType, value: WorkerType) {
    value.outOfBandMessage(WorkerWillFreeze(
        () => LocalWorkMaster.this.synchronized {
          tryo {
            connMgr(key).close()
            connMgr.remove(key)
          }
        }
      ))
   
  }

  // protected def msgType: Class[IdType#MsgType]

  def find(key: IdType): Box[Worker[IdType]] = synchronized {
    instances.get(key) or unfreeze(key).map{w => instances(key) = w; w} or create(key).map{w => instances(key) = w; w}
  }

  Class.forName("org.h2.Driver")

  protected def unfreeze(key: IdType): Box[WorkerType] =
    if (canUnfreeze(key)) Full(unfreezeLifecycle(instantiate(key))) else Empty

  protected def create(key: IdType): Box[WorkerType] =
    if (canCreate(key)) Full(createLifecycle(instantiate(key))) else Empty

  protected def dbFilePathPrefix(id: IdType) = ""

  protected def connIdStringFor(id: IdType): String =
    if (Props.testMode) "jdbc:h2:mem:"+id.uniqueNameForFile+";DB_CLOSE_DELAY=-1"
  else "jdbc:h2:"+dbFilePathPrefix(id)+id.uniqueNameForFile+".db"
  
  protected def connectionFor(id: IdType) = DriverManager.getConnection(connIdStringFor(id))

  protected def ensureJDBCConn(id: IdType) {
    if (!connMgr.contains(id)) {
      connMgr(id) = connectionFor(id)
    }
  }

  protected def unfreezeLifecycle(worker: WorkerType): WorkerType = {
    ensureJDBCConn(worker.id)
    worker.outOfBandMessage(WorkerUnfrozen())
    worker
  }

  protected def createLifecycle(worker: WorkerType): WorkerType = {
    ensureJDBCConn(worker.id)
    worker.outOfBandMessage(WorkerCreated())
    created(worker.id)
    unfreezeLifecycle(worker)
  }

  /**
   * The number of worker instances of this type in memory at one time
   */
  protected[this] def numberInMemory: Int

  protected def connectionManagerForId(id: IdType): ConnectionManager = {
    val conn = connMgr(id)
    new ConnectionManager {
      def newConnection(name: ConnectionIdentifier): Box[Connection] = name match {
        case DefaultConnectionIdentifier => Full(conn)
        case _ => Empty
      }
      def releaseConnection(conn: Connection) = {}
    }
  }

  /**
   * Override this method to indicate that the worker has been created
   */
  protected def created(key: IdType) {}

  protected def canCreate(key: IdType): Boolean

  protected def canUnfreeze(key: IdType): Boolean

  protected def instantiate(key: IdType): WorkerType
}
