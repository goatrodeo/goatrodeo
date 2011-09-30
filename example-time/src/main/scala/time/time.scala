package time

import java.text.DateFormat
import java.util.{ Date, Locale }
import net.liftweb.mapper.ConnectionManager
import org.stambecco._

// Messages
sealed trait TimeMsg extends QBase
final case object GetTimeMsg extends TimeMsg with MsgWithResponse[QString]
final case class SetLocaleMsg(locale: String) extends TimeMsg

// WorkerId
final case class TimeId(id: Int) extends WorkerId {

  override type MyType  = TimeId
  override type MsgType = TimeMsg
  override type IdType  = Int

  override def myType = manifest[TimeId]

  override def uniqueNameForFile = "time-" + id
}

// Worker
class TimeWorker(id: TimeId, con: TimeId => ConnectionManager) extends WorkerImpl[TimeId, TimeMsg](id, con) {
 
  def doGetTime(msg: GetTimeMsg.type): QString = {
    println("Received GetTimeMsg.")
    DateFormat.getTimeInstance(DateFormat.SHORT, locale) format new Date
  }

  def doSetLocala(msg: SetLocaleMsg) {
    println("Received SetLocalaMsg.")
    locale = new Locale(msg.locale)
  }

  private var locale = Locale.getDefault
}

// WorkMaster
object TimeMaster extends LocalWorkMaster[TimeId] {

  override def idType = manifest[TimeId]

  override protected def canCreate(id: TimeId) = true

  override protected def canUnfreeze(id: TimeId) = true

  override protected def instantiate(id: TimeId) = new TimeWorker(id, connectionManagerForId)

  override protected[this] def numberInMemory = 100
}

// Use it!
object Runner {

  def main(args: Array[String]) {
    WorkerMgr registerWorkMaster TimeMaster
    for { timeWorker <- WorkerMgr find TimeId(1) } {
      timeWorker ! SetLocaleMsg("en")
      OutsideOfWorker on timeWorker ask GetTimeMsg complete { _ map println openOr "?" }
      timeWorker ! SetLocaleMsg("de")
      OutsideOfWorker on timeWorker ask GetTimeMsg complete { _ map println openOr "?" }
    }
  }
}
