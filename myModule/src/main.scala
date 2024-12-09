import akka.actor.{Actor, ActorSystem, Props}
import com.lmax.disruptor.{EventFactory, EventHandler, RingBuffer}
import com.lmax.disruptor.dsl.Disruptor
import quickfix._
import quickfix.field.ClOrdID
import quickfix.field.MsgType
import quickfix.fix42.NewOrderSingle

import java.util.concurrent.Executors

// Define the QuickFIX message handler
class QuickFixApplication extends Application {
  override def onCreate(sessionID: SessionID): Unit = println(s"Session created: $sessionID")

  override def onLogon(sessionID: SessionID): Unit = println(s"Logon: $sessionID")

  override def onLogout(sessionID: SessionID): Unit = println(s"Logout: $sessionID")

  override def toAdmin(message: Message, sessionID: SessionID): Unit = println(s"To Admin: $message")

  override def fromAdmin(message: Message, sessionID: SessionID): Unit = println(s"From Admin: $message")

  override def toApp(message: Message, sessionID: SessionID): Unit = println(s"To App: $message")

  override def fromApp(message: Message, sessionID: SessionID): Unit = {
    println(s"From App: $message")
    // Pass the message to the disruptor for further processing
    QuickFixDisruptor.publish(message)
  }
}

// Define a Disruptor event
case class FixEvent(message: Message)

// Factory for FixEvent
object FixEventFactory extends EventFactory[FixEvent] {
  override def newInstance(): FixEvent = FixEvent(null)
}

// Define Disruptor event handler
class FixEventHandler extends EventHandler[FixEvent] {
  override def onEvent(event: FixEvent, sequence: Long, endOfBatch: Boolean): Unit = {
    println(s"Processing FIX message: ${event.message}")
    // Simulate sending the message to an Akka actor
    AkkaMessageProcessor.actorRef ! event.message
  }
}

// QuickFix Disruptor setup
object QuickFixDisruptor {
  private val disruptor = new Disruptor[FixEvent](FixEventFactory, 1024, Executors.defaultThreadFactory())

  // Attach event handler
  disruptor.handleEventsWith(new FixEventHandler)

  // Start the disruptor
  private val ringBuffer: RingBuffer[FixEvent] = disruptor.start()

  // Method to publish FIX messages to the disruptor
  def publish(message: Message): Unit = {
    val sequence = ringBuffer.next()
    try {
      val event = ringBuffer.get(sequence)
      event.message = message
    } finally {
      ringBuffer.publish(sequence)
    }
  }
}

// Define an Akka actor to process messages
class FixMessageProcessorActor extends Actor {
  override def receive: Receive = {
    case message: Message =>
      println(s"Akka Actor processing message: ${message.toString}")
      // Perform further processing here
  }
}

// Akka message processor setup
object AkkaMessageProcessor {
  val system: ActorSystem = ActorSystem("QuickFixSystem")
  val actorRef = system.actorOf(Props[FixMessageProcessorActor], "FixMessageProcessorActor")
}

// Main application
object QuickFixDisruptorApp extends App {
  // QuickFIX session settings
  val settings = new SessionSettings(getClass.getResourceAsStream("/quickfix-config.cfg"))
  val storeFactory = new FileStoreFactory(settings)
  val logFactory = new FileLogFactory(settings)
  val messageFactory = new DefaultMessageFactory()

  // Initialize QuickFIX
  val application = new QuickFixApplication()
  val initiator = new SocketInitiator(application, storeFactory, settings, logFactory, messageFactory)

  // Start QuickFIX
  initiator.start()

  println("QuickFIX engine started. Press ENTER to quit.")
  scala.io.StdIn.readLine()

  // Stop QuickFIX
  initiator.stop()
  println("QuickFIX engine stopped.")
}
