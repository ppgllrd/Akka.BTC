/******************************************************************************
  * BTC-akka
  *
  * Pepe Gallardo, 2018
  *
  ****************************************************************************/

package network.node

import java.net.{InetSocketAddress}

import akka.actor.{Actor, ActorSystem, Kill, Props}
import akka.io.Tcp._
import akka.io.{IO, Tcp}
import akka.util.ByteString
import network.message.RawMessage
import network.message.{Message, _}


case class SendMessage(m : Message)

object Client {
  def props(remote: InetSocketAddress, actorSystem: ActorSystem) = Props(classOf[Client], remote, actorSystem)
}

class Client(address: InetSocketAddress, actorSystem: ActorSystem) extends Actor {
  IO(Tcp)(actorSystem) ! Connect(address)

  private var inputStream = ByteString()

  def receive = {
    case CommandFailed(command: Tcp.Command) =>
      println("Failed to connect to " + address.toString)
      self ! Kill

    case Connected(remote, local) =>
      println("Successfully connected to " + address)
      val connection = sender()
      connection ! Register(self)
      context become {
        case Received(data) =>
          inputStream = inputStream ++ data

          var moreMessages = true
          while(moreMessages) {
            RawMessage.fromBytesOpt(inputStream) match {
              case None =>
                moreMessages = false
              case Some((rawMessage, remainingInputStream)) =>
                val message = Message.fromRawMessage(rawMessage)
                println("Received: "+address+" "+message)
                message match {
                  case Ping(nonce) =>
                    self ! SendMessage(Pong(nonce))
                  case _ =>
                    ;
                }
                inputStream = remainingInputStream
            }
          }


        case SendMessage(message) =>
          println("Sending:  "+address+" "+message)
          connection ! Write(message.toBytes)



        case m =>
          println(m)
      }
  }
}



