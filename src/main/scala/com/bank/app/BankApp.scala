package com.bank.app

import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ ActorRef, ActorSystem, Behavior }
import akka.http.scaladsl.Http
import akka.util.Timeout
import com.bank.actors.Bank
import com.bank.actors.PersistentBankAccount.Command
import com.bank.http.BankRoutes

import scala.concurrent.duration._
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success }

object BankApp {
  def startHttpServer(bank: ActorRef[Command])(implicit system: ActorSystem[_]): Unit = {
    implicit val ec: ExecutionContext = system.executionContext

    val routes = new BankRoutes(bank).routes

    val httpBindingFuture = Http().newServerAt("localhost", 8080).bind(routes)

    httpBindingFuture.onComplete {
      case Success(binding) =>
        val address = binding.localAddress
        system.log.info((s"Server online at http://${address.getHostName}:${address.getPort}"))

      case Failure(exception) =>
        system.log.error("Failed to bind HTTP server", exception)
        system.terminate()
    }
  }

  def main(args: Array[String]): Unit = {
    trait RootCommand
    final case class RetrieveBankActor(replyTo: ActorRef[ActorRef[Command]]) extends RootCommand

    val rootBehavior: Behavior[RootCommand] = Behaviors.setup { context =>
      val bankActor = context.spawn(Bank(), "bank")

      Behaviors.receiveMessage { case RetrieveBankActor(replyTo) =>
        replyTo ! bankActor
        Behaviors.same
      }
    }

    implicit val system: ActorSystem[RootCommand] = ActorSystem(rootBehavior, "bankSystem")
    implicit val timeout: Timeout                 = Timeout(5.seconds)
    implicit val ec: ExecutionContext             = system.executionContext

    val bankActorFuture: Future[ActorRef[Command]] = system.ask[ActorRef[Command]](ref => RetrieveBankActor(ref))

    bankActorFuture.foreach(startHttpServer)
  }
}
