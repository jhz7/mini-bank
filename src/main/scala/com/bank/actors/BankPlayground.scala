package com.bank.actors

import akka.NotUsed
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ ActorSystem, Behavior, Scheduler }
import akka.util.Timeout

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object BankPlayground {

  import PersistentBankAccount.Command._
  import PersistentBankAccount.Response
  import PersistentBankAccount.Response._

  def main(args: Array[String]): Unit = {
    val rooBehavior: Behavior[NotUsed] = Behaviors.setup { context =>
      val logger = context.log

      logger.info("BankPlayground started")

      val bank                          = context.spawn(Bank(), "bank")
      val responseHandler               = context.spawn(
        Behaviors.receiveMessage[Response] {
          case BankAccountCreatedResponse(id) =>
            logger.info(s"BankAccount created with id: $id")
            Behaviors.same

          case GetBankAccountResponse(account) =>
            logger.info(s"BankAccount Details: $account")
            Behaviors.same
        },
        "replyHandler"
      )
      implicit val timeout: Timeout     = Timeout(2.seconds)
      implicit val scheduler: Scheduler = context.system.scheduler
      implicit val ec: ExecutionContext = context.executionContext

//      bank ! CreateBankAccount(user = "Jhon", currency = "USD", initialBalance = 10, responseHandler)
      bank ! GetBankAccount(id = "b76fd4c2-67b4-4ff8-a922-9fc94800545b", responseHandler)

      Behaviors.empty
    }

    val system = ActorSystem(rooBehavior, "BankDemo")
  }

}
