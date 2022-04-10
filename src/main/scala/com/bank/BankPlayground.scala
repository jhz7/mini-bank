package com.bank

import akka.NotUsed
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ ActorSystem, Behavior }
import com.bank.actors.{ Bank, PersistentBankAccount }

object BankPlayground {

  import PersistentBankAccount.Command._
  import PersistentBankAccount.Response
  import PersistentBankAccount.Response._

  def main(args: Array[String]): Unit = {
    val rootBehavior: Behavior[NotUsed] = Behaviors.setup { context =>
      val logger = context.log

      logger.info("BankPlayground started")

      val bank            = context.spawn(Bank(), "bank")
      val responseHandler = context.spawn(
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

//      bank ! CreateBankAccount(user = "Jhon", currency = "USD", initialBalance = 10, responseHandler)
      bank ! GetBankAccount(id = "b76fd4c2-67b4-4ff8-a922-9fc94800545b", responseHandler)

      Behaviors.empty
    }

    val system = ActorSystem(rootBehavior, "BankDemo")
  }

}
