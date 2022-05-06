package com.bank.actors

import akka.actor.typed.scaladsl.{ ActorContext, Behaviors }
import akka.actor.typed.{ ActorRef, Behavior }
import akka.persistence.typed.PersistenceId
import akka.persistence.typed.scaladsl.{ Effect, EventSourcedBehavior }

import java.util.UUID
import scala.util.Failure

object Bank {

  // commands/messages
  import PersistentBankAccount.Command
  import PersistentBankAccount.Command._
  import PersistentBankAccount.Response._

  // events
  sealed trait Event
  final case class BankAccountCreated(id: String) extends Event

  // state
  final case class State(accounts: Map[String, ActorRef[Command]])

  // command handler
  def commandHandler(context: ActorContext[Command]): (State, Command) => Effect[Event, State] = { (state, command) =>
    command match {
      case createBankAccount @ CreateBankAccount(_, _, _, _) =>
        val id             = UUID.randomUUID().toString
        val newBankAccount = context.spawn(PersistentBankAccount(id), id)

        Effect
          .persist(BankAccountCreated(id))
          .thenReply(newBankAccount)(_ => createBankAccount)

      case updateBalanceCommand @ UpdateBalance(id, _, _, replyTo) =>
        state.accounts
          .get(id)
          .fold[Effect[Event, State]](
            Effect.reply(replyTo)(BalanceUpdatedResponse(Failure(new RuntimeException(s"Bank account $id not found"))))
          )(bankAccount => Effect.reply(bankAccount)(updateBalanceCommand))

      case getBankAccount @ GetBankAccount(id, replyTo) =>
        state.accounts
          .get(id)
          .fold[Effect[Event, State]](
            Effect.reply(replyTo)(GetBankAccountResponse(None))
          )(bankAccount => Effect.reply(bankAccount)(getBankAccount))
    }
  }

  // event handler
  def eventHandler(context: ActorContext[Command]): (State, Event) => State = { (state, event) =>
    event match {
      case BankAccountCreated(id) =>
        val account =
          context
            .child(id)                                               // exists after command handling but not in recovery mode
            .getOrElse(context.spawn(PersistentBankAccount(id), id)) // creates a new one in recovery mode
            .asInstanceOf[ActorRef[Command]]

        state.copy(accounts = state.accounts + (id -> account))
    }
  }

  // behavior
  def apply(): Behavior[Command] = Behaviors.setup { context =>
    EventSourcedBehavior[Command, Event, State](
      persistenceId = PersistenceId.ofUniqueId("bank"),
      emptyState = State(Map.empty),
      commandHandler = commandHandler(context),
      eventHandler = eventHandler(context)
    )
  }
}
