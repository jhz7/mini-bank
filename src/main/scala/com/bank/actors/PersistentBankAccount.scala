package com.bank.actors

import akka.actor.typed.{ ActorRef, Behavior }
import akka.persistence.typed.PersistenceId
import akka.persistence.typed.scaladsl.{ Effect, EventSourcedBehavior }

/** Represents a single bank account
  */
object PersistentBankAccount {
  // Commands
  sealed trait Command
  object Command {
    final case class CreateBankAccount(
      user: String,
      currency: String,
      initialBalance: BigDecimal,
      replyTo: ActorRef[Response]
    ) extends Command

    final case class UpdateBalance(
      id: String,
      currency: String,
      amount: BigDecimal,
      replyTo: ActorRef[Response]
    ) extends Command

//    final case class Deposit(
    //      id: String,
    //      currency: String,
    //      amount: BigDecimal,
    //      replyTo: ActorRef[Response]
    //    ) extends Command
    //
    //    final case class Withdraw(
    //      id: String,
    //      currency: String,
    //      amount: BigDecimal,
    //      replyTo: ActorRef[Response]
    //    ) extends Command

    final case class GetBankAccount(
      id: String,
      replyTo: ActorRef[Response]
    ) extends Command
  }

  // Events
  sealed trait Event
  object Event {
    final case class BankAccountCreated(bankAccount: BankAccount) extends Event
    final case class BalanceUpdated(amount: BigDecimal)           extends Event
  }

  // State
  final case class BankAccount(
    id: String,
    user: String,
    currency: String,
    balance: BigDecimal
  )

  // Responses
  sealed trait Response
  object Response {
    final case class BankAccountCreatedResponse(id: String)                   extends Response
    final case class BalanceUpdatedResponse(bankAccount: Option[BankAccount]) extends Response
    final case class GetBankAccountResponse(bankAccount: Option[BankAccount]) extends Response
  }

  import Command._
  import Event._
  import Response._

  val commandHandler: (BankAccount, Command) => Effect[Event, BankAccount] =
    (state, command) =>
      command match {
        case CreateBankAccount(user, currency, initialBalance, bank) =>
          val id = state.id

          /*
          - bank creates me
          - bank sends me CreateBankAccount
          - I persist BankAccountCreated
          - I update my state
          - I reply back to tha bank with the BankAccountCreatedResponse
          - (The bank surfaces the response to the HTTP server) thi is later
           */

          Effect
            .persist(
              BankAccountCreated(
                BankAccount(id, user, currency, balance = initialBalance)
              )
            )
            .thenReply(bank)(_ => BankAccountCreatedResponse(id))

        case UpdateBalance(_, _, amount, bank) =>
          val newBalance = state.balance + amount

          if (newBalance < 0)
            Effect.reply(bank)(BalanceUpdatedResponse(None))
          else
            Effect
              .persist(BalanceUpdated(amount))
              .thenReply(bank)(newState => BalanceUpdatedResponse(Some(newState)))

        case GetBankAccount(_, bank) =>
          Effect.reply(bank)(GetBankAccountResponse(Some(state)))
      }

  val eventHandler: (BankAccount, Event) => BankAccount = (state, event) =>
    event match {
      case BankAccountCreated(bankAccount) => bankAccount
      case BalanceUpdated(amount)          =>
        state.copy(balance = state.balance + amount)
    }

  def apply(id: String): Behavior[Command] =
    EventSourcedBehavior[Command, Event, BankAccount](
      persistenceId = PersistenceId.ofUniqueId(id),
      emptyState = BankAccount(id, user = "", currency = "", balance = 0),
      commandHandler,
      eventHandler
    )
}
