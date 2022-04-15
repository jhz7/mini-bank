package com.bank.http

import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.util.Timeout
import com.bank.actors.PersistentBankAccount.Command._
import com.bank.actors.PersistentBankAccount.Response._
import com.bank.actors.PersistentBankAccount.{Command, Response}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.generic.auto._

import scala.concurrent.Future
import scala.concurrent.duration._

final case class FailureResponse(message: String)

final case class BankAccountCreateRequest(user: String, currency: String, balance: BigDecimal) {
  def toCommand(replyTo: ActorRef[Response]): Command = CreateBankAccount(user, currency, balance, replyTo)
}

final case class BankAccountUpdateRequest(currency: String, balance: BigDecimal) {
  def toCommand(id: String, replyTo: ActorRef[Response]): Command = UpdateBalance(id, currency, balance, replyTo)
}

class BankRoutes(bank: ActorRef[Command])(implicit actorSystem: ActorSystem[_]) {
  implicit val timeout: Timeout = Timeout(5.seconds)

  def createBankAccount(request: BankAccountCreateRequest): Future[Response] =
    bank.ask(replyTo => request.toCommand(replyTo))

  def updateBalance(id: String, request: BankAccountUpdateRequest): Future[Response] =
    bank.ask(replyTo => request.toCommand(id, replyTo))

  def getBankAccount(id: String): Future[Response] =
    bank.ask(replyTo => GetBankAccount(id, replyTo))

  /*
   POST /bank/
     Payload: bank account creation reuqest as JSON
     Response:
       201: bank account created
       Location: /bank/{uuid}

   GET /bank/{uuid}
     Response:
       200: JSON representation of bank account details
       or
       404 not found

   PUT /bank/{uuid}
     Payload: (currency, amount) as JSON
     Response:
       200: JSON representation of bank account details
       or
       404 not found
       or
       400 bad request. TODO
   */
  val routes: Route =
    pathPrefix("bank") {
      pathEndOrSingleSlash {
        post {
          // parse the payload
          entity(as[BankAccountCreateRequest]) { request =>
            /*
            - Convert the request into a command for the bank actor
            - Send the command to the bank actor
            - expect a reply
             */

            onSuccess(createBankAccount(request)) {
              // send back an HTTP response
              case BankAccountCreatedResponse(id) =>
                respondWithHeader(Location(s"/bank/$id")) {
                  complete(StatusCodes.Created)
                }
            }
          }
        }
      } ~
        path(Segment) { id =>
          get {
            /*
            - send command to the bank
            - expect a reply
             */
            onSuccess(getBankAccount(id)) {
              // send back an HTTP response
              case GetBankAccountResponse(Some(account)) => complete(account)
              case GetBankAccountResponse(None)          =>
                complete(StatusCodes.NotFound, FailureResponse(s"Bank account $id not found"))
            }
          } ~
            put {
              // parse the payload
              entity(as[BankAccountUpdateRequest]) { request =>
                /*
                - send command to the bank
                - expect a reply
                 */
                // TODO: validate the request
                onSuccess(updateBalance(id, request)) {
                  // send back an HTTP response
                  case BalanceUpdatedResponse(Some(account)) => complete(account)
                  case BalanceUpdatedResponse(None)          =>
                    complete(StatusCodes.NotFound, FailureResponse(s"Bank account $id not found"))
                }
              }
            }
        }
    }
}
