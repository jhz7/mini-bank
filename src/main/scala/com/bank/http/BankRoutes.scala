package com.bank.http

import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.{ ActorRef, ActorSystem }
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.Location
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.util.Timeout
import cats.data.Validated
import cats.implicits._
import com.bank.actors.PersistentBankAccount.Command._
import com.bank.actors.PersistentBankAccount.Response._
import com.bank.actors.PersistentBankAccount.{ Command, Response }
import com.bank.http.Validation._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.generic.auto._

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{ Failure, Success }

final case class FailureResponse(message: String)

final case class BankAccountCreateRequest(user: String, currency: String, balance: BigDecimal) {
  def toCommand(replyTo: ActorRef[Response]): Command = CreateBankAccount(user, currency, balance, replyTo)
}

object BankAccountCreateRequest {
  implicit val validator: Validator[BankAccountCreateRequest] = (request: BankAccountCreateRequest) =>
    (
      validateRequired(request.user, fieldName = "user"),
      validateRequired(request.currency, fieldName = "currency"),
      validateMinimum(request.balance, fieldName = "balance", threshold = 0).combine(
        validateMinimumAbs(request.balance, fieldName = "balance", threshold = 0.01)
      )
    ).mapN((_, _, _) => request)
}

final case class BankAccountUpdateRequest(currency: String, balance: BigDecimal) {
  def toCommand(id: String, replyTo: ActorRef[Response]): Command = UpdateBalance(id, currency, balance, replyTo)
}

object BankAccountUpdateRequest {
  implicit val validator: Validator[BankAccountUpdateRequest] = (request: BankAccountUpdateRequest) =>
    (
      validateRequired(request.currency, fieldName = "currency"),
      validateMinimumAbs(request.balance, fieldName = "balance", threshold = 0.01)
    ).mapN((_, _) => request)
}

class BankRoutes(bank: ActorRef[Command])(implicit actorSystem: ActorSystem[_]) {
  implicit val timeout: Timeout = Timeout(5.seconds)

  def createBankAccount(request: BankAccountCreateRequest): Future[Response] =
    bank.ask(replyTo => request.toCommand(replyTo))

  def updateBalance(id: String, request: BankAccountUpdateRequest): Future[Response] =
    bank.ask(replyTo => request.toCommand(id, replyTo))

  def getBankAccount(id: String): Future[Response] =
    bank.ask(replyTo => GetBankAccount(id, replyTo))

  def validateRequest[R: Validator](request: R)(routeIfValid: Route): Route =
    validateEntity(request) match {
      case Validated.Invalid(errors) =>
        complete(StatusCodes.BadRequest -> FailureResponse(errors.toList.map(_.errorMessage).mkString(", ")))
      case Validated.Valid(_)        => routeIfValid
    }

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
            // validation
            validateRequest(request) {
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
                // validation
                validateRequest(request) {
                  /*
                    - send command to the bank
                    - expect a reply
                   */
                  onSuccess(updateBalance(id, request)) {
                    // send back an HTTP response
                    case BalanceUpdatedResponse(Success(account)) => complete(account)
                    case BalanceUpdatedResponse(Failure(ex))      =>
                      complete(StatusCodes.NotFound, FailureResponse(s"${ex.getMessage}"))
                  }
                }
              }
            }
        }
    }
}
