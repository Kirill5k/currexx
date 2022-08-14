package currexx.domain

import currexx.domain.market.CurrencyPair
import currexx.domain.session.SessionId
import currexx.domain.user.{UserEmail, UserId}

object errors {

  sealed trait AppError extends Throwable {
    def message: String
    override def getMessage: String = message
  }

  object AppError {
    sealed trait Unauth        extends AppError
    sealed trait NotFound      extends AppError
    sealed trait Conflict      extends AppError
    sealed trait BadReq        extends AppError
    sealed trait Forbidden     extends AppError
    sealed trait Unprocessable extends AppError
    sealed trait BadData       extends AppError

    final case class ClientFailure(client: String, error: String) extends AppError:
      override val message: String = s"Error sending a request to $client: $error"

    final case class NotEnoughDataPoints(source: String, returnedCount: Int) extends BadData:
      override val message: String = s"$source response returned only $returnedCount data points"

    final case class FailedCalculation(message: String) extends BadData

    final case class JsonParsingFailure(original: String, error: String) extends AppError:
      override val message: String = s"Failed to parse json response: $error\n$original"

    final case class AccessDenied(message: String) extends AppError

    final case class AlreadyBeingMonitored(cp: CurrencyPair) extends Conflict:
      override val message: String = s"Monitor for currency pair $cp already exists"

    final case class AccountAlreadyExists(email: UserEmail) extends Conflict:
      override val message: String = s"An account with email $email already exists"

    final case class EntityDoesNotExist(entityName: String, id: String) extends NotFound:
      override val message: String = s"$entityName with id $id does not exist"

    final case class NotSetup(entityName: String) extends NotFound:
      override val message: String = s"Current account doesn't have $entityName-settings set up"

    final case class NotTracked(cp: CurrencyPair) extends NotFound :
      override val message: String = s"Currency pair $cp is not being tracked"
    
    case object InvalidEmailOrPassword extends Unauth:
      override val message: String = "Invalid email or password"

    case object InvalidPassword extends Unauth:
      override val message: String = "Entered password appears to be incorrect"

    case object SomeoneElsesSession extends Forbidden:
      override val message: String = "The current session belongs to a different user"

    case object ExpiredSession extends Forbidden:
      override val message: String = "Session has expired"

    case object InvalidBearerToken extends Forbidden:
      override val message: String = "Invalid Bearer token"

    case object MissingAuthorizationHeader extends Forbidden:
      override val message: String = "Missing authorization header"

    final case class InvalidAuthorizationHeader(error: String) extends Forbidden:
      override val message: String = s"Invalid authorization header - $error"

    final case class SessionDoesNotExist(id: SessionId) extends Forbidden:
      override val message: String = s"Session with id $id does not exist"

    case object IdMismatch extends BadReq:
      override val message: String = "The id supplied in the path does not match with the id in the request body"

    final case class InvalidJwtEncryptionAlgorithm(alg: String) extends AppError:
      override val message = s"unrecognized jwt encryption algorithm $alg"

    final case class InvalidJwtToken(message: String) extends Forbidden

    final case class FailedValidation(message: String) extends Unprocessable

    case object InvalidDateRange extends Unprocessable:
      override val message: String = "Date 'from' must be before date 'to'"
  }
}
