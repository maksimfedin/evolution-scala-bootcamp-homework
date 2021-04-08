package com.evolutiongaming.bootcamp.homework.error_handling

import cats.data.ValidatedNec
import cats.implicits.{catsSyntaxApply, catsSyntaxTuple4Semigroupal, catsSyntaxValidatedIdBinCompat0}
import com.evolutiongaming.bootcamp.homework.error_handling.ErrorHandling.PaymentCard.ValidationError

import java.time.YearMonth
import java.time.format.DateTimeFormatter
import scala.util.{Failure, Success, Try}

/**
 * @author Maxim Fedin
 */
// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.

object ErrorHandling {

    final case class OwnerName(v: String) extends AnyVal

    final case class CardNumber(v: String) extends AnyVal

    final case class ExpirationDate(v: YearMonth) extends AnyVal

    final case class CVVNumber(v: String) extends AnyVal

    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    final case class PaymentCard private(
        owner: OwnerName,
        cardNumber: CardNumber,
        expirationDate: ExpirationDate,
        cvvNumber: CVVNumber
    )

    object PaymentCard {

        import PaymentCardValidator._


        def apply(
            name: String,
            number: String,
            expirationDate: String,
            securityCode: String
        ): AllErrorsOr[PaymentCard] =
            validate(name, number, expirationDate, securityCode)

        def validate(
            name: String,
            number: String,
            expirationDate: String,
            securityCode: String,
        ): AllErrorsOr[PaymentCard] =
            (validateName(name), validateCardNumber(number), validateExpirationDate(expirationDate), validateSecurityCode(securityCode)).mapN {
                case (validName, validNumber, validExpirationDate, validSecurityCode) =>
                    new PaymentCard(validName, validNumber, validExpirationDate, validSecurityCode)
            }


        sealed trait ValidationError

        object ValidationError {

            final case object NameLengthIsInvalid extends ValidationError {
                override def toString: String = "Name must be between 3 and 30 characters"
            }

            final case object WrongName extends ValidationError {
                override def toString: String = "Name cannot contain special characters and digits"
            }

            final case object CardNumberIsNotNumeric extends ValidationError {
                override def toString: String = "Card number must be a number"
            }

            final case object CardNumberInvalidChecksum extends ValidationError {
                override def toString: String = "Card number has invalid checksum based on Lihn algorithm"
            }

            final case object ExpirationDateCardExpired extends ValidationError {
                override def toString: String = "Credit card has expired"
            }

            final case object ExpirationDateIsInvalid extends ValidationError {
                override def toString: String = "Expiration date format must be in MM/yy"
            }

            final case object SecurityCodeIsInvalid extends ValidationError {
                override def toString: String = "Security code must contain exactly 3 digits"
            }

        }

        object PaymentCardValidator {

            import com.evolutiongaming.bootcamp.homework.error_handling.ErrorHandling.PaymentCard.ValidationError._

            def validateName(name: String): AllErrorsOr[OwnerName] = {
                def validateNameLength: AllErrorsOr[OwnerName] =
                    if (name.length >= 3 && name.length <= 30) OwnerName(name).validNec
                    else NameLengthIsInvalid.invalidNec

                def validateNameHasSpecialCharacters: AllErrorsOr[OwnerName] =
                    if (name.matches("^[a-zA-Z ]+$")) OwnerName(name).validNec
                    else WrongName.invalidNec

                validateNameLength *> validateNameHasSpecialCharacters

            }


            def validateCardNumber(number: String): AllErrorsOr[CardNumber] = {

                def validateCardNumberIsNotNumeric: AllErrorsOr[CardNumber] = number.toLongOption match {
                    case Some(_) => CardNumber(number).validNec
                    case None => CardNumberIsNotNumeric.invalidNec
                }

                def validateCardNumberInvalidChecksum(number: String): AllErrorsOr[CardNumber] =
                    if (luhnChecksum(number)) CardNumber(number).validNec
                    else CardNumberInvalidChecksum.invalidNec

                validateCardNumberIsNotNumeric.andThen(number => validateCardNumberInvalidChecksum(number.v))

            }


            def validateExpirationDate(expDate: String): AllErrorsOr[ExpirationDate] = {

                def validateExpirationDateIsInvalid: AllErrorsOr[ExpirationDate] = {
                    Try(YearMonth.parse(expDate, DateTimeFormatter.ofPattern("MM/yy"))) match {
                        case Success(yearMonth) => ExpirationDate(yearMonth).validNec
                        case Failure(_) => ExpirationDateIsInvalid.invalidNec
                    }
                }

                def validateExpirationDateCardExpired(yearMonth: YearMonth): AllErrorsOr[ExpirationDate] = {
                    if (YearMonth.now().isBefore(yearMonth)) ExpirationDate(yearMonth).validNec
                    else ExpirationDateCardExpired.invalidNec

                }

                validateExpirationDateIsInvalid.andThen(yearMonth => validateExpirationDateCardExpired(yearMonth.v))
            }

            def validateSecurityCode(code: String): AllErrorsOr[CVVNumber] = {
                if (code.matches("^\\d{3,4}$")) {
                    CVVNumber(code).validNec
                } else {
                    SecurityCodeIsInvalid.invalidNec
                }
            }


        }


        def luhnChecksum(number: String): Boolean = {
            def luhnChecksum_(digitPair: Seq[Int]): Int = {
                digitPair.head + (
                  if (digitPair.length > 1)
                      digitPair.last * 2 - (if (digitPair.last >= 5) 9 else 0)
                  else 0
                  )
            }

            number
              .reverse
              .map(_.toString.toInt)
              .grouped(2)
              .foldLeft(0)((acc, a) => acc + luhnChecksum_(a)) % 10 == 0
        }

    }

}

