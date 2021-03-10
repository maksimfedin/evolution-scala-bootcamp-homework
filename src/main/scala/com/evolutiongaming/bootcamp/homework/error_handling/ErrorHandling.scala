package com.evolutiongaming.bootcamp.homework.error_handling

import cats.data.ValidatedNec
import cats.implicits.{catsSyntaxApply, catsSyntaxTuple4Semigroupal, catsSyntaxValidatedIdBinCompat0}
import com.evolutiongaming.bootcamp.homework.error_handling.ErrorHandling.ValidationError._

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

    final case class PaymentCard(
        name: String,
        number: String,
        expirationDate: String,
        securityCode: String
    )


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

        type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

        private def validateName(name: String): AllErrorsOr[String] = {
            def validateNameLength: AllErrorsOr[String] =
                if (name.length >= 3 && name.length <= 30) name.validNec
                else NameLengthIsInvalid.invalidNec

            def validateNameHasSpecialCharacters: AllErrorsOr[String] =
                if (name.matches("^[a-zA-Z ]+$")) name.validNec
                else WrongName.invalidNec

            validateNameLength *> validateNameHasSpecialCharacters

        }


        private def validateCardNumber(number: String): AllErrorsOr[String] = {

            def validateCardNumberIsNotNumeric: AllErrorsOr[String] = number.toLongOption match {
                case Some(_) => number.validNec
                case None => CardNumberIsNotNumeric.invalidNec
            }

            def validateCardNumberInvalidChecksum(number: String): AllErrorsOr[String] =
                if (luhnChecksum(number)) number.validNec
                else CardNumberInvalidChecksum.invalidNec

            validateCardNumberIsNotNumeric.andThen(number => validateCardNumberInvalidChecksum(number))

        }


        private def validateExpirationDate(expDate: String): AllErrorsOr[String] = {

            def validateExpirationDateIsInvalid: AllErrorsOr[YearMonth] = {
                Try(YearMonth.parse(expDate, DateTimeFormatter.ofPattern("MM/yy"))) match {
                    case Success(yearMonth) => yearMonth.validNec
                    case Failure(_) => ExpirationDateIsInvalid.invalidNec
                }
            }

            def validateExpirationDateCardExpired(yearMonth: YearMonth): AllErrorsOr[String] = {
                if (YearMonth.now().isBefore(yearMonth)) expDate.validNec
                else ExpirationDateCardExpired.invalidNec

            }

            validateExpirationDateIsInvalid.andThen(yearMonth => validateExpirationDateCardExpired(yearMonth))
        }

        private def validateSecurityCode(code: String): AllErrorsOr[String] = {
            if (code.matches("^\\d{3,4}$")) {
                code.validNec
            } else {
                SecurityCodeIsInvalid.invalidNec
            }
        }


        def validate(
            name: String,
            number: String,
            expirationDate: String,
            securityCode: String,
        ): AllErrorsOr[PaymentCard] =
            (validateName(name), validateCardNumber(number), validateExpirationDate(expirationDate), validateSecurityCode(securityCode)).mapN(PaymentCard)
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

