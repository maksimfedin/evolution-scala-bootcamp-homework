package com.evolutiongaming.bootcamp.homework.error_handling

import cats.data.{NonEmptyChain, Validated}
import com.evolutiongaming.bootcamp.homework.error_handling.ErrorHandling.PaymentCard
import com.evolutiongaming.bootcamp.homework.error_handling.ErrorHandling.PaymentCard.PaymentCardValidator
import com.evolutiongaming.bootcamp.homework.error_handling.ErrorHandling.PaymentCard.ValidationError._
import org.scalatest.funsuite.AnyFunSuite

/**
 * @author Maxim Fedin
 */
// Homework. Place the solution under `error_handling` package in your homework repository.
//
// 1. Model `PaymentCard` class as an ADT (protect against invalid data as much as it makes sense).
// 2. Add `ValidationError` cases (at least 5, may be more).
// 3. Implement `validate` method to construct `PaymentCard` instance from the supplied raw data.

class ErrorHandlingTest extends AnyFunSuite {


    test("Luhn checksum algorithm works fine") {
        import ErrorHandling.PaymentCard.luhnChecksum

        assert(luhnChecksum("371449635398431"), "American Express")
        assert(luhnChecksum("30569309025904"), "Diners Club")
        assert(luhnChecksum("6011111111111117"), "Discover")
        assert(luhnChecksum("3530111333300000"), "JCB	")
        assert(luhnChecksum("5555555555554444"), "MasterCard")
        assert(luhnChecksum("4111111111111111"), "Visa")
        assert(!luhnChecksum("312312"), "Random number")

    }

    test("Error Handling with Proper Card works fine") {
        assert(
            PaymentCard(
                  name = "John Doe",
                  number = "5555555555554444",
                  expirationDate = "12/22",
                  securityCode = "312"
              ).isValid
        )
    }


    test("Error Handling with Invalid Card works fine with Expired Expiration date") {
        assert(
            PaymentCard(
                  name = "John Doe",
                  number = "5555555555554444",
                  expirationDate = "12/20",
                  securityCode = "312"
              ) match {
                case Validated.Valid(_) => false
                case Validated.Invalid(errors) =>
                    errors === NonEmptyChain(ExpirationDateCardExpired)
            }
        )
    }

    test("Error Handling with Invalid Card works fine with different errors") {
        assert(
            PaymentCard(
                  name = "fasd1212s33fas",
                  number = "123e",
                  expirationDate = "12/2d2",
                  securityCode = "zxda"
              ) match {
                case Validated.Valid(_) => false
                case Validated.Invalid(errors) =>
                    println(errors.map(_.toString))
                    errors === NonEmptyChain(WrongName, CardNumberIsNotNumeric, ExpirationDateIsInvalid, SecurityCodeIsInvalid)
            }
        )
    }


}
