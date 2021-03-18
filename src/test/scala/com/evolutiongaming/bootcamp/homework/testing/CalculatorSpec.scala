package com.evolutiongaming.bootcamp.homework.testing

import com.evolutiongaming.bootcamp.homework.testing.Calculator.Operation.{Divide, Minus, Multiply, Plus}
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

/**
 * @author Maxim Fedin
 */
// As you might have noticed the calculator from the exercise 2 is buggy and far from perfect.
// So you are to write a real thing: production-ready calculator!
// Try to cover it with tests as much as possible.
// Check the coverage level with [scoverage](https://github.com/scoverage/sbt-scoverage)
// and improve it if possible. Note that the point of this exercise is not to write something
// complicated but to write tests and play with code coverage.
// What you write is up to you and you can choose more entertaining topic if you like.
class CalculatorSpec extends AnyFunSuite with EitherValues {


    test("Calculator screen works correctly") {
        val calculator = Calculator()
        val updatedCalculator = calculator.enter(1).value.enter(2).value.enter(3).value.enter(4).value
        assert(updatedCalculator.screen == 1234)
    }

    test("Calculator screen works correctly on a wrong digit") {
        val calculator = Calculator()
        assert(calculator.enter(-1).left.value == "digit out of range")
    }


    test("Calculator  works  correctly on multiply") {
        val calculator = Calculator(screen = 10)

        val calcMultiply = calculator.multiply
        calcMultiply shouldBe Calculator(10, 0, Some(Multiply))

        val calcResult = calcMultiply.enter(3).value.calculate
        calcResult.screen shouldBe 30

    }

    test("Calculator  works  correctly on divide") {
        val calculator = Calculator(screen = 10)

        val calcDivide = calculator.divide
        calcDivide shouldBe Calculator(10, 0, Some(Divide))

        val calcResult = calcDivide.enter(2).value.calculate
        calcResult.screen shouldBe 5
    }


    test("Calculator  works  correctly on minus") {
        val calculator = Calculator(screen = 10)

        val calcMinus = calculator.minus
        calcMinus shouldBe Calculator(10, 0, Some(Minus))

        val calcResult = calcMinus.enter(3).value.calculate
        calcResult.screen shouldBe 7
    }


    test("Calculator  works  correctly on plus") {
        val calculator = Calculator(screen = 10)

        val calcPlus = calculator.plus
        calcPlus shouldBe Calculator(10, 0, Some(Plus))

        val calcResult = calcPlus.enter(3).value.calculate
        calcResult.screen shouldBe 13
    }


}
