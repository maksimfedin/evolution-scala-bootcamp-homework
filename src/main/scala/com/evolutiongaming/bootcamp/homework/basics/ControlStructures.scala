package com.evolutiongaming.bootcamp.homework.basics

import cats.implicits._

import java.text.DecimalFormat
import scala.io.Source
import scala.util.{Failure, Success, Try}

// Homework

// Create a command line application that reads various "commands" from the
// stdin, evaluates them, and writes output to stdout.

// Commands are:

//   divide 4 5
// which should output "4 divided by 5 is 0.8"

//   sum 5 5 6 8.5
// which should output "the sum of 5 5 6 8.5 is 24.5"

//   average 4 3 8.5 4
// which should output "the average of 4 3 8.5 4 is 4.875"

//   min 4 -3 -17
// which should output "the minimum of 4 -3 -17 is -17"

//   max 4 -3 -17
// which should output "the maximum of 4 -3 -17 is 4"

// In case of commands that cannot be parsed or calculations that cannot be performed,
// output a single line starting with "Error: "
object ControlStructures {

    object CommandName {
        val Divide = "divide"
        val Sum = "sum"
        val Average = "average"
        val Min = "min"
        val Max = "max"
    }

    sealed trait Command

    final case class Divide(dividend: Double, divisor: Double) extends Command

    final case class Sum(numbers: List[Double]) extends Command

    final case class Average(numbers: List[Double]) extends Command

    final case class Min(numbers: List[Double]) extends Command

    final case class Max(numbers: List[Double]) extends Command

    final case class ErrorMessage(value: String)

    sealed trait Result

    final case class DivideResult(command: Divide, result: Double) extends Result

    final case class SumResult(command: Sum, result: Double) extends Result

    final case class AverageResult(command: Average, result: Double) extends Result

    final case class MinResult(command: Min, result: Double) extends Result

    final case class MaxResult(command: Max, result: Double) extends Result


    def parseCommand(x: String): Either[ErrorMessage, Command] = {
        x
          .trim
          .split(" ")
          .toList match {
            case x :: xs =>
                Try(xs.map(_.toDouble)) match {
                    case Success(arguments) =>
                        (x, arguments) match {
                            case (CommandName.Divide, args) =>
                                args match {
                                    case List(dividend, divisor) => Right(Divide(dividend, divisor))
                                    case _ => Left(ErrorMessage("Unable to parse divide arguments"))
                                }
                            case (CommandName.Sum, args) => Right(Sum(args))
                            case (CommandName.Average, args) => Right(Average(args))
                            case (CommandName.Min, args) => Right(Min(args))
                            case (CommandName.Max, args) => Right(Max(args))
                            case (wrongCommand, _) => Left(ErrorMessage(s"""Wrong command "$wrongCommand""""))
                        }
                    case Failure(_) =>
                        Left(ErrorMessage("arguments should be numeric"))
                }
            case _ => Left(ErrorMessage("Unable to parse command"))
        }
    }

    def calculate(x: Command): Either[ErrorMessage, Result] = x match {
        case cmd@Divide(dividend, divisor) =>
            Either.cond(
                divisor != 0,
                DivideResult(cmd, dividend / divisor),
                ErrorMessage("Cannot divide on zero")
            )
        case cmd@Sum(numbers) => Right(SumResult(cmd, numbers.sum))
        case cmd@Average(numbers) => Right(AverageResult(cmd, numbers.sum / numbers.length))
        case cmd@Min(numbers) => Right(MinResult(cmd, numbers.min))
        case cmd@Max(numbers) => Right(MaxResult(cmd, numbers.max))

    }

    def renderResult(x: Result): Either[ErrorMessage, String] = {
        x match {
            case DivideResult(command, result) => Right(s"${formatResult(command.dividend)} divided by ${formatResult(command.divisor)} is ${formatResult(result)}")
            case SumResult(command, result) => Right(s"the sum of ${formatList(command.numbers)} is ${formatResult(result)}")
            case AverageResult(command, result) => Right(s"the average of ${formatList(command.numbers)} is ${formatResult(result)}")
            case MinResult(command, result) => Right(s"the minimum of ${formatList(command.numbers)} is ${formatResult(result)}")
            case MaxResult(command, result) => Right(s"the maximum of ${formatList(command.numbers)} is ${formatResult(result)}")
        }
    }


    private def formatList(xs: List[Double]): String = xs.map(x => formatResult(x)).mkString(" ")

    private def formatResult(result: Double): String = {
        val format = new DecimalFormat("#.###")
        format.format(result)
    }


    private def formatErrorResult(errorMessage: ErrorMessage) = s"Error: ${errorMessage.value}"

    def process(x: String): String = {
        for {
            command <- parseCommand(x)
            result <- calculate(command)
            renderedResult <- renderResult(result)
        } yield renderedResult
    }.leftMap(formatErrorResult)
      .merge

    def main(args: Array[String]): Unit = Source.stdin.getLines() map process foreach println
}
