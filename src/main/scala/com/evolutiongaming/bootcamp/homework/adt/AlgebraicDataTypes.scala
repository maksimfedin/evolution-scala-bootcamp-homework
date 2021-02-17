package com.evolutiongaming.bootcamp.homework.adt

import cats.implicits._

import scala.collection.SortedSet
import scala.reflect.ClassTag
import scala.util.Try

/**
 * @author Maxim Fedin
 */
// Homework. Define all algebraic data types, which would be needed to implement “Hold’em Hand Strength”
// task you completed to join the bootcamp. Use your best judgement about particular data types to include
// in the solution, you can model concepts like:
//
// 1. Suit
// 2. Rank
// 3. Card
// 4. Hand (Texas or Omaha)
// 5. Board
// 6. Poker Combination (High Card, Pair, etc.)
// 7. Test Case (Board & Hands to rank)
// 8. Test Result (Hands ranked in a particular order for a particular Board, accounting for splits)
//
// Make sure the defined model protects against invalid data. Use value classes and smart constructors as
// appropriate. Place the solution under `adt` package in your homework repository.

object AlgebraicDataTypes extends App {

    sealed trait Suit

    final case object Club extends Suit

    final case object Diamond extends Suit

    final case object Heart extends Suit

    final case object Spade extends Suit

    object Suit {
        def apply(value: String): Either[String, Suit] = {
            value match {
                case "D" => Right(Diamond)
                case "S" => Right(Spade)
                case "C" => Right(Club)
                case "H" => Right(Heart)
                case suit => Left(s"Unable to parse suit $suit")
            }
        }
    }

    // the reason I use case class instead of case object,
    // as I will be able to compare the each rank afterwards
    sealed abstract class Rank(val rank: Int)


    final case object Two extends Rank(2)

    final case object Three extends Rank(3)

    final case object Four extends Rank(4)

    final case object Five extends Rank(5)

    final case object Six extends Rank(6)

    final case object Seven extends Rank(7)

    final case object Eight extends Rank(8)

    final case object Nine extends Rank(9)

    final case object Ten extends Rank(10)

    final case object Eleven extends Rank(11)

    final case object Jack extends Rank(12)

    final case object Queen extends Rank(13)

    final case object King extends Rank(14)

    final case object Ace extends Rank(15)

    object Rank {
        def apply(value: Int): Either[String, Rank] = {
            value match {
                case 2 => Right(Two)
                case 3 => Right(Three)
                case 4 => Right(Four)
                case 5 => Right(Five)
                case 6 => Right(Six)
                case 7 => Right(Seven)
                case 8 => Right(Eight)
                case 9 => Right(Nine)
                case 10 => Right(Ten)
                case 11 => Right(Eleven)
                case 12 => Right(Jack)
                case 13 => Right(Queen)
                case 14 => Right(King)
                case 15 => Right(Ace)
                case rank => Left(s"Unable to parse rank $rank")
            }
        }
    }

    final case class Card(
        rank: Rank,
        suit: Suit
    )

    object Card {

        def apply(rank: Rank, suit: Suit): Either[String, Card] = Right(new Card(rank, suit))

        def apply(stringCard: String): Either[String, Card] = {
            stringCard.splitAt(1) match {
                case (suit, rank) =>
                    Try(rank.toInt)
                      .toEither
                      .leftMap(_ => s"Unable to parse string $rank rank as Int")
                      .flatMap { r =>
                          (Rank(r), Suit(suit)) match {
                              case (Right(rank), Right(suit)) => Card(rank, suit)
                              case (Left(err), Right(_)) => Left(err)
                              case (Right(_), Left(err)) => Left(err)
                              case (Left(err1), Left(err2)) => Left(s"$err1, $err2")
                          }
                      }
                case _ => Left(s"Unable to parse card $stringCard")
            }
        }
    }


    sealed trait PokerCombination

    final case object StraightFlush extends PokerCombination

    final case object FourOfKind extends PokerCombination

    final case object FullHouse extends PokerCombination

    final case object Flush extends PokerCombination

    final case object Straight extends PokerCombination

    final case object ThreeOfKind extends PokerCombination

    final case object TwoPair extends PokerCombination

    final case object Pair extends PokerCombination

    final case object HighestCard extends PokerCombination


    sealed trait Hand

    sealed case class HoldemHand private(cards: List[Card])

    object HoldemHand {

        def apply(values: List[Card]): Either[String, HoldemHand] = Either.cond(
            values.length == 2,
            new HoldemHand(values) {},
            s"[HoldemHand error] Incorrect number of cards in: $values"
        )


        def apply(values: String): Either[String, HoldemHand] = foldEither(
            values
              .split(" ")
              .toList
              .map(Card(_))
        ).flatMap { cards =>
            if (cards.length == 2) HoldemHand(cards)
            else Left(s"Incorrect number of cards in: $values")
        }.leftMap(err => s"[HoldemHand error] $err")

    }


    sealed abstract case class OmahaHand private(cards: List[Card])

    object OmahaHand {

        def apply(values: List[Card]): Either[String, OmahaHand] = Either.cond(
            values.length == 4,
            new OmahaHand(values) {},
            s"[OmahaHand error] Incorrect number of cards in: $values"
        )


        def apply(values: String): Either[String, OmahaHand] = foldEither(
            values
              .split(" ")
              .toList
              .map(Card(_))
        ).flatMap { cards =>
            if (cards.length == 4) OmahaHand(cards)
            else Left(s"Incorrect number of cards in: $values")
        }.leftMap(err => s"[OmahaHand error] $err")

    }


    sealed abstract case class Board private(values: List[Card])

    object Board {

        def apply(values: List[Card]): Either[String, Board] = Either.cond(
            values.length == 5,
            new Board(values) {},
            s"[Board error] Incorrect number of cards in: $values"
        )


        def apply(values: String): Either[String, Board] = foldEither(
            values
              .split(" ")
              .toList
              .map(Card(_))
        ).flatMap { cards =>
            Board(cards)
        }.leftMap(err => s"[Board error] $err")
    }

    sealed trait TestCase

    abstract case class TexasHoldemCase private(hands: List[HoldemHand], board: Board) extends TestCase

    object TexasHoldemCase {

        def apply(
            hands: List[HoldemHand] = List.empty[HoldemHand],
            board: Board
        ): Either[String, TexasHoldemCase] = Right(new TexasHoldemCase(hands, board) {})


    }

    abstract case class OmahaHoldemCase private(hands: List[OmahaHand], board: Board) extends TestCase

    object OmahaHoldemCase {
        def apply(
            hands: List[OmahaHand] = List.empty[OmahaHand],
            board: Board
        ): Either[String, OmahaHoldemCase] = Right(new OmahaHoldemCase(hands, board) {})


    }

    abstract case class TestResult private(hands: SortedSet[Hand])

    object TestResult {
        def apply(hands: SortedSet[Hand]): Either[String, TestResult] = Right(new TestResult(hands) {})
    }

    private def foldEither[X, Y: ClassTag](source: List[Either[X, Y]]): Either[X, List[Y]] = {
        source
          .foldLeft[Either[X, List[Y]]](Right(List.empty[Y])) { case (c, r) =>
              r.flatMap { rv =>
                  c.map { cv => cv :+ rv }
              }
          }
    }

}

