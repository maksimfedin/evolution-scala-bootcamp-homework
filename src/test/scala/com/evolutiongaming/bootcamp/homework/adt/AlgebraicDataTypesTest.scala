package com.evolutiongaming.bootcamp.homework.adt

import com.evolutiongaming.bootcamp.homework.adt.AlgebraicDataTypes._
import org.scalatest.flatspec.AnyFlatSpec

/**
 * @author Maxim Fedin
 */


class AlgebraicDataTypesTest extends AnyFlatSpec {


    // Card
    "Card" should "be parsed" in {
        assert(Card("S14") == Right(Card(Right(King), Right(Spade))))
    }

    "Card" should "not be parsed as the Suit is wrong" in {
        assert(Card("E14") == Left("Unable to parse suit E"))
    }

    "Card" should "not be parsed as the Suit and Rank is wrong" in {
        assert(Card("E16") == Left("Unable to parse rank 16, Unable to parse suit E"))
    }

    // HoldemHand

    "HoldemHand" should "be parsed" in {
        assert(
            HoldemHand("S14 D2") match {
                case Right(HoldemHand(List(Card(Right(King), Right(Spade)), Card(Right(Two), Right(Diamond))))) => true
                case _ => false
            }
        )
    }

    "HoldemHand" should "not be parsed as the First card Suit is wrong" in {
        assert(HoldemHand("E14 D2") == Left("[HoldemHand error] Unable to parse suit E"))
    }

    "HoldemHand" should "not be parsed as the Second card Suit is wrong" in {
        assert(HoldemHand("S14 E2") == Left("[HoldemHand error] Unable to parse suit E"))
    }

    "HoldemHand" should "not be parsed as as incorrect number of cards" in {
        assert(HoldemHand("S14 D2 S3") == Left("[HoldemHand error] Incorrect number of cards in: S14 D2 S3"))
    }

    // OmahaHand

    "OmahaHand" should "be parsed" in {
        assert(
            OmahaHand("S14 D2 C6 S9") match {
                case Right(OmahaHand(List(Card(Right(King),Right(Spade)), Card(Right(Two),Right(Diamond)), Card(Right(Six),Right(Club)), Card(Right(Nine),Right(Spade))))) => true
                case x => false
            }
        )
    }

    "OmahaHand" should "not be parsed as the First card Suit is wrong" in {
        assert(OmahaHand("E14 D2 C6 S9") == Left("[OmahaHand error] Unable to parse suit E"))
    }

    "OmahaHand" should "not be parsed as the Second card Suit is wrong" in {
        assert(OmahaHand("S14 E2 C6 S9") == Left("[OmahaHand error] Unable to parse suit E"))
    }


    "OmahaHand" should "not be parsed as as incorrect number of cards" in {
        assert(OmahaHand("S14 D2") == Left("[OmahaHand error] Incorrect number of cards in: S14 D2"))
    }


    // Board
    "Board" should "be parsed" in {
        assert(
            Board("D3 S5 H5 S14 D2") match {
                case Right(Board(List(Card(Right(Three), Right(Diamond)), Card(Right(Five), Right(Spade)), Card(Right(Five), Right(Heart)), Card(Right(King), Right(Spade)), Card(Right(Two), Right(Diamond))))) => true
                case _ => false
            }
        )
    }

    "Board" should "not be parsed" in {
        assert(Board("D16 S5 H5 S14 D2") == Left("[Board error] Unable to parse rank 16"))
    }

    //TestCase

    "TexasHoldemCase" should "be parsed" in {
        assert(
            TexasHoldemCase(
                hands = List(HoldemHand("S14 D2"), HoldemHand("S14 D3")),
                board = Board("D3 S5 H5 S14 D2")
            ) match {
                case Right(TexasHoldemCase(List(Right(HoldemHand(List(Card(Right(King),Right(Spade)), Card(Right(Two),Right(Diamond))))), Right(HoldemHand(List(Card(Right(King),Right(Spade)), Card(Right(Three),Right(Diamond)))))),Right(Board(List(Card(Right(Three),Right(Diamond)), Card(Right(Five),Right(Spade)), Card(Right(Five),Right(Heart)), Card(Right(King),Right(Spade)), Card(Right(Two),Right(Diamond))))))) => true
                case _ => true
            }
        )
    }


    "OmahaHoldemCase" should "be parsed" in {
        assert(
            OmahaHoldemCase(
                hands = List(OmahaHand("S14 D2 C6 S9"), OmahaHand("S11 D5 C9 D3")),
                board = Board("D3 S5 H5 S14 D2")
            ) match {
                case Right(OmahaHoldemCase(List(Right(OmahaHand(List(Card(Right(King),Right(Spade)), Card(Right(Two),Right(Diamond))))), Right(OmahaHand(List(Card(Right(King),Right(Spade)), Card(Right(Three),Right(Diamond)))))),Right(Board(List(Card(Right(Three),Right(Diamond)), Card(Right(Five),Right(Spade)), Card(Right(Five),Right(Heart)), Card(Right(King),Right(Spade)), Card(Right(Two),Right(Diamond))))))) => true
                case _ => true
            }
        )
    }

}
