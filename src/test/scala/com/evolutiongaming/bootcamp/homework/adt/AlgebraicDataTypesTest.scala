package com.evolutiongaming.bootcamp.homework.adt

import com.evolutiongaming.bootcamp.homework.adt.AlgebraicDataTypes._
import org.scalatest.flatspec.AnyFlatSpec

/**
 * @author Maxim Fedin
 */


class AlgebraicDataTypesTest extends AnyFlatSpec {


    // Card
    "Card" should "be parsed" in {
        assert(Card("S14") == Card(King,Spade))
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
                case Right(HoldemHand(List(Card(King, Spade), Card(Two, Diamond)))) => true
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
                case Right(OmahaHand(List(Card(King, Spade), Card(Two, Diamond), Card(Six, Club), Card(Nine, Spade)))) => true
                case _ => false
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
                case Right(Board(List(Card(Three, Diamond), Card(Five, Spade), Card(Five, Heart), Card(King, Spade), Card(Two, Diamond)))) => true
                case _ => false
            }
        )
    }

    "Board" should "not be parsed" in {
        assert(Board("D16 S5 H5 S14 D2") == Left("[Board error] Unable to parse rank 16"))
    }



}
