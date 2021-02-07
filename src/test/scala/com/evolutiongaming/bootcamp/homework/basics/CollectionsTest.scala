package com.evolutiongaming.bootcamp.homework.basics

import com.evolutiongaming.bootcamp.homework.basics.Collections.{count, scanLeft}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

/**
 * @author Maxim Fedin
 */
class CollectionsTest extends AnyFlatSpec {

    "scanLeft" should "work correctly on numbers" in {
        val numbers = (1 to 100).toList
        scanLeft(0)(numbers)(_ + _) shouldEqual numbers.scanLeft(0)(_ + _)
    }

    "scanLeft" should "work correctly on letters" in {
        val letters = ('a' to 'z').toList.map(_.toString)
        scanLeft("")(letters)(_ + _) shouldEqual letters.scanLeft("")(_ + _)
    }

    "count" should "pass" in {
        count("aaaabbbcca") shouldEqual List(('a', 4), ('b', 3), ('c', 2), ('a', 1))
    }


}
