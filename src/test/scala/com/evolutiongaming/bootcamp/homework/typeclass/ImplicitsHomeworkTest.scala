package com.evolutiongaming.bootcamp.homework.typeclass

import com.evolutiongaming.bootcamp.homework.typeclass.ImplicitsHomework.MyTwitter
import com.evolutiongaming.bootcamp.homework.typeclass.ImplicitsHomework.MyTwitter.{FbiNote, Twit}
import com.evolutiongaming.bootcamp.homework.typeclass.ImplicitsHomework.SuperVipCollections4s.instances._
import com.evolutiongaming.bootcamp.homework.typeclass.ImplicitsHomework.SuperVipCollections4s.syntax._
import com.evolutiongaming.bootcamp.homework.typeclass.ImplicitsHomework.SuperVipCollections4s.{MutableBoundedCache, PackedMultiMap}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ImplicitsHomeworkTest extends AnyFunSuite {


    test("Counting BYTE works fine") {
        val byte = 1.toByte

        assert(byte.sizeScore == 1)
    }


    test("Counting CHAR works fine") {
        val char = 'a'

        assert(char.sizeScore == 2)
    }

    test("Counting Int works fine") {
        val int = 100

        assert(int.sizeScore == 4)
    }

    test("Counting LONG works fine") {
        val long = 100L

        assert(long.sizeScore == 8)
    }

    test("Counting String works fine") {
        val str = "TEST"

        assert(str.sizeScore == 12 + 2 + 2 + 2 + 2)
    }


    test("Counting Array works fine") {
        val arr = Array("a", "b", "c")

        assert(arr.sizeScore == 12 + 12 + 12 + 12 + 2 + 2 + 2)
    }

    test("Counting List works fine") {
        val list = List("a", "b", "c")

        assert(list.sizeScore == 12 + 12 + 12 + 12 + 2 + 2 + 2)
    }


    test("Counting Vector works fine") {
        val vec = Vector("a", "b", "c")

        assert(vec.sizeScore == 12 + 12 + 12 + 12 + 2 + 2 + 2)
    }


    test("MutableBoundedCache works fine") {

        val cache = new MutableBoundedCache[Char, Char](10)

        cache.put('a', 'a') // cache is 4
        cache.put('b', 'b') // cache is 8
        cache.put('c', 'c') // cache is 12, and the max size score is 10, so we drop the very first element with key 'a', and put 'c'

        cache.get('c') shouldEqual Some('c')
        cache.get('a') shouldEqual None

    }

    test("Calculating TwitCache works fine") {
        val tweet1 = Twit(
            id = 10000,
            userId = 123,
            hashTags = Vector("#hello", "#world"),
            attributes = PackedMultiMap("scala" -> "love"),
            fbiNotes = List(
                FbiNote(
                    month = "April",
                    favouriteChar = 'a',
                    watchedPewDiePieTimes = 234
                )
            )
        ) // Size is 146

        val tweet2 = Twit(
            id = 10001,
            userId = 321,
            hashTags = Vector("#foo", "#bar"),
            attributes = PackedMultiMap("Bitcoin" -> "50000"),
            fbiNotes = List(
                FbiNote(
                    month = "November",
                    favouriteChar = 's',
                    watchedPewDiePieTimes = 283
                )
            )
        ) // Size is 144

        val twitterCache = MyTwitter.createTwitCache(150)

        twitterCache.put(tweet1)
        twitterCache.get(10000) shouldEqual Some(tweet1)
        twitterCache.put(tweet2)
        // It removes tweet1 and puts tweet2 in the cache as the maxSizeScore is 150:
        twitterCache.get(10001) shouldEqual Some(tweet2)
        twitterCache.get(10000) shouldEqual None
    }

}
