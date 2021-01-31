package com.evolutiongaming.bootcamp.homework.basics

import com.evolutiongaming.bootcamp.homework.basics.ClassesAndTraits.Point
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

/**
 * @author Maxim Fedin
 */
class ClassesAndTraitsTest extends AnyFlatSpec {


    "Point" should "be correct" in {
        val point = Point(3.0, 2.0)
        point.minX shouldEqual 3.0
        point.maxX shouldEqual 3.0
        point.minY shouldEqual 2.0
        point.maxY shouldEqual 2.0
        point.move(15, 20) shouldEqual Point(18.0, 22.0)
        point.move(-1, -10) shouldEqual Point(2.0, -8.0)
    }



    //    val test = Triangle(Point(3,2),Point(7,5),Point(0,0))

}
