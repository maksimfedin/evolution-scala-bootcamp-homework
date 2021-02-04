package com.evolutiongaming.bootcamp.homework.basics

import com.evolutiongaming.bootcamp.homework.basics.Basics.{gcd, lcm}
import org.scalatest.funsuite.AnyFunSuite

/**
 * @author Maxim Fedin
 */
class BasicsTest extends AnyFunSuite {


    test("GCD works fine on a positive values (1024,600)") {
        assert(gcd(1024, 600) == 8)
    }


    test("GCD works fine if one of the values is negative (-1024,600)") {
        assert(gcd(-1024, 600) == 8)
    }

    test("GCD works fine on a negative values (-1024,-600)") {
        assert(gcd(-1024, -600) == 8)
    }


    test("LCM works fine on a positive values (1024,600)") {
        assert(lcm(1024, 600).contains(76800))
    }

    test("LCM works fine if one of the values is negative (-1024,600)") {
        assert(lcm(-1024, 600).contains(76800))
    }

    test("LCM works fine on a negative values (-1024,-600)") {
        assert(lcm(-1024, -600).contains(76800))
    }


    test("LCM throws an error if one of the values is zero") {
        assert(lcm(1024, 0) === None)
    }


}
