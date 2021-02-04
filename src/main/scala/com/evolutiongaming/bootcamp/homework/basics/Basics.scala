package com.evolutiongaming.bootcamp.homework.basics


import scala.annotation.tailrec

/**
 * @author Maxim Fedin
 */
// Homework. Implement functions that calculate https://en.wikipedia.org/wiki/Lowest_common_denominator and
// https://en.wikipedia.org/wiki/Greatest_common_divisor for integers.
object Basics extends App {


    def lcm(a: Int, b: Int): Option[Int] = (a, b) match {
        case (a, b) if a == 0 || b == 0 => None
        case _ => Some(Math.abs(a * b) / gcd(a, b))
    }

    @tailrec
    def gcd(a: Int, b: Int): Int = (Math.abs(a), Math.abs(b)) match {
        case (a, 0) => a
        case _ => gcd(b, a % b)
    }
}
