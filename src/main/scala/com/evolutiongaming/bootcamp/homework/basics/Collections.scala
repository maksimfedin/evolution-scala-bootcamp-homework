package com.evolutiongaming.bootcamp.homework.basics

import scala.annotation.tailrec

/**
 * @author Maxim Fedin
 */
// hometask:
// https://leetcode.com/problems/shuffle-the-array
// https://leetcode.com/problems/richest-customer-wealth
// https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
// https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points

// optional hometask:
//
// https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
// https://leetcode.com/problems/split-a-string-in-balanced-strings
// https://leetcode.com/problems/matrix-block-sum/
object Collections extends App {


    // https://leetcode.com/problems/running-sum-of-1d-array/
    def runningSum1(nums: Array[Int]): Array[Int] = {
        @tailrec
        def runningSum1(nums: List[Int], acc: List[Int] = List.empty): List[Int] = {
            nums match {
                case Nil => acc
                case x :: xs => runningSum1(xs, acc :+ (x + acc.lastOption.getOrElse(0)))
            }
        }

        runningSum1(nums.toList).toArray
    }

    def runningSum2(nums: Array[Int]): Array[Int] = nums.scanLeft(0)(_ + _).tail


    // https://leetcode.com/problems/shuffle-the-array
    def shuffle(nums: Array[Int], n: Int): Array[Int] = {
        val mid = nums.length / 2

        @tailrec
        def shuffleAcc(x: Int, acc: List[Int] = List.empty[Int]): List[Int] = {
            x match {
                case _ if x == mid => acc
                case _ => shuffleAcc(x + 1, acc ++ Array(nums(x), nums(x + mid)))
            }

        }

        shuffleAcc(0).toArray
    }

    // https://leetcode.com/problems/richest-customer-wealth
    def maximumWealth(accounts: Array[Array[Int]]): Int = accounts.foldLeft(accounts.head.sum) { (acc, x) => acc max x.sum }


    // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
    def kidsWithCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = candies.foldLeft(Array.empty[Boolean]) { (acc, x) => acc :+ ((x + extraCandies) >= candies.max) }


    // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points

    // Straightforward solution
    def maxWidthOfVerticalArea(points: Array[Array[Int]]): Int = {
        val xs = points
          .map { case Array(x, _) => x }
          .sorted
        xs
          .foldLeft((0, xs.head)) { case ((maxDistance, lastElement), x) =>
              if (x - lastElement > maxDistance) (x - lastElement, x) else (maxDistance, x)

          }
    }._1

    // optional hometask:


    // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
    def maxDepth(s: String): Int = {
        s
          .foldLeft(0, 0) { case ((currentDepth, maximumDepth), current) =>
              current match {
                  case '(' => (currentDepth + 1, currentDepth + 1 max maximumDepth)
                  case ')' => (currentDepth - 1, maximumDepth)
                  case _ => (currentDepth, maximumDepth)
              }
          }._2
    }


    // https://leetcode.com/problems/split-a-string-in-balanced-strings
    def balancedStringSplit(s: String): Int =
        s
          .foldLeft((0, 0)) { case ((flag, count), current) =>
              ((flag, count), current) match {
                  case ((1, count), 'L') => (0, count + 1)
                  case ((-1, count), 'R') => (0, count + 1)
                  case ((flag, count), 'R') => (flag + 1, count)
                  case ((flag, count), 'L') => (flag - 1, count)
                  case _ => (flag, count)
              }
          }._2

    // https://leetcode.com/problems/matrix-block-sum/
    def matrixBlockSum(mat: Array[Array[Int]], K: Int): Array[Array[Int]] = ???

    def scanLeft[T](zero: T)(list: List[T])(f: (T, T) => T): List[T] = list.foldLeft(List(zero)) { (acc, current) => acc :+ f(acc.last, current) }

    def count(s: String): List[(Char, Int)] =
        s.foldRight(List.empty[(Char, Int)]) { (current, acc) =>
            (acc, current) match {
                case ((prevChar, prevOccur) :: xs, curr) if curr == prevChar => (prevChar, prevOccur + 1) +: xs
                case (acc, curr: Char) => (curr, 1) +: acc
            }

        }

}
