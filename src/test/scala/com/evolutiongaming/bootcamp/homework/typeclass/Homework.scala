package com.evolutiongaming.bootcamp.homework.typeclass

import com.evolutiongaming.bootcamp.homework.typeclass.v2.Task1.Money
import com.evolutiongaming.bootcamp.homework.typeclass.v2.Task2.{Show, ShowSyntax}
import com.evolutiongaming.bootcamp.homework.typeclass.v2.Task3.{Parse, ParseSyntax}
import com.evolutiongaming.bootcamp.homework.typeclass.v2.Task4.{Equals, EqualsSyntax}
import com.evolutiongaming.bootcamp.homework.typeclass.v2.User
import org.scalatest.funsuite.AnyFunSuite

class Homework extends AnyFunSuite {
    implicit val parseUser: Parse[User] = _.split(",")
      .toList match {
        case id :: name :: Nil => Right(User(id, name))
        case entity => Left(s"Unable to parse entity: $entity")
    }


    implicit def anyEquals[T]: Equals[T] = _ == _

    test("[Task1] Money ordering works fine") {
        val moneyBags = Array(Money(BigDecimal(10000)), Money(BigDecimal(10000)), Money(BigDecimal(3000)), Money(BigDecimal(2000)), Money(BigDecimal(1000)))

        implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)

        assert(moneyBags.sorted sameElements Array(Money(1000), Money(2000), Money(3000), Money(10000), Money(10000)))
    }


    test("[Task2] Money ordering works fine") {

        val user = User("1", "Oleg")
        implicit val showUser: Show[User] = user => s"id: ${user.id}, name: ${user.name}"

        assert(user.show == "id: 1, name: Oleg")

    }


    test("[Task3] Parse works fine") {

        assert("1,John".parse[User] == Right(User("1","John")))

    }

    test("[Task3] Parse error works fine") {


        assert("lalala".parse[User] == Left("Unable to parse entity: List(lalala)"))

    }

    test("[Task4] Equals works fine") {

        assert(123 ~== 123)

    }

}
