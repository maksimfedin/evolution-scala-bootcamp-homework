package com.evolutiongaming.bootcamp.homework.typeclass.v2

// make as many exercises as you can


final case class User(id: String, name: String)


object Task1 extends App {

    final case class Money(amount: BigDecimal)

    implicit val moneyOrdering: Ordering[Money] = Ordering.by(_.amount)

}

object Task2 extends App {

    trait Show[T] { // fancy toString
        def show(entity: T): String
    }

    object Show {
        def apply[F: Show]: Show[F] = implicitly[Show[F]]

    }

    implicit class ShowSyntax[A: Show](x: A) {
        def show: String = Show[A].show(x)
    }

    implicit val showUser: Show[User] = user => s"id: ${user.id}, name: ${user.name}"

    println(User("1", "Oleg").show)

}

object Task3 extends App {
    type Error = String

    trait Parse[T] { // invent any format you want or it can be csv string
        def parse(entity: String): Either[Error, T]
    }

    object Parse {
        def apply[F: Parse]: Parse[F] = implicitly[Parse[F]]

    }

    implicit class ParseSyntax[A: Parse](x: String) {
        def parse[P: Parse]: Either[Error, P] = Parse[P].parse(x)
    }

    implicit val parseUser: Parse[User] = _.split(",")
      .toList match {
        case id :: name :: Nil => Right(User(id, name))
        case entity => Left(s"Unable to parse entity: $entity")
    }

}

object Task4 extends App {

    trait Equals[T] {
        def equals(left: T, right: T): Boolean
    }

    object Equals {
        def apply[F: Equals]: Equals[F] = implicitly[Equals[F]]

    }

    implicit class EqualsSyntax[A: Equals](left: A) {

        def ~==(right: A): Boolean = Equals[A].equals(left, right)
    }


    implicit def anyEquals[T]: Equals[T] = _ == _

    // Will compile
    23 ~== 34

    //    Won't compile
    //    23 ~== "34"

}

object AdvancedHomework extends App {

    trait Functor[F[_]] {
        def flatMap[A, B](x: F[A])(f: A => F[B]): F[B]
    }

    object Functor {
        def apply[F[_] : Functor]: Functor[F] = implicitly[Functor[F]]
    }


    implicit class FlatMapFunctor[F[_] : Functor, A](x: F[A]) {
        def >>>[B](f: A => F[B]): F[B] = Functor[F].flatMap(x)(f)
    }


    implicit val optionFlatMap: Functor[Option] = new Functor[Option] {
        def flatMap[A, B](x: Option[A])(f: A => Option[B]): Option[B] = x.flatMap(f)
    }

}
