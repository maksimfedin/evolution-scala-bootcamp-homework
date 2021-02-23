package com.evolutiongaming.bootcamp.homework.typeclass.v2


object TypeclassTask extends App {

    trait HashCode[T] {
        def hash(t: T): Int
    }

    object HashCode {
        def apply[F: HashCode]: HashCode[F] = implicitly[HashCode[F]]

    }

    implicit class HashCodeSyntax[A: HashCode](x: A) {
        def hash: Int = HashCode[A].hash(x)
    }

    implicit val hashcodeString: HashCode[String] = _.hashCode

}
