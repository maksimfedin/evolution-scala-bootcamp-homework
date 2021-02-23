package com.evolutiongaming.bootcamp.homework.typeclass

import com.evolutiongaming.bootcamp.homework.typeclass.v2.TypeclassTask.{HashCode, HashCodeSyntax}
import org.scalatest.funsuite.AnyFunSuite

class TypeclassTask extends AnyFunSuite {


    test("Hash for string works fine") {
        implicit val hashcodeString: HashCode[String] = _.hashCode
        assert("hello world".hash == 1794106052)
    }
}
