package com.evolutiongaming.bootcamp.homework.basics

import com.evolutiongaming.bootcamp.homework.basics.ControlStructures.process
import org.scalatest.flatspec.AnyFlatSpec

/**
 * @author Maxim Fedin
 */
class ControlStructuresTest extends AnyFlatSpec {


    "divide" should "be ok" in {
        assert(process("divide 4 5") == "4 divided by 5 is 0.8")
    }

    "divide on zero" should "be ok" in {
        assert(process("divide 4 0") == "Error: Cannot divide on zero")
    }

    "divide incorrect number of arguments" should "be ok" in {
        assert(process("divide 4 5 3") == "Error: Unable to parse divide arguments")
    }

    "sum" should "be ok" in {
        assert(process("sum 5 5 6 8.5") == "the sum of 5 5 6 8.5 is 24.5")
    }


    "average" should "be ok" in {
        assert(process("average 4 3 8.5 4") == "the average of 4 3 8.5 4 is 4.875")
    }

    "no arguments passed" should "be ok" in {
        assert(process("average") == "Error: No arguments has been added")
    }

    "min" should "be ok" in {
        assert(process("min 4 -3 -17") == "the minimum of 4 -3 -17 is -17")
    }

    "max" should "be ok" in {
        assert(process("max 4 -3 -17") == "the maximum of 4 -3 -17 is 4")
    }

    "wrong command" should "be ok" in {
        assert(process("wrongCommand 1 2 3") == "Error: Wrong command \"wrongCommand\"")
    }

    "wrong arguments" should "be ok" in {
        assert(process("sum asd 2 3") == "Error: arguments should be numeric")
    }

}
