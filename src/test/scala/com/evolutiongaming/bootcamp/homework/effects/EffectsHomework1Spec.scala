package com.evolutiongaming.bootcamp.homework.effects

/**
 * @author Maxim Fedin
 */

import com.evolutiongaming.bootcamp.homework.effects.EffectsHomework1.IO
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Try

class EffectsHomework1Spec extends AnyFreeSpec with Matchers {

    "IO map works fine" in {
        assert(IO(2).map(_ + 1).unsafeRunSync() == 3)
    }

    "IO flatMap works fine" in {
        assert(IO(1).flatMap(a => IO(a + 1)).unsafeRunSync() == 2)
    }

    "IO *> works fine" in {
        assert((IO(1) *> IO(2)).unsafeRunSync() == 2)
    }

    "IO as works fine" in {
        assert((IO(1).as(3)).unsafeRunSync() == 3)
    }

    "IO void works fine" in {
        assert(IO(1).void.unsafeRunSync() == ())
    }

    "IO attempt works fine" - {

        "Right case" in {
            assert(IO(2 + 2).attempt.unsafeRunSync() == Right(4))
        }

        "Left case" in {
            val error = new Exception
            assert(IO(throw error).attempt.unsafeRunSync() == Left(error))
        }

    }

    "IO option works fine" - {
        "Some case" in {
            assert(IO(1).option.unsafeRunSync() == Option(1))
        }

        "None case" in {
            assert(IO(null).option.unsafeRunSync().isEmpty)
        }

    }

    "IO handleErrorWith works fine" - {
        "Success case" in {
            assert(IO(1).handleErrorWith(_ => IO(2)).unsafeRunSync() == 1)
        }

        "Error handle case" in {
            val error = new Exception
            assert(IO(throw error).handleErrorWith(_ => IO(2)).unsafeRunSync() == 2)
        }

    }

    "IO redeem works fine" - {
        "Success case" in {
            assert(IO(1).redeem(_ => 1, (x: Int) => x + 1).unsafeRunSync() == 2)
        }

        "redeem handle case" in {
            assert(IO(1 / 0).redeem(_ => 1, (x: Int) => x + 1).unsafeRunSync() == 1)
        }
    }


    "IO redeemWith works fine" - {
        "Success case" in {
            assert(IO(1).redeemWith(_ => IO(1), (x: Int) => IO(x + 1)).unsafeRunSync() == 2)
        }

        "redeem handle case" in {
            assert(IO(1 / 0).redeemWith(_ => IO(1), (x: Int) => IO(x + 1)).unsafeRunSync() == 1)
        }

    }

    "IO unsafeRunSync works fine" in {
        assert(IO(1).unsafeRunSync() == 1)
    }

    "IO apply works fine" in {
        assert(IO.apply(1).unsafeRunSync() == 1)

    }

    "IO suspend works fine" in {
        assert(IO.suspend(IO(1)).unsafeRunSync() == 1)

    }

    "IO delay works fine" in {
        assert(IO.delay(1).unsafeRunSync() == 1)

    }

    "IO pure works fine" in {
        assert(IO.pure(1).unsafeRunSync() == 1)
    }

    "IO fromEither works fine" - {
        "Right case" in {
            assert(IO.fromEither(Right(1)).unsafeRunSync() == 1)
        }

        "Left case" in {
            val error = new RuntimeException("Error")
            assertThrows[RuntimeException](IO.fromEither(Left(error)).unsafeRunSync())
        }

    }

    "IO fromOption works fine" - {
        "Some case" in {
            val error = new RuntimeException("Error")
            IO.fromOption(Some(1))(error).unsafeRunSync() shouldBe 1
        }

        "None case" in {
            val error = new RuntimeException("Error")
            assertThrows[RuntimeException](IO.fromOption(None)(error).unsafeRunSync())
        }
    }

    "IO fromTry works fine" - {
        "Success case" in {
            IO.fromTry(Try(1)).unsafeRunSync() shouldBe 1
        }

        "Failure case" in {
            val error = new RuntimeException("Error")
            assert(IO.fromTry(Try(error)).unsafeRunSync() == error)
        }
    }

    "IO none works fine" in {
        assert(IO.none.unsafeRunSync().isEmpty)

    }

    "IO raiseError works fine" in {
        val error = new RuntimeException("Error")
        assertThrows[RuntimeException](IO.raiseError(error).unsafeRunSync())

    }

    "IO raiseUnless works fine" - {

        "Success case" in {
            val error = new RuntimeException("Error")
            IO.raiseUnless(cond = true)(error).unsafeRunSync() shouldBe()
        }

        "Failure case" in {
            val error = new RuntimeException("Error")
            assertThrows[RuntimeException](IO.raiseUnless(cond = false)(error).unsafeRunSync())
        }

    }

    "IO raiseWhen works fine" - {
        "Success case" in {
            val error = new RuntimeException("Error")
            IO.raiseWhen(cond = false)(error).unsafeRunSync() shouldBe()
        }

        "Failure case" in {
            val error = new RuntimeException("Error")
            assertThrows[RuntimeException](IO.raiseWhen(cond = true)(error).unsafeRunSync())
        }
    }

    "IO unlessA works fine" - {
        "Success case" in {
            val error = new RuntimeException("Error")
            IO.unlessA(cond = false)(IO()).unsafeRunSync() shouldBe()
        }
    }

    "IO whenA works fine" - {
        "Success case" in {
            IO.whenA(cond = false)(IO()).unsafeRunSync() shouldBe()
        }
    }

    "IO unit works fine" in {
        assert(IO.unit.unsafeRunSync() == ())
    }
}
