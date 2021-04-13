package com.evolutiongaming.bootcamp.homework.effects

import cats.Monad
import cats.effect.concurrent.Ref
import cats.effect.{Clock, Concurrent, ExitCode, IO, IOApp, Timer}
import cats.implicits._

import scala.concurrent.duration._

/*
 * Please implement a Cache which allows concurrent access.
 *
 * Tip: checking expiration could be represented as some infinite process somewhere in background
 *
 * Tip: you can use following structure to get current time suspended in effect : Clock[F].realTime(MILLISECONDS).flatMap(...)
 * Cached items should have an expiration timestamp after which they are evicted.
 */
object SharedStateHomework extends IOApp {

    trait Cache[F[_], K, V] {
        def get(key: K): F[Option[V]]

        def put(key: K, value: V): F[Unit]
    }

    class RefCache[F[_] : Clock : Monad, K, V](
        state: Ref[F, Map[K, (Long, V)]],
        expiresIn: FiniteDuration
    ) extends Cache[F, K, V] {

        def get(key: K): F[Option[V]] =
            state.get.map { map =>
                map.get(key).map { case (_, value) => value }
            }

        def put(key: K, value: V): F[Unit] = for {
            now <- Clock[F].realTime(MILLISECONDS)
            _ <- state.update(_ + (key -> (now + expiresIn.toMillis, value)))
        } yield ()

    }

    object Cache {
        def of[F[_] : Clock, K, V](
            expiresIn: FiniteDuration,
            checkOnExpirationsEvery: FiniteDuration
        )(implicit T: Timer[F], C: Concurrent[F]): F[Cache[F, K, V]] = for {
            state <- Ref.of(Map.empty[K, (Long, V)])
            _ <- C.start(handler(state, checkOnExpirationsEvery))
        } yield new RefCache[F, K, V](state, expiresIn)

        def handler[F[_] : Clock, K, V](
            state: Ref[F, Map[K, (Long, V)]],
            checkOnExpirationsEvery: FiniteDuration
        )(implicit T: Timer[F], C: Concurrent[F]): F[Unit] =
            (for {
                _ <- T.sleep(checkOnExpirationsEvery)
                now <- T.clock.realTime(MILLISECONDS)
                _ <- state.update(_.filter { case (_, (expirationTime, _)) => expirationTime > now })
            } yield ()).flatMap(_ => handler(state, checkOnExpirationsEvery))


    }

    override def run(args: List[String]): IO[ExitCode] = {
        for {
            cache <- Cache.of[IO, Int, String](10.seconds, 4.seconds)
            _ <- cache.put(1, "Hello")
            _ <- cache.put(2, "World")
            _ <- cache.get(1).flatMap(s => IO {
                println(s"first key $s")
            })
            _ <- cache.get(2).flatMap(s => IO {
                println(s"second key $s")
            })
            _ <- IO.sleep(12.seconds)
            _ <- cache.get(1).flatMap(s => IO {
                println(s"first key $s")
            })
            _ <- cache.get(2).flatMap(s => IO {
                println(s"second key $s")
            })
            _ <- IO.sleep(12.seconds)
            _ <- cache.get(1).flatMap(s => IO {
                println(s"first key $s")
            })
            _ <- cache.get(2).flatMap(s => IO {
                println(s"second key $s")
            })
        } yield ExitCode.Success
    }
}

