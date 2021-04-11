package com.evolutiongaming.bootcamp.homework.effects

import cats.Applicative
import cats.effect._
import cats.implicits.catsSyntaxApplicativeId

import scala.collection.mutable
import scala.io.{BufferedSource, Source, StdIn}

/**
 * @author Maxim Fedin
 */

/*
  Additional assignment:
  1. Read from the console the file path.
    1.1 Use Blocking Thread Pool
    1.2 Check the transmitted data(Error Handling + Validation).
  2. Read from the console the seed.
    2.1 Use Blocking Thread Pool
    2.2 Check the transmitted data(Error Handling + Validation).
  3. Read the data from the file.
  4. Calculate the signature (in parallel if possible).
    4.1 Use Separate Thread Pool(ContextShift)
    4.2 Split text into words
    4.3 Calculate hash for each word
    4.4 Take the minimal hash
    4.5* Repeat the process for n different hash functions.
  5. Save the signature in memory(think about storage).
  6. Terminate the application.
  def javaHash(word: String, seed: Int = 0): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = 31 * hash + ch.toInt
    hash = hash ^ (hash >> 20) ^ (hash >> 12)
    hash ^ (hash >> 7) ^ (hash >> 4)
  }
  def knuthHash(word: String, constant: Int): Int = {
    var hash = 0
    for (ch <- word.toCharArray)
      hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
    hash % constant
  }
 */
object EffectsHomework2 extends IOApp {

    val store: StoreMap[IO] = new StoreMap[IO]

    override def run(args: List[String]): IO[ExitCode] = Blocker[IO].use { blocker =>
        for {
            sourceResult <- readSource(blocker)
            seed <- readSeed(blocker)
            words <- processSource(blocker)(sourceResult.source)
            minHash <- calculateMinHash(blocker)(words, seed)
            _ <- store.save(sourceResult.fileName, minHash)
        } yield ()
    }.as(ExitCode.Success)

    private def readSource(blocker: Blocker)(implicit contextShift: ContextShift[IO], sync: Sync[IO]): IO[SourceResult] = {
        for {
            fileName <- blocker.blockOn(IO(StdIn.readLine("Read file: ")))
            sourceResult <- blocker
              .delay(SourceResult(fileName, Source.fromFile(fileName)))
              .handleErrorWith(_ => IO(println("Unable to open file. Please try again: ")) *> readSource(blocker))
        } yield sourceResult

    }

    private def readSeed(blocker: Blocker)(implicit contextShift: ContextShift[IO], sync: Sync[IO]): IO[Int] =
        blocker
          .delay(StdIn.readLine("Read seed: ").toInt)
          .handleErrorWith(_ => IO(println("Seed should be numeric. Please Try again:")) *> readSeed(blocker))


    private def processSource(blocker: Blocker)(bs: BufferedSource)(implicit contextShift: ContextShift[IO], sync: Sync[IO]): IO[List[String]] = {
        blocker.delay {
            bs.getLines()
              .flatMap(_.split("\\s+"))
              .filter(_ != "")
              .toList

        }
    }


    private def calculateMinHash(blocker: Blocker)(word: List[String], seed: Int)(implicit contextShift: ContextShift[IO], sync: Sync[IO]): IO[Int] = {
        for {
            javaHash <- blocker.delay(word.map(word => javaHash(word, seed)).min)
            knuthHash <- blocker.delay(word.map(word => knuthHash(word, seed)).min)
            min <- blocker.delay(javaHash min knuthHash)
            _ <- IO(println(s"MinHash: $min"))
        } yield min
    }


    private def javaHash(word: String, seed: Int = 0): Int = {
        var hash = 0
        for (ch <- word.toCharArray)
            hash = 31 * hash + ch.toInt
        hash = hash ^ (hash >> 20) ^ (hash >> 12)
        hash ^ (hash >> 7) ^ (hash >> 4)
    }

    private def knuthHash(word: String, constant: Int): Int = {
        var hash = 0
        for (ch <- word.toCharArray)
            hash = ((hash << 5) ^ (hash >> 27)) ^ ch.toInt
        hash % constant
    }

}

trait Store[F[_]] {
    def save(key: String, hash: Int): F[Unit]
}

class StoreMap[F[_] : Applicative] extends Store[F] {
    private val map: mutable.HashMap[String, Int] = mutable.HashMap.empty[String, Int]

    override def save(key: String, hash: Int): F[Unit] = {
        map.addOne((key, hash))
        ().pure[F]
    }
}

final case class SourceResult(
    fileName: String,
    source: BufferedSource
)
