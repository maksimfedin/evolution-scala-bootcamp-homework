package com.evolutiongaming.bootcamp.homework.typeclass

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable

//fill in implementation gaps here making the ImplicitsHomeworkSpec pass!
object ImplicitsHomework extends App {

    /**
     * Lo and behold! Brand new super-useful collection library for Scala!
     *
     * Our main guest today - [[SuperVipCollections4s.MutableBoundedCache]],
     * a specially crafted, mutable but non-thread-safe (sic!), key-value in-memory cache which bounds the size
     * of the data stored.
     *
     * As the real memory footprint of values on JVM is clouded in mystery, for data size estimation we use
     * a thing called size score. Its calculation rules:
     * - size score of a Byte is 1
     * - Int - 4 (as primitive JVM int consists of 4 bytes)
     * - Long - 8
     * - Char - 2 (one UTF-16 symbol is 2 bytes)
     * - String - 12 (supposedly the size of the JVM object header) + length * size score of Char
     * - score for any case class is 12 (again our folk wisdom about JVM object layout) + sum of scores of all
     * the fields
     * - score for any sequence (Array[T], List[T], Vector[T]) is
     * 12 (our old friend object header) + sum of scores of all elements
     * - score for any Map[K, V] is 12 + sum of scores of all keys + sum of scores of all values
     */
    object SuperVipCollections4s {
        type SizeScore = Int

        val BYTE_SIZE = 1
        val INT_SIZE = 4
        val LONG_SIZE = 8
        val CHAR_SIZE = 2

        val OBJECT_HEADER_SIZE = 12

        trait GetSizeScore[T] {
            def apply(value: T): SizeScore
        }

        object GetSizeScore {
            def apply[F: GetSizeScore]: GetSizeScore[F] = implicitly[GetSizeScore[F]]
        }


        object syntax {

            implicit class GetSizeScoreOps[T: GetSizeScore](inner: T) {
                def sizeScore: SizeScore = GetSizeScore[T].apply(inner)
            }

        }

        /**
         * Mutable key-value cache which limits the size score of the data scored.
         *
         * The size score of the data is sum of size scores of all keys + sum of size scores of all values.
         * If upon insertion the total score gets over [[maxSizeScore]], the oldest KV-pairs
         * (in the insertion order) should be evicted. If even with the eviction of all the existing elements,
         * the KV-pair can't be added without violating [[maxSizeScore]] - the behaviour is undefined.
         *
         * @param maxSizeScore max size score for the stored data
         * @tparam K key type
         * @tparam V value type
         */
        final class MutableBoundedCache[K: GetSizeScore, V: GetSizeScore](maxSizeScore: SizeScore) {

            import syntax._

            //with this you can use .sizeScore syntax on keys and values

            /*
            mutable.LinkedHashMap is a mutable map container which preserves insertion order - this might be useful!
             */
            private val map: mutable.LinkedHashMap[K, V] = mutable.LinkedHashMap.empty[K, V]

            private def currentSizeScore: SizeScore = map.iterator
              .map { case (k, v) => k.sizeScore + v.sizeScore }
              .sum

            private def canBeAdded(key: K, value: V): Boolean = {
                val newElementSize = key.sizeScore + value.sizeScore

                currentSizeScore + newElementSize <= maxSizeScore
            }


            @tailrec
            def put(key: K, value: V): Unit =
                if (canBeAdded(key, value)) {
                    map.addOne(key, value)
                } else {
                    if (map.nonEmpty) {
                        map.subtractOne(map.head._1)
                        put(key, value)
                    }
                }

            def get(key: K): Option[V] = map.get(key)

        }


        /**
         * Cool custom immutable multi-map collection - does not extend the standard library collection types
         * (yes, this is a feature)
         */
        final case class PackedMultiMap[K, +V](inner: ArraySeq[(K, V)])

        object PackedMultiMap {
            def empty[K, V]: PackedMultiMap[K, V] = PackedMultiMap()

            def apply[K, V](values: (K, V)*): PackedMultiMap[K, V] = PackedMultiMap(inner = ArraySeq(values: _*))
        }

        /**
         * Type-class allowing us to iterate over different "collection-like" types with one type arg
         */
        trait Iterate[-F[_]] {
            def iterator[T](f: F[T]): Iterator[T]
        }

        object Iterate {
            def apply[F[_] : Iterate]: Iterate[F] = implicitly
        }


        /**
         * Same as [[Iterate]] but for collections containing 2 types of values (think Map's and like)
         */
        trait Iterate2[-F[_, _]] {
            def iterator1[T, S](f: F[T, S]): Iterator[T]

            def iterator2[T, S](f: F[T, S]): Iterator[S]
        }

        object Iterate2 {
            def apply[F[_, _] : Iterate2]: Iterate2[F] = implicitly
        }


        object instances {

            implicit val iterableOnceIterate: Iterate[Iterable] = new Iterate[Iterable] {
                override def iterator[T](f: Iterable[T]): Iterator[T] = f.iterator
            }

            implicit val arrayIterate: Iterate[Array] = new Iterate[Array] {
                override def iterator[T](f: Array[T]): Iterator[T] = f.iterator
            }

            implicit val setIterate: Iterate[Set] = new Iterate[Set] {
                override def iterator[T](f: Set[T]): Iterator[T] = f.iterator
            }

            implicit val mapIterate: Iterate2[mutable.Map] = new Iterate2[mutable.Map] {
                override def iterator1[T, S](f: mutable.Map[T, S]): Iterator[T] = f.keys.iterator

                override def iterator2[T, S](f: mutable.Map[T, S]): Iterator[S] = f.values.iterator
            }

            implicit val packedMultiMapIterate: Iterate2[PackedMultiMap] = new Iterate2[PackedMultiMap] {
                override def iterator1[T, S](f: PackedMultiMap[T, S]): Iterator[T] = f.inner.map(_._1).iterator

                override def iterator2[T, S](f: PackedMultiMap[T, S]): Iterator[S] = f.inner.map(_._2).iterator
            }

            import syntax._
            /*
             replace this big guy with proper implicit instances for types:
             - Byte, Char, Int, Long
             - String
             - Array[T], List[T], Vector[T], Map[K,V], PackedMultiMap[K,V]
               - points to karma if you provide those in a generic way
               (Iterate and Iterate2 type-classes might be helpful!)

             If you struggle with writing generic instances for Iterate and Iterate2, start by writing instances for
             List and other collections and then replace those with generic instances.
              */

            implicit val byteSizeScore: GetSizeScore[Byte] = _ => BYTE_SIZE
            implicit val charSizeScore: GetSizeScore[Char] = _ => CHAR_SIZE
            implicit val intSizeScore: GetSizeScore[Int] = _ => INT_SIZE
            implicit val longSizeScore: GetSizeScore[Long] = _ => LONG_SIZE


            implicit val StringSizeScore: GetSizeScore[String] = _.length * CHAR_SIZE + OBJECT_HEADER_SIZE

            implicit def ArraySizeScore[T: GetSizeScore]: GetSizeScore[Array[T]] = _.map(_.sizeScore).sum + OBJECT_HEADER_SIZE

            implicit def ListSizeScore[T: GetSizeScore]: GetSizeScore[List[T]] = _.map(_.sizeScore).sum + OBJECT_HEADER_SIZE

            implicit def VectorSizeScore[T: GetSizeScore]: GetSizeScore[Vector[T]] = _.map(_.sizeScore).sum + OBJECT_HEADER_SIZE

            implicit def IterableSizeScore[T: GetSizeScore, F[_] : Iterate]: GetSizeScore[F[T]] = iter => Iterate[F].iterator(iter).map(_.sizeScore).sum + OBJECT_HEADER_SIZE

            implicit def Iterable2SizeScore[K: GetSizeScore, V: GetSizeScore, F[_, _] : Iterate2]: GetSizeScore[F[K, V]] = iter =>
                Iterate2[F].iterator1(iter).map(_.sizeScore).sum + Iterate2[F].iterator2(iter).map(_.sizeScore).sum + OBJECT_HEADER_SIZE
        }

    }

    object MyTwitter {

        import SuperVipCollections4s._
        import instances._
        import syntax._


        final case class Twit(
            id: Long,
            userId: Int,
            hashTags: Vector[String],
            attributes: PackedMultiMap[String, String],
            fbiNotes: List[FbiNote],
        )

        final case class FbiNote(
            month: String,
            favouriteChar: Char,
            watchedPewDiePieTimes: Long,
        )

        trait TwitCache {
            def put(twit: Twit): Unit

            def get(id: Long): Option[Twit]
        }

        // It is much better to implement implicit on a case classes for a GetSizeScore though...
        implicit val twitSizeScore: GetSizeScore[Twit] = twit => twit.id.sizeScore + twit.attributes.sizeScore + twit.hashTags.sizeScore + twit.userId.sizeScore + OBJECT_HEADER_SIZE

        implicit val fbiNoteSizeScore: GetSizeScore[FbiNote] = fbiNote => fbiNote.month.sizeScore + fbiNote.favouriteChar.sizeScore + fbiNote.watchedPewDiePieTimes.sizeScore + OBJECT_HEADER_SIZE

        def createTwitCache(maxSizeScore: SizeScore): TwitCache = new TwitCache {

            private val cache: MutableBoundedCache[Long, Twit] = new MutableBoundedCache[Long, Twit](maxSizeScore = maxSizeScore)

            override def put(twit: Twit): Unit = cache.put(twit.id, twit)

            override def get(id: Long): Option[Twit] = cache.get(id)
        }
    }

}
