package org.polyvariant.classfile

import scodec.Codec
import scodec.Decoder
import scodec.Encoder
import scodec.codecs._

object ScodecUtils {

  def masked[A](range: Codec[Int], valuesWithMasks: Map[A, Int]): Codec[Set[A]] = range.xmap(
    bits => valuesWithMasks.collect { case (flag, mask) if (mask & bits) == mask => flag }.toSet,
    flags => flags.map(flag => valuesWithMasks(flag)).foldLeft(0)(_ | _),
  )

  def weightedN[A](n: Codec[Int], item: Codec[A])(weight: A => Int): Codec[Vector[A]] =
    "weightedN" | {

      def go(remaining: Int): Decoder[List[A]] =
        remaining match {
          case 0 => Decoder.pure(Nil)
          case n =>
            item
              .flatMap { a =>
                go(remaining - weight(a)).map(a :: _)
              }
        }

      n.consume { size =>
        val encoder: Encoder[Vector[A]] = vector(item)

        val decoder = go(size).map(_.toVector)

        Codec(encoder, decoder)
      }(_.map(weight).sum)
    }

}
