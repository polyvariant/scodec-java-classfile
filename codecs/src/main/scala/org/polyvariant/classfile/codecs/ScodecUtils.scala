/*
 * Copyright 2022 Polyvariant
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.polyvariant.classfile.codecs

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
          case _ =>
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
