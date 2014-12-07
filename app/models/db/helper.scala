package reactivemongo.bson

package helpers

/*
 * Copyright 2014(06.12.14) Arakcheev Artem (artem.arakcheev@phystech.edu)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package object helper{

  implicit class Helpers(field: String) {

    def >=[A](value: A) = {
      GTE(field -> value)
    }

    def >[A](value: A) = {
      GT[A](field -> value)
    }

    def <=[A](value: A) = {
      LTE(field -> value)
    }

    def <[A](value: A) = {
      LT(field -> value)
    }

    def in[A](xs: Traversable[A]) = {
      IN(field -> xs)
    }

    def is[A](v: A) = {
      new SimpleOperator[A](field -> v)
    }

    def &(exs: Expression*) = {
      AND("unpublishDate" > 1, "id" in List(1, 2, 3))
    }

  }

  def example() = {
    apply(
      OR("quantity" < 10, "price" is 10)
    )
  }

  def apply[A <: Operator](ops: A*) = {
    ops.foldLeft(BSONDocument.empty) {
      case (bson, op) => bson ++ op.make
    }
  }
}
