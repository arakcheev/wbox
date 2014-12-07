package reactivemongo.bson

import org.joda.time.DateTime

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

/**
 * This trait is basic trait for all operators
 */
sealed trait Operator {

  /**
   * Method to make BSONDocument
   * @return
   */
  def make: BSONDocument

  /**
   * Operator name
   */
  val op: String

  //todo: Replace with macro
  def valueMatcher[A](v: A): BSONValue = {
    v match {
      case s: String => BSONString(s)
      case d: Double => BSONDouble(d)
      case i: Int => BSONInteger(i)
      case date: DateTime => BSONDateTime(date.getMillis)
      case _ => BSONNull
    }
  }
}

sealed trait Expression

class SimpleOperator[A](el: (String, A)) extends Operator {
  /**
   * Method to make BSONDocument
   * @return
   */
  override def make: BSONDocument = BSONDocument(el._1 -> valueMatcher(el._2))

  override val op: String = ":"
}

/**
 * Class for comparable operators in mongo query
 * @param el
 */
abstract sealed class Comparable[T](el: (String, T)) extends Operator {

  def make = BSONDocument(el._1 -> BSONDocument(op -> valueMatcher(el._2)))

}

/**
 * Class for logical operators in mongo query
 */
abstract sealed class Logical[A](el: (String, A)*) extends Operator {

  override def make: BSONDocument = {
    BSONDocument(
      op -> BSONArray(el.map { case (field, value) => BSONDocument(field -> valueMatcher(value))})
    )
  }

}

case class AND[T](xs: (String, T)*) extends Logical {

  override val op: String = "$and"
}

object AND {

  def apply(ops: Operator*): Operator = {
    new Operator {
      /**
       * Method to make BSONDocument
       * @return
       */
      override def make: BSONDocument = BSONDocument(
        "$and" -> {
          if (ops.isEmpty) Seq(BSONDocument.empty) else ops.map(_.make)
        }
      )

      override val op: String = ""
    }
  }
}

case class OR[T](xs: (String, T)*) extends Logical {

  override val op: String = "$or"
}

object OR {

  def apply(ops: Operator*): Operator = {
    new Operator {
      /**
       * Method to make BSONDocument
       * @return
       */
      override def make: BSONDocument = BSONDocument(
        "$or" -> {
          if (ops.isEmpty) Seq(BSONDocument.empty) else ops.map(_.make)
        }
      )

      override val op: String = "$or"
    }
  }
}

/**
 * Implements `$gt` operator
 * @param el
 */
case class GT[A](el: (String, A)) extends Comparable(el) {
  override val op: String = "$gt"
}

/**
 * Implements `$gte` operator
 * @param el
 */
case class GTE[A](el: (String, A)) extends Comparable(el) {
  override val op: String = "$gte"
}

/**
 * Implements `$lt` operator
 * @param el
 */
case class LT[A](el: (String, A)) extends Comparable(el) {
  override val op: String = "$lt"
}

/**
 * Implements `$lte` operator
 * @param el
 */
case class LTE[A](el: (String, A)) extends Comparable(el) {
  override val op: String = "$lte"
}

case class NE[A](el: (String, A)) extends Comparable(el) {
  override val op: String = "$ne"
}

case class IN[A](el: (String, A)) extends Comparable(el) {
  override val op: String = "$in"
}

case class NIN[A](el: (String, A)) extends Comparable(el) {
  override val op: String = "$nin"
}