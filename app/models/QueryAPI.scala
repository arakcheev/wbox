package models

import models.db.MongoConnection
import models.entities.{Document, DocumentDB, Mask, MaskDB}
import org.joda.time.DateTime
import reactivemongo.api.QueryOpts
import reactivemongo.bson._
import reactivemongo.core.commands._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

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

//todo:
//todo why 'or' but not 'and' in query
object QueryAPI {

  object MaskAPI extends MaskQueryAPI


  object DocumentAPI extends DocumentQueryAPI

}

trait MaskQueryAPI extends MaskDB {

  import models.entities.EntityRW._

  val PROJECTION = BSONDocument("_id" -> 0, "user" -> 0, "revision" -> 0, "status" -> 0,"repo" ->0)

  final private def query(mather: BSONDocument)(implicit offset: Int, limit: Option[Int]): Future[List[Mask]] = {
    MongoConnection.db.command(
      Aggregate(
        COLLECTION_NAME, Seq(
          Match(
            mather ++ BSONDocument(
              "status" -> BSONDocument("$gt" -> DELETED)
            )
          ), Group(
            BSONString("$uuid")
          )(
              "maxRevision" -> Max("revision")
            )
        )
      )
    ).flatMap { docs =>
      val array = docs.foldLeft(Seq.empty[BSONDocument]) {
        case (xs, bson) => xs :+ BSONDocument(
          "uuid" -> bson.getAs[String]("_id"),
          "revision" -> bson.getAs[Int]("maxRevision")
        )
      }
      collection.find(BSONDocument(
        "$or" -> {
          if (array.isEmpty) Seq(BSONDocument.empty) else array
        }
      ), PROJECTION).options(QueryOpts(offset, limit.getOrElse(10))).cursor[Mask].collect[List](limit.getOrElse(10))
    }
  }

  def list(repo: String)(implicit offset: Int, limit: Option[Int]) = {
    query(BSONDocument("repo" -> repo))
  }

}

trait DocumentQueryAPI extends DocumentDB {

  import models.entities.EntityRW._

  /**
   * Projection to all documents
   */
  val PROJECTION = BSONDocument("_id" -> 0, "user" -> 0, "revision" -> 0, "mask" -> 0, "status" -> 0) //todo: projection to release?

  /**
   *
   * @param mather
   * @param offset
   * @param limit
   * @return
   */
  //todo: skip limit in aggregate or in queryOpt or in fold??
  final private def query(mather: BSONDocument)(implicit offset: Int, limit: Option[Int]): Future[List[Document]] = {
    val now = DateTime.now().getMillis
    //old version of Aggregate
    /*val aggregate =
      BSONDocument(
        "aggregate" -> COLLECTION_NAME,
        "pipeline" -> BSONArray(
          BSONDocument(
            "$match" -> BSONDocument(
              "mask" -> mask,
              "publishDate" -> BSONDocument(
                "$lte" -> BSONDateTime(now)
              ),
              "unpublishDate" -> BSONDocument(
                "$gte" -> BSONDateTime(now)
              ),
              "status" -> BSONDocument("$gt" -> DELETED)
            )
          ),
          BSONDocument(
            "$group" -> BSONDocument(
              "_id" -> "$uuid", //group matched docs by uuid
              "revision" -> BSONDocument("$max" -> "$revision") //take max revision value
            )
          )
        )
      )*/
    MongoConnection.db.command(
      Aggregate(
        COLLECTION_NAME, Seq(
          Match(
            mather ++ BSONDocument(
              "publishDate" -> BSONDocument(
                "$lte" -> BSONDateTime(now)
              ),
              "unpublishDate" -> BSONDocument(
                "$gte" -> BSONDateTime(now)
              ),
              "status" -> BSONDocument("$gt" -> DELETED)
            )
          ), Group(
            BSONString("$uuid")
          )(
              "maxRevision" -> Max("revision")
            )
        )
      )
    ).flatMap { docs =>
      val array = docs.foldLeft(Seq.empty[BSONDocument]) {
        case (xs, bson) => xs :+ BSONDocument(
          "uuid" -> bson.getAs[String]("_id"),
          "revision" -> bson.getAs[Int]("maxRevision")
        )
      }
      collection.find(BSONDocument(
        "$or" -> {
          if (array.isEmpty) Seq(BSONDocument.empty) else array
        }
      ), PROJECTION).options(QueryOpts(offset, limit.getOrElse(10))).cursor[Document].collect[List](limit.getOrElse(10))
    }
  }

  /**
   * List of documents with such mask and limits
   * @param mask
   * @param offset
   * @param limit
   * @return
   */
  def list(mask: Option[String])(implicit offset: Int, limit: Option[Int]) = {
    query(BSONDocument("mask" -> mask))
  }

  /**
   * Implements %like% SQL query
   * @param maskUUID uuid of mask
   * @param name name of field
   * @param value value of filed
   */
  def likeField(maskUUID: Option[String], name: Option[String], value: Option[String])(implicit offset: Int, limit: Option[Int]) = {
    query(BSONDocument(
      "mask" -> maskUUID,
      "params." + name.getOrElse("") -> BSONDocument(
        "$regex" -> value
      )
    ))
  }

  /**
   * Query for document by uuid
   * @param uuid
   * @param offset
   * @param limit
   * @return
   */
  def byId(uuid: Option[String])(implicit offset: Int, limit: Option[Int]) = {
    query(BSONDocument("uuid" -> uuid))
  }


}
