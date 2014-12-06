package models

import models.db.MongoConnection
import models.entities.{DocumentDB, Document, Mask}
import org.joda.time.DateTime
import play.api.Logger
import reactivemongo.api.QueryOpts
import reactivemongo.bson.{BSONArray, BSONDateTime, BSONString, BSONDocument}
import reactivemongo.core.commands._
import scala.concurrent.ExecutionContext.Implicits.global

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
//todo: Only entities with max revision and valid date between must output thought API
object QueryAPI {

  object MaskAPI {

    def list(repo: String)(implicit offset: Int, limit: Option[Int]) = {
      Mask.list(repo)
    }

  }


  object DocumentAPI extends DocumentQueryAPI

}

trait DocumentQueryAPI extends DocumentDB {

  import models.entities.EntityRW._

  /**
   * Projection to all documents
   */
  val PROJECTION = BSONDocument("_id" -> 0, "user" -> 0, "revision" -> 0, "mask" -> 0,"status" -> 0) //todo: projection to release?

  /**
   * List of documents with such mask and limits
   * @param mask
   * @param offset
   * @param limit
   * @return
   */
  //todo: skip limit in aggregate or in queryOpt or in fold??
  def list(mask: Option[String])(implicit offset: Int, limit: Option[Int]) = {
    import scala.concurrent.ExecutionContext.Implicits.global
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
          Match(BSONDocument(
            "mask" -> mask,
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
      collection.find(BSONDocument(
        "$or" -> docs.foldLeft(Seq.empty[BSONDocument]) {//todo why $or but not $and
          case (xs, bson) => xs :+ BSONDocument(
            "uuid" -> bson.getAs[String]("_id"),
            "revision" -> bson.getAs[Int]("maxRevision")
          )
        }
      ), PROJECTION).options(QueryOpts(offset, limit.getOrElse(10))).cursor[Document].collect[List](limit.getOrElse(10))
    }
  }

  /**
   * Implements %like% SQL query
   * @param maskUUID uuid of mask
   * @param name name of field
   * @param value value of filed
   */
  def likeField(maskUUID: Option[String], name: String, value: String)(implicit offset: Int, limit: Option[Int]) = {
    val query = BSONDocument(
      "mask" -> maskUUID,
      "params." + name -> BSONDocument(
        "$regex" -> BSONString(value) //todo: String or Regex
      )
    )
    //projection
    collection.find(query, BSONDocument("_id" -> 0)).cursor[Document].collect[List]()
  }


}
