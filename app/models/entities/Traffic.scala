/*
 * Copyright 2014(8.12.14) Arakcheev Artem (artem.arakcheev@phystech.edu)
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

package models.entities

import controllers.AccessHeaders
import models.db.MongoConnection
import org.joda.time.DateTime
import play.api.libs.iteratee.Iteratee
import play.api.mvc.{Request, Result}
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.{BSONDateTime, BSONDocument, BSONLong}

import scala.concurrent.ExecutionContext.Implicits.global

class Traffic {

}

object Traffic extends TrafficDB

trait TrafficDB extends Entity[Traffic] {
  override type TT = this.type

  override val collection: BSONCollection = MongoConnection.db.collection("traffic")

  def apply() = {

  }

  def write(response: Array[Byte], repo: String) = {
    ???
  }

  /**
   * Write traffic asynchronously
   * @param request
   * @param response
   * @tparam A
   * @return
   */
  def write[A](request: Request[A], response: Result) {
    response.body.run(Iteratee.foreach[Array[Byte]] { body =>
      collection.insert(BSONDocument(
        "ts" -> BSONDateTime(DateTime.now().getMillis),
        "repo" -> request.headers.get(AccessHeaders.X_REPOSITORY),
        "request" -> body.length,
        "rid" -> BSONLong(request.id)
      ))
    })
  }

}
