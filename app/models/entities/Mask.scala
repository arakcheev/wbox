package models.entities

import models.SecureGen
import models.db.MongoConnection
import org.joda.time.DateTime
import play.api.{Play, Logger}
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.{BSONDocument, BSONObjectID}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/*
 * Copyright 2014(28.11.14) Arakcheev Artem (artem.arakcheev@phystech.edu)
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

case class Mask(var id: Option[BSONObjectID], var name: Option[String], var title: Option[String],
                var params: Map[String, String], var repo: Option[BSONObjectID], var status: Int, var uuid: Option[String],
                var revision: Option[Int], var user: Option[BSONObjectID], var date: Option[Long]) {

}

object Mask extends Entity[Mask] {

  import models.entities.EntityRW._

  override type TT = Mask

  override val collection: BSONCollection = MongoConnection.db.collection("mask")

  /**
   * Generate empty Mask
   * @return
   */
  def empty() = {
    Mask(Some(BSONObjectID.generate), name = None, title = None, params = Map.empty, repo = None, status = 1,
      uuid = Some(SecureGen.nextSessionId()), revision = None, user = None, date = Some(DateTime.now().getMillis))
  }

  @deprecated("user empty or gen", "25.11.14")
  def newMask(name: String, repo: String, title: String, params: Map[String, String]) = {
    val mask = Mask(Some(BSONObjectID.generate), Some(name), Some(title), params, None, 1, None, None, None, None)
    insert(mask)
  }

  /**
   * Shortcut method for generate new mask by params and save it.
   * @param name
   * @param repo
   * @param title
   * @param params
   * @return
   */
  def gen(name: String, repo: String, title: String, params: Map[String, String])(implicit user: User) = {
    val mask = empty()
    BSONObjectID.parse(repo).map { repoId =>
      mask.repo = Some(repoId)
      mask.name = Some(name)
      mask.title = Some(title)
      mask.params = params
      mask.user = user.id
      mask
    } match {
      case Success(m) =>
        insert(m)
      case Failure(e) =>
        Future.failed(e)
    }
  }

  /**
   * Insert new mask
   * @param mask
   * @return
   */
  def insert(mask: Mask) = {
    save(mask, MaskWriter)
  }

  /**
   * Update current mask. Find mask with such ObjectID and update all other fields.
   * Method or update current mask or create new mask with updated fields.
   * @param mask
   * @param user
   * @return
   */
  //todo: Revision on deletion multi docs
  def update(mask: Mask, genNew: Boolean = true, multi: Boolean = false, deleted: Boolean = false)(implicit user: User): Future[Option[Mask]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    mask.user = user.id
    if (!deleted) {
      mask.revision = mask.revision.map(_ + 1)
    } else {
      mask.revision = None
    }
    mask.date = Some(DateTime.now().getMillis)
    if (genNew) {
      mask.id = Some(BSONObjectID.generate)
      insert(mask)
    } else {
      val _id = mask.id
      val selector = if (multi) {
        BSONDocument("uuid" -> mask.uuid)
      } else {
        BSONDocument("_id" -> _id)
      }
      mask.id = None
      collection.update(selector, BSONDocument(
        "$set" -> MaskWriter.write(mask)
      ), multi = multi).map { wr =>
        if (wr.inError) {
          Logger.logger.error(s"Error updating document (${getClass.getName}}) in MongoDB. More info: ${wr.message}")
          None
        } else {
          Some(mask)
        }
      }
    }
  }

  /**
   * See [[models.entities.Mask.update]]
   * @param id
   * @param name
   * @param title
   * @param params
   * @param user
   * @return
   */
  def update(id: String, name: String, title: String, params: Map[String, String])(implicit user: User): Future[Option[Mask]] = {
    BSONObjectID.parse(id) match {
      case Success(bsonId) =>
        update(bsonId, name, title, params)
      case Failure(e) =>
        Future.successful(None)
    }
  }

  /**
   * See [[models.entities.Mask.update]]
   * @param bsonId
   * @param name
   * @param title
   * @param params
   * @param user
   * @return
   */
  def update(bsonId: BSONObjectID, name: String, title: String, params: Map[String, String])(implicit user: User): Future[Option[Mask]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    byId(bsonId).flatMap {
      case Some(mask) =>
        mask.name = Some(name)
        mask.title = Some(title)
        mask.params = params
        update(mask)
      case None =>
        Future.successful(None)
    }
  }

  /**
   * Find one mask by BSONObjectID
   * @param bsonId
   * @return
   */
  def byId(bsonId: BSONObjectID): Future[Option[Mask]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("_id" -> bsonId)).one[Mask]
  }

  /**
   * Get Option[Mask] by uuid
   * @param uuid
   * @return
   */
  def byUUID(uuid: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("uuid" -> uuid)).sort(BSONDocument("revision" -> -1)).one[Mask]
  }

  /**
   * Find one mask by string representation of BSONObjectId
   * @param id
   * @return
   */
  def byId(id: String): Future[Option[Mask]] = {
    BSONObjectID.parse(id) match {
      case Success(bsonId) =>
        byId(bsonId)
      case Failure(e) =>
        Future.successful(None)
    }
  }

  /**
   * List of masks in repository
   * @param repo must be uuid of repository
   * @return
   */
  //todo move ObjectId tp uuid
  def list(repo: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("repo" -> BSONObjectID.parse(repo).get)).cursor[Mask].collect[List]()
  }

  /**
   * Delete mask.
   * If Mode is ''Test'' then delete from DB, otherwise set status
   * @param id
   */
  def del(id: String)(implicit user: User) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    byId(id).flatMap {
      case Some(mask) =>
        import Play.current
        if (Play.isTest) {
          collection.remove(BSONDocument("_id" -> mask.id)).map { le =>
            if (le.inError) {
              Logger.logger.error(s"Error deleting document (${getClass.getName}}) in MongoDB. More info: ${le.message}")
              None
            } else {
              Some(mask)
            }
          }
        } else {
          mask.status = -1
          update(mask, genNew = false, multi = true, deleted = true)
        }
      case None =>
        Future.successful(None)
    }
  }

}