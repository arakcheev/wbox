package models.entities

import models.SecureGen
import models.db.{MongoDB, MongoConnection}
import org.joda.time.DateTime
import play.api.{Logger, Play}
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.{BSONInteger, BSONDocumentWriter, BSONDocument, BSONObjectID}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/*
 * Copyright 2014(23.11.14) Arakcheev Artem (artem.arakcheev@phystech.edu)
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

case class Repository(var id: Option[BSONObjectID], var name: Option[String], var status: Int, var user: Option[BSONObjectID],
                      var uuid: Option[String], var revision: Option[Int], var date: Option[Long]) {

}

object Repository extends Entity[Repository] {

  import EntityRW._

  override type TT = Repository


  override val collection: BSONCollection = MongoConnection.db.collection("repository")

  def empty() = Repository(Some(BSONObjectID.generate), name = None, status = 1, user = None, uuid = Some(SecureGen.nextSessionId()), revision = None,
    Some(DateTime.now().getMillis))

  /**
   * Generate new repo by name and save it.
   * @param name
   * @param user
   * @return
   */
  def gen(name: String)(implicit user: User) = {
    val repo = empty()
    repo.user = user.id
    repo.name = Some(name)
    insert(repo)
  }

  /**
   * Saves new repository
   * @param repo
   * @return
   */
  def insert(repo: Repository) = {
    save(repo, RepositoryWriter)
  }

  /**
   * Find repo by ObjectId<id>
   * @param id
   * @return
   */
  def byId(id: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    BSONObjectID.parse(id) match {
      case Success(bsonId) =>
        collection.find(BSONDocument("_id" -> bsonId)).one[Repository]
      case Failure(e) =>
        Future.successful(None)
    }
  }

  /**
   * Find repo with uuid and max revision
   * @param uuid
   * @return
   */
  def byUUID(uuid: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("uuid" -> uuid)).sort(BSONDocument("revision" -> -1)).one[Repository]
  }

  /**
   * List of all user repositories
   * @param user
   * @return
   */
  def list(implicit user: User) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("user" -> user.id)).cursor[Repository].collect[List]()
  }

  /**
   * Update current repo. Find repo with such ObjectID and update all other fields.
   * Method or update current repo or create new repo with updated fields.
   * @param repo
   * @param user
   * @return
   */
  //todo: Revision on deletion multi docs
  def update(repo: Repository, genNew: Boolean = true, multi: Boolean = false)(implicit user: User): Future[Option[Repository]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    repo.user = user.id
    repo.revision = repo.revision.map(_ + 1)
    repo.date = Some(DateTime.now().getMillis)
    if (genNew) {
      repo.id = Some(BSONObjectID.generate)
      insert(repo)
    } else {
      val _id = repo.id
      val selector = if (multi) {
        BSONDocument("uuid" -> repo.uuid)
      } else {
        BSONDocument("_id" -> _id)
      }
      repo.id = None
      collection.update(selector, BSONDocument(
        "$set" -> RepositoryWriter.write(repo)
      ), multi = multi).map { wr =>
        if (wr.inError) {
          Logger.logger.error(s"Error updating document (${getClass.getName}}) in MongoDB. More info: ${wr.message}")
          None
        } else {
          Some(repo)
        }
      }
    }
  }

  def update(id: String, name: String)(implicit user: User): Future[Option[Repository]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    byId(id).flatMap {
      case Some(repo) =>
        repo.name = Some(name)
        update(repo)
      case None =>
        Future.successful(None)
    }
  }

  /**
   * Delete repository.
   * If Mode is ''Test'' then delete from DB, otherwise set status 
   * @param id
   */
  def del(id: String)(implicit user: User) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    byId(id).flatMap {
      case Some(repo) =>
        import Play.current
        if (Play.isTest) {
          collection.remove(BSONDocument("_id" -> repo.id)).map { le =>
            if (le.inError) {
              Logger.logger.error(s"Error deleting document (${getClass.getName}}) in MongoDB. More info: ${le.message}")
              None
            } else {
              Some(repo)
            }
          }
        } else {
          repo.status = -1
          update(repo, genNew = false, multi = true)
        }
      case None =>
        Future.successful(None)
    }
  }

}
