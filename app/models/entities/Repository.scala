package models.entities

import controllers.AccessRule
import models.SecureGen
import models.db.MongoConnection
import org.joda.time.DateTime
import play.api.libs.iteratee.Iteratee
import play.api.{Logger, Play}
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.{BSONDocument, BSONObjectID}

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

/**
 *
 * @param id
 * @param name
 * @param status
 * @param user - owner of latest update of repository
 * @param uuid
 * @param revision
 * @param date
 */
case class Repository(var id: Option[BSONObjectID], var name: Option[String], var status: Int, var user: Option[String],
                      var uuid: Option[String], var revision: Option[Int], var date: Option[Long], var users: Option[Map[String, Int]],
                      var description: Option[String]) {

  /** Get rule for user */
  def getRule(user: Option[String]) = {
    users.flatMap(_.get(user.getOrElse(""))).getOrElse(AccessRule.NONE)
  }

}

object Repository extends Entity[Repository] {

  import models.entities.EntityRW._

  override type TT = Repository


  override val collection: BSONCollection = MongoConnection.db.collection("repository")

  def empty() = Repository(Some(BSONObjectID.generate), name = None, status = 1, user = None, uuid = Some(SecureGen.nextSessionId()), revision = None,
    Some(DateTime.now().getMillis), users = None, description = None)

  /**
   * Generate new repo by name and save it.
   * @param name
   * @param user
   * @return
   */
  def gen(name: String, description: Option[String])(implicit user: User) = {
    val repo = empty()
    repo.user = user.uuid
    repo.name = Some(name)
    repo.description = description
    repo.users = user.uuid.map(id => Map(id -> AccessRule.CREATOR))
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
   * @param bsonId bson id of repo
   * @return
   */
  def byId(bsonId: BSONObjectID) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("_id" -> bsonId)).one[Repository]
  }


  /**
   * Find repo by ObjectId<id>
   * @param id
   * @return
   */
  @deprecated("Use byId(bsonId: BSONObjectID) instead", "06.12.14")
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
   * Find repo with uuid and max revision
   * @param uuid
   * @return
   */
  //fixme: Option with None return all results
  def byUUID(uuid: Option[String]) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("uuid" -> uuid.getOrElse("-1"))).sort(BSONDocument("revision" -> -1)).one[Repository]
  }

  /**
   * List of all user repositories
   * @param user
   * @return
   */
  //FIXME: Fix max revision access OR delegate to front-end
  def list(implicit user: User) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument(
      "$or" -> List(
        BSONDocument(
          "user" -> user.uuid.getOrElse("-1")
        ),
        BSONDocument(
          "users." + user.uuid.getOrElse("-1") -> BSONDocument("$exists" -> true)
        )
      )
    )).cursor[Repository].collect[List]().map { xs =>
      xs.groupBy(r => r.uuid.getOrElse("-1")).map { case (uuid, ys) =>
        val repo = ys.sortBy(r => -r.revision.getOrElse(1)).head
        if (repo.getRule(user.uuid) <= AccessRule.READ) {
          Some(repo)
        } else {
          None
        }
      }.toList.filter(r => r.isDefined)
    }
  }

  /**
   * Update current repo. Find repo with such ObjectID and update all other fields.
   * Method or update current repo or create new repo with updated fields.
   * @param repo
   * @param user
   * @return
   */
  //todo: Revision on deletion multi docs
  private def update(repo: Repository, genNew: Boolean = true, multi: Boolean = false)(implicit user: User): Future[Option[Repository]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    repo.user = user.uuid
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

  def update(uuid: Option[String], name: String)(implicit user: User): Future[Option[Repository]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    byUUID(uuid).flatMap {
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
   * @param uuid
   */
  def del(uuid: Option[String])(implicit user: User) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    byUUID(uuid).flatMap {
      case Some(repo) =>
        import play.api.Play.current
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
          repo.status = DELETED
          update(repo, genNew = false, multi = true)
        }
      case None =>
        Future.successful(None)
    }
  }

  /**
   * Add access to repository
   * @param uuid id of repo
   * @param rule rule to add
   * @param to uuid of user
   * @param user
   * @return
   */
  def addAccess(uuid: Option[String], rule: Int, to: String)(implicit user: User) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    byUUID(uuid).flatMap {
      case Some(repo) =>
        repo.users = repo.users.map(xs => xs ++ Map(to -> rule))
        update(repo)
      case None =>
        Future.successful(None)
    }
  }

}
