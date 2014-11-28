package models.entities

import models.SecureGen
import models.db.{MongoDB, MongoConnection}
import org.joda.time.DateTime
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.{BSONInteger, BSONDocumentWriter, BSONDocument, BSONObjectID}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Created by artem on 23.11.14.
 */
case class Repository(var id: Option[BSONObjectID], var name: Option[String], var status: Int, var user: Option[BSONObjectID],
                      var uuid: Option[String], var revision: Option[Int], var date: Option[Long]) {

}

object Repository extends Entity[Repository] {

  import EntityRW._

  override val collection: BSONCollection = MongoConnection.db.collection("repository")

  def empty() = Repository(Some(BSONObjectID.generate), name = None, status = 1, user = None, uuid = None, revision = None,
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

}
