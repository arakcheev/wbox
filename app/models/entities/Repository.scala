package models.entities

import models.SecureGen
import models.db.{MongoDB, MongoConnection}
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.{BSONInteger, BSONDocumentWriter, BSONDocument, BSONObjectID}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Created by artem on 23.11.14.
 */
case class Repository(id: Option[BSONObjectID], name: String, status: Int, user: String, uuid: String) {

}

object Repository extends Entity[Repository] {

  import EntityRW._

  override val collection: BSONCollection = MongoConnection.db.collection("repository")

  /**
   * Saves new repository
   * @param repo
   * @return
   */
  def insert(repo: Repository) = {
    save(repo,RepositoryWriter)
  }

  def byId(id: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    BSONObjectID.parse(id) match {
      case Success(bsonId) =>
        collection.find(BSONDocument("_id" -> bsonId)).one[Repository]
      case Failure(e) =>
        Future.successful(None)
    }
  }

  def list(user: User) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("user" -> user.id.get.stringify)).cursor[Repository].collect[List]()
  }

  def apply(name: String, user: User): Repository = {
    Repository(Some(BSONObjectID.generate), name, 1, user.id.get.stringify, SecureGen.nextSessionId())
  }

}
