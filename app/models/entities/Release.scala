package models.entities

import models.db.MongoConnection
import org.joda.time.DateTime
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.{BSONDocument, BSONObjectID}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Created by artem on 25.11.14.
 */
case class Release(var id: Option[BSONObjectID], var name: Option[String], var publishDate: Option[Long],
                   var unpublishDate: Option[Long],
                   var mask: Option[BSONObjectID]) {

}

object Release extends Entity[Release] {

  import EntityRW._

  override val collection: BSONCollection = MongoConnection.db.collection("releases")

  /**
   * Generate empty Release
   * @return
   */
  def empty() = Release(None, None, None, None, None)

  /**
   * Insert new relese
   * //TODO: update is it method?
   * @param rel
   * @return
   */
  def insert(rel: Release) = {
    save(rel, ReleaseWriter)
  }

  /**
   * Find release by ObjectId<id>
   * @param id
   * @return
   */
  def byId(id: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    BSONObjectID.parse(id) match {
      case Success(bsonId) =>
        collection.find(BSONDocument("_id" -> bsonId)).one[Release]
      case Failure(e) =>
        Future.successful(None)
    }
  }

  /**
   * List all releases by mask id
   * @param maskId
   * @return
   */
  def list(maskId: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    BSONObjectID.parse(maskId) match {
      case Success(bsonId) =>
        collection.find(BSONDocument("mask" -> bsonId)).cursor[Release].collect[List]()
      case Failure(e) =>
        Future.successful(Nil)
    }
  }

}