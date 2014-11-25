package models.entities

import models.db.MongoConnection
import org.joda.time.DateTime
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson._

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Created by artem on 23.11.14.
 */
case class Document(var id: Option[BSONObjectID], var name: String, var mask: String,
                    var params: Map[String, String], var date: Long, var status: Int,
                    var publishDate: Long, var unpublishDate: Long,var release: Option[BSONObjectID]) {

}

case class Mask(id: Option[BSONObjectID], var name: String, var title: String,
                var params: Map[String, String], repo: String, status: Int) {

}

object Mask extends Entity[Mask] {

  import EntityRW._

  override val collection: BSONCollection = MongoConnection.db.collection("mask")

  def empty(name: String, repo: String, title: String, params: Map[String, String]) = {
    Mask(Some(BSONObjectID.generate), name, title, params, repo, 1)
  }

  def newMask(name: String, repo: String, title: String, params: Map[String, String]) = {
    val mask = Mask(Some(BSONObjectID.generate), name, title, params, repo, 1)
    insert(mask)
  }

  def insert(mask: Mask) = {
    save(mask,MaskWriter)
  }

  def byId(id: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    BSONObjectID.parse(id) match {
      case Success(bsonId) =>
        collection.find(BSONDocument("_id" -> bsonId)).one[Mask]
      case Failure(e) =>
        Future.successful(None)
    }
  }

  def list(repo: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("repo" -> repo)).cursor[Mask].collect[List]()
  }

}

object Document extends Entity[Document] {

  import EntityRW._

  override val collection: BSONCollection = MongoConnection.db.collection("documents")

  /**
   * Generate empty Document
   * @return
   */
  def empty() = Document(None, "", "", Map.empty, DateTime.now().getMillis, 0, DateTime.now().getMillis,
    DateTime.now().plusYears(10).getMillis,None)

  /**
   * Insert new doc
   * TODO: update is it method?
   * @param doc
   * @return
   */
  def insert(doc: Document) = {
    save(doc, DocumentWriter)
  }

  /**
   * Find document by ObjectId<id>
   * @param id
   * @return
   */
  def byId(id: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    BSONObjectID.parse(id) match {
      case Success(bsonId) =>
        collection.find(BSONDocument("_id" -> bsonId)).one[Document]
      case Failure(e) =>
        Future.successful(None)
    }
  }

  /**
   * List all document by mask id
   * @param maskId
   * @return
   */
  def list(maskId: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("mask" -> maskId)).cursor[Document].collect[List]()
  }

}

