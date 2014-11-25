package models.entities

import models.db.MongoConnection
import org.joda.time.DateTime
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.commands._
import reactivemongo.bson._

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Created by artem on 23.11.14.
 */
case class Document(var id: Option[BSONObjectID], var name: Option[String], var mask: Option[BSONObjectID],
                    var params: Map[String, String], var date: Long, var status: Int,
                    var publishDate: Option[Long], var unpublishDate: Option[Long], var release: Option[BSONObjectID],
                    var user: Option[BSONObjectID]) {

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
    save(mask, MaskWriter)
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
  def empty() = Document(Some(BSONObjectID.generate), None, None, Map.empty, DateTime.now().getMillis, 1, None,
    None, None, None)

  /**
   * Generate Document by set of parameters
   * @param maskId
   * @param name
   * @param params
   * @return
   */
  def gen(maskId: String, name: String, params: Map[String, String], user: User) = {
    val doc = empty()
    BSONObjectID.parse(maskId).map { id =>
      doc.mask = Some(id)
      doc.name = Some(name)
      doc.params = params
      doc.user = user.id
      doc
    } match {
      case Success(r) =>
        insert(r)
      case Failure(e) =>
        Future.failed(e)
    }
  }

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

  /**
   * Update Document. This updated currently saved doc in DB
   * @param doc
   * @return
   */
  def update(doc: Document) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val selector = BSONDocument("_id" -> doc.id)
    val mod = BSONDocument(
      "$set" -> BSONDocument(
        "name" -> doc.name,
        "mask" -> doc.mask,
        "params" -> doc.params,
        "date" -> BSONDateTime(doc.date),
        "status" -> BSONInteger(doc.status),
        "publishDate" -> BSONDateTime(doc.publishDate.getOrElse(DateTime.now().getMillis)),
        "unpublishDate" -> BSONDateTime(doc.unpublishDate.getOrElse(DateTime.now().plusYears(10).getMillis)),
        "release" -> doc.release,
        "user" -> doc.user
      )
    )
    collection.update(selector,mod).map{_ => doc}
  }

}

