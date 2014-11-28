package models.entities

import models.SecureGen
import models.db.MongoConnection
import org.joda.time.DateTime
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson._

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Created by artem on 23.11.14.
 */
case class Document(var id: Option[BSONObjectID], var name: Option[String], var mask: Option[BSONObjectID],
                    var params: Map[String, String], var date: Long, var status: Int,
                    var publishDate: Option[Long], var unpublishDate: Option[Long], var release: Option[BSONObjectID],
                    var user: Option[BSONObjectID], var revision: Option[Int], var uuid: Option[String]) {

}

case class Mask(var id: Option[BSONObjectID], var name: Option[String], var title: Option[String],
                var params: Map[String, String], repo: String, status: Int) {

}

object Mask extends Entity[Mask] {

  import models.entities.EntityRW._

  override val collection: BSONCollection = MongoConnection.db.collection("mask")

  def empty(name: String, repo: String, title: String, params: Map[String, String]) = {
    Mask(Some(BSONObjectID.generate), Some(name), Some(title), params, repo, 1)
  }

  def newMask(name: String, repo: String, title: String, params: Map[String, String]) = {
    val mask = Mask(Some(BSONObjectID.generate), Some(name), Some(title), params, repo, 1)
    insert(mask)
  }

  def insert(mask: Mask) = {
    save(mask, MaskWriter)
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

  def list(repo: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("repo" -> repo)).cursor[Mask].collect[List]()
  }

  /**
   * Update current mask. Find mask with such ObjectID and update all other fields.
   * @param mask
   * @param user
   * @return
   */
  def update(mask: Mask)(implicit user: User): Future[Option[Mask]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val selector = BSONDocument("_id" -> mask.id.get)
    mask.id = None
    //    mask.user = user.id
    collection.update(selector, BSONDocument(
      "$set" -> MaskWriter.write(mask)
    )).map { wr => Some(mask)}
  }

  def update(id: String, name: String, title: String, params: Map[String, String])(implicit user: User): Future[Option[Mask]] = {
    BSONObjectID.parse(id) match {
      case Success(bsonId) =>
        update(bsonId, name, title, params)
      case Failure(e) =>
        Future.successful(None)
    }
  }

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

}

object Document extends Entity[Document] {

  import models.entities.EntityRW._

  override val collection: BSONCollection = MongoConnection.db.collection("documents")

  /**
   * Generate empty Document
   * @return
   */
  def empty() = Document(id = Some(BSONObjectID.generate), None, None, Map.empty, DateTime.now().getMillis, 1, None,
    None, None, None, None, uuid = Some(SecureGen.nextSessionId()))

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
   * Find document by uuid
   * @param uuid
   * @return
   */
  def byId(uuid: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("uuid" -> uuid)).sort(BSONDocument("revision" -> -1)).one[Document]
  }

  /**
   * Find document by ObjectId<id>
   * @param bsonId
   * @return
   */
  def byId(bsonId: BSONObjectID) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("_id" -> bsonId)).one[Document]
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
  def update(doc: Document)(implicit user: User): Future[Option[Document]] = {
    doc.id = Some(BSONObjectID.generate)
    doc.revision = doc.revision.map(_ + 1)
    doc.date = DateTime.now().getMillis
    doc.user = user.id
    insert(doc)
  }

  def update(uuid: String, name: String, params: Map[String, String])(implicit user: User): Future[Option[Document]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    byId(uuid).flatMap {
      case Some(doc) =>
        doc.name = Some(name)
        doc.params = params
        update(doc)
      case None =>
        Future.successful(None)
    }
  }


  /**
   * this method return list of documents with same uuid. this list indicates history of document changing
   */
  def history(uuid: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("uuid" -> uuid)).sort(BSONDocument("revision" -> -1)).cursor[Document].collect[List]()
  }

}

