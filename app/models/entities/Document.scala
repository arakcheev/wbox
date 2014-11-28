package models.entities

import models.SecureGen
import models.db.MongoConnection
import org.joda.time.DateTime
import play.api.Logger
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

object Document extends Entity[Document] {

  import models.entities.EntityRW._

  override val collection: BSONCollection = MongoConnection.db.collection("documents")

  /**
   * Generate empty Document
   * @return
   */
  def empty() = Document(id = Some(BSONObjectID.generate), name = None, mask = None, params = Map.empty,
    date = DateTime.now().getMillis, status = 1, publishDate = None, unpublishDate = None, release = None,
    user = None, revision = None, uuid = Some(SecureGen.nextSessionId()))

  /**
   * Generate Document by set of parameters and save it
   * @param maskId
   * @param name
   * @param params
   * @return
   */
  def gen(maskId: String, name: String, params: Map[String, String])(implicit user: User) = {
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
  def byUUID(uuid: String) = {
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
   * Update Document. This updated currently saved doc in DB with new revision
   * Method or update current doc or create new doc with updated fields.
   * @param doc
   * @return
   */
  def update(doc: Document, genNew: Boolean = true)(implicit user: User): Future[Option[Document]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    doc.revision = doc.revision.map(_ + 1)
    doc.date = DateTime.now().getMillis
    doc.user = user.id
    if (genNew) {
      doc.id = Some(BSONObjectID.generate)
      insert(doc)
    } else {
      val _id = doc.id
      doc.id = None
      collection.update(BSONDocument("_id" -> _id), BSONDocument(
        "$set" -> DocumentWriter.write(doc)
      )).map { wr =>
        if (wr.inError) {
          Logger.logger.error(s"Error updating document (${getClass.getName}}) in MongoDB. More info: ${wr.message}")
          None
        } else {
          Some(doc)
        }
      }
    }

  }

  def update(uuid: String, name: String, params: Map[String, String])(implicit user: User): Future[Option[Document]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    byUUID(uuid).flatMap {
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

