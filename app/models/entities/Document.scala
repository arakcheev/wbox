package models.entities

import models.SecureGen
import models.db.MongoConnection
import org.joda.time.DateTime
import play.api.{Play, Logger}
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
                    var user: Option[BSONObjectID], var revision: Option[Int], var uuid: Option[String], var tags: Option[List[String]]) {

}

object Document extends Entity[Document] {



  import models.entities.EntityRW._

  override type TT = Document

  override val collection: BSONCollection = MongoConnection.db.collection("documents")

  /**
   * Generate empty Document
   * @return
   */
  def empty() = Document(id = Some(BSONObjectID.generate), name = None, mask = None, params = Map.empty,
    date = DateTime.now().getMillis, status = 1, publishDate = None, unpublishDate = None, release = None,
    user = None, revision = None, uuid = Some(SecureGen.nextSessionId()), tags = None)

  /**
   * Generate Document by set of parameters and save it
   * @param maskId
   * @param name
   * @param params
   * @return
   */
  def gen(maskId: String, name: String, params: Map[String, String], tags: List[String], pd: Option[Long],
          upd: Option[Long])(implicit user: User) = {
    val doc = empty()
    BSONObjectID.parse(maskId).map { id =>
      doc.mask = Some(id)
      doc.name = Some(name)
      doc.params = params
      doc.user = user.id
      doc.tags = Some(tags)
      doc.publishDate = pd
      doc.unpublishDate = upd
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
    collection.find(BSONDocument("mask" -> BSONObjectID(maskId))).cursor[Document].collect[List]()
  }

  /**
   * Update Document. This updated currently saved doc in DB with new revision
   * Method or update current doc or create new doc with updated fields.
   * @param doc
   * @return
   */
  def update(doc: Document, genNew: Boolean = true, multi: Boolean = false, deleted: Boolean = false)(implicit user: User): Future[Option[Document]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    if (!deleted) {
      doc.revision = doc.revision.map(_ + 1)
    } else {
      doc.revision = None
    }
    doc.date = DateTime.now().getMillis
    doc.user = user.id
    if (genNew) {
      doc.id = Some(BSONObjectID.generate)
      insert(doc)
    } else {
      val _id = doc.id
      val selector = if (multi) {
        BSONDocument("uuid" -> doc.uuid)
      } else {
        BSONDocument("_id" -> _id)
      }
      doc.id = None
      collection.update(selector, BSONDocument(
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

  def update(uuid: String, name: String, params: Map[String, String], tags: List[String], pd: Option[Long],
             upd: Option[Long])(implicit user: User): Future[Option[Document]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    byUUID(uuid).flatMap {
      case Some(doc) =>
        doc.name = Some(name)
        doc.params = params
        doc.tags = Some(tags)
        doc.publishDate = pd
        doc.unpublishDate = upd
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

  /**
   * Delete documents
   * If Mode is ''Test'' then delete from DB, otherwise set status
   * @param uuid
   * @param user
   * @return
   */
  def del(uuid: String)(implicit user: User) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    byUUID(uuid).flatMap {
      case Some(doc) =>
        import Play.current
        if (Play.isTest) {
          collection.remove(BSONDocument("_id" -> doc.id)).map { le =>
            if (le.inError) {
              Logger.logger.error(s"Error deleting document (${getClass.getName}}) in MongoDB. More info: ${le.message}")
              None
            } else {
              Some(doc)
            }
          }
        } else {
          doc.status = -1
          update(doc, genNew = false, multi = true, deleted = true)
        }
      case None =>
        Future.successful(None)
    }
  }

}

