package models.entities

import java.io.File

import models.SecureGen
import models.db.MongoConnection
import models.services.aws.S3
import org.joda.time.DateTime
import play.api.libs.Files
import play.api.mvc.MultipartFormData
import play.api.mvc.MultipartFormData.FilePart
import play.api.{Play, Logger}
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.{BSONDocument, BSONObjectID}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Created by artem on 29.11.14.
 */
case class Attachment(var id: Option[BSONObjectID], var name: Option[String], var entity: Option[String], var url: Option[String],
                      var status: Option[Int], var revision: Option[Int], var uuid: Option[String], var date: Option[Long],
                      var user: Option[String], var repo: Option[String]) {

}


object Attachment extends Entity[Attachment] {

  import models.entities.EntityRW._

  type TT = Attachment

  override val collection: BSONCollection = MongoConnection.db.collection("attachment")

  val FOLDER = "wbox/files"

  /**
   * Insert new Attachment
   * @return
   */
  def insert(att: Attachment) = {
    save(att, AttachmentWriter)
  }

  /**
   * Generate empty Attachment
   * @return
   */
  def empty() = Attachment(id = Some(BSONObjectID.generate), name = None, entity = None, url = None,
    date = Some(DateTime.now().getMillis), status = Some(1), revision = None,
    user = None, uuid = Some(SecureGen.nextSessionId()), repo = None)

  /**
   * Generate Attachment by set of parameters and save it
   * @param name
   * @return
   */
  def gen(name: String, url: String, entity: String, repo: Option[String])(implicit user: User) = {
    val att = empty()
    att.name = Some(name)
    att.user = user.uuid
    att.repo = repo
    att.url = Some(url)
    att.entity = Some(entity)
    insert(att)
  }

  /**
   * Find attachment by uuid
   * @param uuid
   * @return
   */
  def byUUID(uuid: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("uuid" -> uuid)).sort(BSONDocument("revision" -> -1)).one[TT]
  }

  /**
   * Find attachment by ObjectId<id>
   * @param bsonId
   * @return
   */
  def byId(bsonId: BSONObjectID) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("_id" -> bsonId)).one[TT]
  }

  /**
   * List all attachments in repo
   * @param repoUUId
   * @return
   */
  def list(repoUUId: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("repo" -> repoUUId)).cursor[TT].collect[List]()
  }

  /**
   * List of all attachments to entity
   * @param entity
   * @return
   */
  def byEntity(entity: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("entity" -> entity)).cursor[TT].collect[List]()
  }

  /**
   * Update Document. This updated currently saved doc in DB with new revision
   * Method or update current doc or create new doc with updated fields.
   * @param att
   * @return
   */
  def update(att: Attachment, genNew: Boolean = true, multi: Boolean = false, deleted: Boolean = false)(implicit user: User): Future[Option[Attachment]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    if (!deleted) {
      att.revision = att.revision.map(_ + 1)
    } else {
      att.revision = None
    }
    att.date = Some(DateTime.now().getMillis)
    att.user = user.uuid //todo user of all hierarchy of updated docs
    if (genNew) {
      att.id = Some(BSONObjectID.generate)
      insert(att)
    } else {
      val _id = att.id
      val selector = if (multi) {
        BSONDocument("uuid" -> att.uuid)
      } else {
        BSONDocument("_id" -> _id)
      }
      att.id = None
      collection.update(selector, BSONDocument(
        "$set" -> AttachmentWriter.write(att)
      )).map { wr =>
        if (wr.inError) {
          Logger.logger.error(s"Error updating document (${getClass.getName}}) in MongoDB. More info: ${wr.message}")
          None
        } else {
          Some(att)
        }
      }
    }
  }

  def update(uuid: String, name: String, url: String)(implicit user: User): Future[Option[Attachment]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    byUUID(uuid).flatMap {
      case Some(att) =>
        att.name = Some(name)
        att.url = Some(url)
        update(att)
      case None =>
        Future.successful(None)
    }
  }

  /**
   * Delete attachment
   * If Mode is ''Test'' then delete from DB, otherwise set deleted status
   * @param uuid
   * @param user
   * @return
   */
  def del(uuid: String)(implicit user: User) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    byUUID(uuid).flatMap {
      case Some(att) =>
        import Play.current
        if (Play.isTest) {
          collection.remove(BSONDocument("_id" -> att.id)).map { le =>
            if (le.inError) {
              Logger.logger.error(s"Error deleting document (${getClass.getName}}) in MongoDB. More info: ${le.message}")
              None
            } else {
              Some(att)
            }
          }
        } else {
          att.status = Some(-1)
          update(att, genNew = false, multi = true, deleted = true)
        }
      case None =>
        Future.successful(None)
    }
  }

  /**
   * Put file to S3 and return public URL
   * @param file
   */
  def put(file: Option[MultipartFormData.FilePart[Files.TemporaryFile]]): Future[Option[String]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    file match {
      case Some(FilePart(key, filename, contentType, ref)) =>
        Logger.logger.debug(s"$key")
        S3.put(ref.file, filename, FOLDER).map { case (uuid, result) =>
          Some(uuid)
        }
      case None => Future.successful(None)
    }
  }

}