package models.entities

import models.SecureGen
import models.db.MongoConnection
import org.joda.time.DateTime
import play.api.Logger
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.{BSONDocument, BSONObjectID}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Created by artem on 25.11.14.
 */
case class Release(var id: Option[BSONObjectID], var name: Option[String], var publishDate: Option[Long],
                   var unpublishDate: Option[Long], var repo: Option[BSONObjectID], var user: Option[BSONObjectID],
                   var uuid: Option[String], var revision: Option[Int], var date: Option[Long]) {

}

object Release extends Entity[Release] {

  import EntityRW._

  override type TT = Release

  override val collection: BSONCollection = MongoConnection.db.collection("releases")

  /**
   * Generate empty Release
   * @return
   */
  def empty(): Release = Release(id = Some(BSONObjectID.generate), name = None, publishDate = None, unpublishDate = None,
    repo = None, user = None, uuid = Some(SecureGen.nextSessionId()), revision = None, date = Some(DateTime.now().getMillis))

  /**
   * Generate new release by params
   *
   * @param maskId
   * @param user
   * @return
   */
  @deprecated("use gen(repoId: String, name: String, pd: Option[Long] = None, upd: Option[Long] = None)" +
    "(implicit user: User) instead", "29.11.14")
  def gen(maskId: String, user: User): Future[Option[Release]] = {
    val r = empty()
    BSONObjectID.parse(maskId).map { id =>
      r.repo = Some(id)
      r.user = user.id
      r
    } match {
      case Success(r) =>
        insert(r)
      case Failure(e) =>
        Future.failed(e)
    }
  }

  /**
   * Generate new release by params
   * @param maskId
   * @param user
   * @param pd
   * @param upd
   * @param name
   * @return
   */
  def gen(repoId: String, name: String, pd: Option[Long] = None, upd: Option[Long] = None)(implicit user: User): Future[Option[Release]] = {
    val r = empty()
    BSONObjectID.parse(repoId).map { id =>
      r.repo = Some(id)
      r.user = user.id
      r.publishDate = pd
      r.unpublishDate = upd
      r.name = Some(name)
      r
    } match {
      case Success(r) =>
        insert(r)
      case Failure(e) =>
        Future.failed(e)
    }
  }

  /**
   * Insert new relese
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
   * List all releases with the same mask id
   * @param repoId
   * @return
   */
  def list(repoId: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    BSONObjectID.parse(repoId) match {
      case Success(bsonId) =>
        collection.find(BSONDocument("repo" -> bsonId)).cursor[Release].collect[List]()
      case Failure(e) =>
        Future.successful(Nil)
    }
  }

  /**
   * List of all user releases
   * @param user
   * @return
   */
  def list(implicit user: User) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("user" -> user.id)).cursor[Release].collect[List]()
  }

  /**
   * Add document to release
   * @param releaseId
   * @param docUUID
   * @param user
   * @return
   */
  def pushDoc(releaseId: String, docUUID: String)(implicit user: User) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    byId(releaseId).flatMap { release =>
      Document byUUID docUUID flatMap { document =>
        document.zip(release).headOption.map { case (d, r) =>
          d.release = r.id
          d.name = d.name
          d.publishDate = r.publishDate
          d.unpublishDate = r.unpublishDate
          Document update d
        }.getOrElse(Future(None))
      }
    }
  }

  /**
   * Pop document from release
   * @param releaseId
   * @param docUUID
   * @param user
   * @return
   */
  //todo: publish and unpublish date of documents after pop from release
  def popDoc(releaseId: String, docUUID: String)(implicit user: User) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    byId(releaseId).flatMap { release =>
      Document byUUID docUUID flatMap { document =>
        document.zip(release).headOption.map { case (d, r) =>
          d.release = None
          d.publishDate = None
          d.unpublishDate = None
          Document update d
        }.getOrElse(Future(None))
      }
    }
  }

  /**
   * Update release.
   * Method or update current mask or create new mask with updated fields.
   * @param rel
   * @param user
   * @return
   */
  def update(rel: Release, genNew: Boolean = true)(implicit user: User) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    rel.user = user.id
    rel.revision = rel.revision.map(_ + 1)
    rel.date = Some(DateTime.now().getMillis)
    if (genNew) {
      rel.id = Some(BSONObjectID.generate)
      insert(rel)
    } else {
      val _id = rel.id
      rel.id = None
      collection.update(BSONDocument("_id" -> _id), BSONDocument(
        "$set" -> ReleaseWriter.write(rel)
      )).map { wr =>
        if (wr.inError) {
          Logger.logger.error(s"Error updating document (${getClass.getName}}) in MongoDB. More info: ${wr.message}")
          None
        } else {
          Some(rel)
        }
      }
    }
  }

  /**
   *
   * @param id
   * @param name
   * @param pd
   * @param upd
   * @param user
   * @return
   */
  def update(id: String, name: String, pd: Long, upd: Long)(implicit user: User): Future[Option[Release]] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    byId(id).flatMap {
      case Some(rel) =>
        rel.name = Some(name)
        rel.publishDate = Some(pd)
        rel.unpublishDate = Some(upd)
        update(rel)
      case None =>
        Future.successful(None)
    }
  }


}