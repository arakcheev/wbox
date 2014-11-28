package models.entities

import models.db.MongoConnection
import org.joda.time.DateTime
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.{BSONDocument, BSONObjectID}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Created by artem on 25.11.14.
 */
case class Release(var id: Option[BSONObjectID], var name: Option[String], var publishDate: Option[Long],
                   var unpublishDate: Option[Long],
                   var mask: Option[BSONObjectID], var user: Option[BSONObjectID]) {

}

object Release extends Entity[Release] {

  import EntityRW._

  override val collection: BSONCollection = MongoConnection.db.collection("releases")

  /**
   * Generate empty Release
   * @return
   */
  def empty(): Release = Release(Some(BSONObjectID.generate), None, None, None, None, None)

  /**
   * Generate new release by params
   *
   * //todo this method saves release. Need some wrapper with Release method to move matching functionality from controllers
   * @param maskId
   * @param user
   * @return
   */
  def gen(maskId: String, user: User): Future[Option[Release]] = {
    val r = Release.empty
    BSONObjectID.parse(maskId).map { id =>
      r.mask = Some(id)
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
  def gen(maskId: String, user: User, pd: Long, upd: Long, name: String): Future[Option[Release]] = {
    val r = Release.empty
    BSONObjectID.parse(maskId).map { id =>
      r.mask = Some(id)
      r.user = user.id
      r.publishDate = Some(pd)
      r.unpublishDate = Some(upd)
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

  /**
   * List of all user releases
   * @param user
   * @return
   */
  def list(user: User) = {
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
      Document byId docUUID flatMap { document =>
        document.zip(release).headOption.map { case (d, r) =>
          d.release = r.id
          d.name = d.name.map(_ + d.revision)
          d.publishDate = r.publishDate
          d.unpublishDate = r.unpublishDate
          Document update d
        }.getOrElse(Future(None))
      }
    }
  }

  /**
   * Pop document from release
   * //TODO: publish and unpublish date of documents after pop from release
   * @param releaseId
   * @param docUUID
   * @param user
   * @return
   */
  def popDoc(releaseId: String, docUUID: String)(implicit user: User) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    byId(releaseId).flatMap { release =>
      Document byId docUUID flatMap { document =>
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
   * Update release. Update - is create new release in DB.?????
   * @param rel
   * @param user
   * @return
   */
  def update(rel: Release, user: User) = {
    rel.user = user.id
    insert(rel)
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
  def update(id: String,name: String,pd: Long,upd: Long,user: User): Future[Option[Release]]={
    import scala.concurrent.ExecutionContext.Implicits.global
    byId(id).flatMap{
      case Some(rel) =>
        rel.name = Some(name)
        rel.publishDate = Some(pd)
        rel.unpublishDate = Some(upd)
        update(rel,user)
      case None =>
        Future.successful(None)
    }
  }


}