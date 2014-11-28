package controllers

import models.entities
import models.entities.{Document, Mask, Release}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{JsString, JsValue, Json, Writes, _}
import play.api.mvc.{AnyContent, Request}
import play.mvc.Result
import reactivemongo.bson.BSONObjectID

import scala.concurrent.Future

/**
 * Created by artem on 23.11.14.
 *
 * TODO:
 * 1) delete docs
 * 2) edit docs
 * 3) save new docs
 * 4) edit mask
 * 5) delete mask
 * 6) releases
 * 7) ???
 */
object DocumentController extends JsonSerializerController with Secured {

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val bsonIdWrites = new Writes[reactivemongo.bson.BSONObjectID] {
    def writes(bson: BSONObjectID): JsValue = JsString(bson.stringify)
  }

  implicit val documentWriter = Json.writes[Document]

  implicit val releaseWriter = Json.writes[Release]

  @deprecated("Use macros", "25.11.14")
  implicit val writes = new Writes[Document] {
    def writes(doc: Document): JsValue = Json.obj(
      "id" -> JsString(doc.id.map(_.stringify).getOrElse("")),
      "name" -> doc.name,
      "status" -> doc.status,
      "params" -> doc.params,
      "mask" -> doc.mask,
      "date" -> doc.date
    )
  }

  @deprecated("Use macros", "25.11.14")
  implicit val maskWrites = new Writes[Mask] {
    def writes(doc: Mask): JsValue = Json.obj(
      "id" -> JsString(doc.id.map(_.stringify).getOrElse("")),
      "name" -> doc.name,
      "status" -> doc.status,
      "params" -> doc.params,
      "title" -> doc.title,
      "repo" -> doc.repo
    )
  }

  /**
   * List of documents by maskId
   * @param maskId
   * @return
   */
  def list(maskId: String) = Auth.async(parse.anyContent) { user => implicit request =>
    implicit val method = "docsList"
    Document.list(maskId).map { docs =>
      ok(Json.toJson(docs))
    }
  }


  /**
   * Get document by uuid
   * @param uuid
   * @return
   */
  def byId(uuid: String) = Auth.async(parse.anyContent) { user => implicit request =>
    implicit val method = "docsByUUID"
    Document.byId(uuid).map { mayBeDoc =>
      ok(Json.toJson(mayBeDoc.map(d => List(d)).getOrElse(Nil)))
    }
  }

  /**
   * TODO: permission to creation repo
   * TODO: field type (number,email,text,id,????)
   * @param repo
   * @return
   */
  def newMask(repo: String) = Auth.async(parse.anyContent) { implicit user => implicit request =>
    implicit val method = "newMask"
    !>>((
      (__ \ "name").read[String] ~
        (__ \ "title").read[String] ~
        (__ \ "params").read[Map[String, String]]
      )((name: String, title: String, params: Map[String, String]) =>
      Mask gen(name, repo, title, params)
      ))
  }

  def updateMask(id: String) = Auth.async(parse.anyContent) { implicit user => implicit request =>
    implicit val method = "maskEdit"
    !>>((
      (__ \ "name").read[String] ~
        (__ \ "title").read[String] ~
        (__ \ "params").read[Map[String, String]]
      )((name: String, title: String, params: Map[String, String]) =>
      Mask update(id, name, title, params)
      ))
  }

  /**
   *
   * @param maskId
   * @return
   */
  def newDoc(maskId: String) = Auth.async(parse.anyContent) { implicit user => implicit request =>
    implicit val method = "docsNew"
    !>>((
      (__ \ "name").read[String] ~
        (__ \ "params").read[Map[String, String]]
      )((name: String, params: Map[String, String]) => {
      Document gen(maskId, name, params)
    }))
  }

  def updateDoc(uuid: String) = Auth.async(parse.anyContent) { implicit user => implicit request =>
    implicit val method = "docsUpdate"
    !>>((
      (__ \ "name").read[String] ~
        (__ \ "params").read[Map[String, String]]
      )((name: String, params: Map[String, String]) => {
      Document update(uuid, name, params)
    }))
  }


  /**
   * Push document to release
   * @return
   */
  def pushToRelease = Auth.async() { implicit user => implicit request =>
    implicit val method = "docsPushToRelease"
    !>>((
      (__ \ "release").read[String] ~
        (__ \ "doc").read[String]
      )((releaseId, documentId) =>
      Release pushDoc(releaseId, documentId)
      ))
  }


  /**
   *
   * Pooping document from release
   * @return
   */
  def popFromRelease = Auth.async() { implicit user => implicit request =>
    implicit val method = "docsPopToRelease"
    !>>((
      (__ \ "release").read[String] ~
        (__ \ "doc").read[String]
      )((releaseId, documentId) =>
      Release popDoc(releaseId, documentId)
      ))
  }

  /**
   * Create new Release
   * @param maskId
   * @return
   */
  def newRelease(maskId: String) = Auth.async() { implicit user => implicit request =>
    implicit val method = "releaseNew"
    !>>((
      (__ \ "publishDate").readNullable[Long] ~
        (__ \ "unpublishDate").readNullable[Long] ~
        (__ \ "name").read[String]
      )((pd: Option[Long], upd: Option[Long], name: String) =>
      Release gen(maskId, name, pd, upd)))
  }

  def updateRelease(id: String) = Auth.async() { implicit user => implicit request =>
    implicit val method = "releaseUpdate"
    !>>((
      (__ \ "publishDate").read[Long] ~
        (__ \ "unpublishDate").read[Long] ~
        (__ \ "name").read[String]
      )((pd: Long, upd: Long, name: String) =>
      Release update(id, name, pd, upd)
      ))
  }

}
