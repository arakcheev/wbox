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
    implicit val method = "docsNewMask"
    implicit val MaskJson = (
      (__ \ "name").read[String] ~
        (__ \ "title").read[String] ~
        (__ \ "params").read[Map[String, String]]
      )((name, title, params) => Mask.empty(name, repo, title, params))
    //TODO: replace json reads/writes to Json.format
    //TODO: test shows that parse.tolerantJson will get invalid Json bad request.
    request.body.asJson.getOrElse(Json.obj()).validate(MaskJson) match {
      case m: JsSuccess[Mask] =>
        Mask.insert(m.get).map { _ =>
          ok(Json.toJson(m.get))
        }
      case JsError(e) =>
        futureBad(s"error parse json. ${e}")
    }
  }

  def updateMask(id: String) = Auth.async(parse.anyContent) { implicit user => implicit request =>
    implicit val method = "docsMaskEdit"
    implicit val MaskJson = (
      (__ \ "name").read[String] ~
        (__ \ "title").read[String] ~
        (__ \ "params").read[Map[String, String]]
      )((name, title, params) => Mask.update(id, name, title, params))
    //TODO: replace json reads/writes to Json.format
    //TODO: test shows that parse.tolerantJson will get invalid Json bad request.
    request.body.asJson.getOrElse(Json.obj()).validate(MaskJson) match {
      case m: JsSuccess[Future[Option[Mask]]] =>
        m.get.map {
          case Some(mask) =>
            ok(Json.toJson(mask))
          case None =>
            bad("Error update mask")
        }
      case JsError(e) =>
        futureBad(s"error parse json. ${e}")
    }
  }

  /**
   *
   * @param maskId
   * @return
   */
  def newDoc(maskId: String) = Auth.async(parse.anyContent) { implicit user => implicit request =>
    implicit val method = "docsNew"
    request.body.asJson.getOrElse(Json.obj()).validate((
      (__ \ "name").read[String] ~
        (__ \ "params").read[Map[String, String]]
      )((name, params) => {
      Document gen(maskId, name, params, user)
    })) match {
      case d: JsSuccess[Future[Option[Document]]] =>
        d.get.map {
          case Some(doc) =>
            ok(Json.toJson(doc))
          case None =>
            bad("Error save new document")
        }
      case JsError(e) =>
        futureBad("error parse json")
    }
  }

  def updateDoc(uuid: String) = Auth.async(parse.anyContent) {implicit user => implicit request =>
    implicit val method = "docsUpdate"
    request.body.asJson.getOrElse(Json.obj()).validate((
      (__ \ "name").read[String] ~
        (__ \ "params").read[Map[String, String]]
      )((name, params) => {
      Document update(uuid, name, params)
    })) match {
      case d: JsSuccess[Future[Option[Document]]] =>
        d.get.map {
          case Some(doc) =>
            ok(Json.toJson(doc))
          case None =>
            bad("Error update document")
        }
      case JsError(e) =>
        futureBad("error parse json")
    }
  }

  /**
   * //todo: Wrap all requests to one pattern
   * @param request
   * @tparam T
   */
  def !>>[T](implicit request: Request[AnyContent]): Future[Result] = {
    ???
  }

  /**
   * Push document to release
   * @return
   */
  def pushToRelease = Auth.async() { implicit user => implicit request =>
    request.body.asJson.getOrElse(Json.obj()).validate((
      (__ \ "release").read[String] ~
        (__ \ "doc").read[String]
      )((releaseId, documentId) => {
      Release pushDoc(releaseId, documentId)
    })) match {
      case d: JsSuccess[Future[Option[Document]]] =>
        d.get.map { doc =>
          ok(Json toJson doc)
        }
      case JsError(e) =>
        futureBad("error parse json")
    }
  }


  /**
   *
   * Pooping document from release
   * @return
   */
  def popFromRelease = Auth.async() { implicit user => implicit request =>
    request.body.asJson.getOrElse(Json.obj()).validate((
      (__ \ "release").read[String] ~
        (__ \ "doc").read[String]
      )((releaseId, documentId) => {
      Release popDoc(releaseId, documentId)
    })) match {
      case d: JsSuccess[Future[Option[Document]]] =>
        d.get.map { doc =>
          ok(Json toJson doc)
        }
      case JsError(e) =>
        futureBad("error parse json")
    }
  }

  /**
   * Create new Release
   * @param maskId
   * @return
   */
  def newRelease(maskId: String) = Auth.async() { implicit user => implicit request =>
    implicit val method = "releaseNew"
    case class ReleaseJson(publishDate: Long, unpublishDate: Long, name: String) {}
    //TODO: parse request body as tolerantJson
    (request.body.asJson.getOrElse(Json.obj()).validate((
      (__ \ "publishDate").read[Long] ~
        (__ \ "unpublishDate").read[Long] ~
        (__ \ "name").read[String]
      )((pd, upd, name) => ReleaseJson(pd, upd, name))) match {
      case r: JsSuccess[ReleaseJson] =>
        Release.gen(maskId, user, r.get.publishDate, r.get.unpublishDate, r.get.name)
      case JsError(e) =>
        Release gen(maskId, user)
    }).map {
      case Some(release) =>
        ok(Json.toJson(release))
      case None =>
        bad("error save new release")
    }
  }

  //TODO: parse request body as tolerantJson
  def updateRelease(id: String) = Auth.async() { implicit user => implicit request =>
    implicit val method = "releaseUpdate"
    request.body.asJson.getOrElse(Json.obj()).validate((
      (__ \ "publishDate").read[Long] ~
        (__ \ "unpublishDate").read[Long] ~
        (__ \ "name").read[String]
      )((pd, upd, name) =>
      entities.Release.update(id, name, pd, upd, user)
      )) match {
      case r: JsSuccess[Future[Option[Release]]] =>
        r.get.map {
          case Some(release) =>
            ok(Json.toJson(release))
          case None =>
            bad("error updating release")
        }
      case JsError(e) =>
        futureBad("error parse json")
    }
  }

}
