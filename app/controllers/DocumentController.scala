package controllers

import models.entities.{Document, Mask, Release}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json.{JsString, JsValue, Json, Writes, _}
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

  def list(maskId: String) = Auth.async(parse.anyContent) { user => implicit request =>
    implicit val method = "docsList"
    Document.list(maskId).map { docs =>
      ok(Json.toJson(docs))
    }
  }


  def byId(id: String) = Auth.async(parse.anyContent) { user => implicit request =>
    implicit val method = "docsById"
    Document.byId(id).map { mayBeDoc =>
      ok(Json.toJson(mayBeDoc.map(d => List(d)).getOrElse(Nil)))
    }
  }

  /**
   * TODO: permission to creation repo
   * TODO: field type (number,email,text,id,????)
   * @param repo
   * @return
   */
  def newMask(repo: String) = Auth.async(parse.anyContent) { user => implicit request =>
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

  /**
   *
   * @param maskId
   * @return
   */
  def newDoc(maskId: String) = Auth.async(parse.anyContent) { user => implicit request =>
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

  /**
   * Push document to release
   * @return
   */
  def pushToRelease = Auth.async() {user => implicit request =>
    request.body.asJson.getOrElse(Json.obj()).validate((
      (__ \ "release").read[String] ~
        (__ \ "doc").read[String]
      )((release, document) => {
      Release pushDoc(release,document,user)
    })) match {
      case d: JsSuccess[Future[Document]] =>
        d.get.map { doc =>
          ok(Json.toJson(doc))
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
  def popFromRelease = Auth.async() {user => implicit request =>
    request.body.asJson.getOrElse(Json.obj()).validate((
      (__ \ "release").read[String] ~
        (__ \ "doc").read[String]
      )((release, document) => {
      Release popDoc (release,document,user)
    })) match {
      case d: JsSuccess[Future[Document]] =>
        d.get.map { doc =>
          ok(Json.toJson(doc))
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
  def newRelease(maskId: String) = Auth.async() { user => implicit request =>
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
        Release.gen(maskId, user)
    }).map {
      case Some(release) =>
        ok(Json.writes[Release].writes(release))
      case None =>
        bad("error save new release")
    }

  }

}
