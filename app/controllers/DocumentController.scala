package controllers

import models.entities.{Mask, Document}
import play.api.libs.json.{JsString, Json, JsValue, Writes}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

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
  def newMask(repo: String) = Auth.async(parse.tolerantJson) { user => implicit request =>
    implicit val method = "docsNewMask"
    implicit val MaskJson = (
      (__ \ "name").read[String] ~
        (__ \ "title").read[String] ~
        (__ \ "params").read(Reads(js =>
          JsSuccess(js.as[JsArray].value.foldLeft(Map.empty[String, String]) { case item =>
            val name = item._2.\("name").as[String]
            val _type = item._2.\("type").as[String]
            item._1 ++ Map(name -> _type)
          }
          )
        ))
      )((name, title, params) => Mask.empty(name, repo, title, params))

    //TODO: replace json reads/writes to Json.format

    request.body.validate(MaskJson) match {
      case m: JsSuccess[Mask] =>
        Mask.insert(m.get).map { _ =>
          ok(Json.toJson(m.get))
        }
      case JsError(e) =>
        futureBad("error parse json")
    }
  }


  def newDoc(repo: String, mask: String) = Auth.async(parse.tolerantJson) { user => implicit request =>
    implicit val method = "docsNew"
    implicit val DocJson = (
      (__ \ "name").read[String] ~
        (__ \ "params").read(Reads(js =>
          JsSuccess(js.as[JsArray].value.foldLeft(Map.empty[String, String]) { case item =>
            val name = item._2.\("name").as[String]
            val _type = item._2.\("type").as[String]
            item._1 ++ Map(name -> _type)
          }
          )
        ))
      )((name, params) => {
      val doc = Document empty()
      doc.name = name
      doc.params = params
      doc
    })

    request.body.validate(DocJson) match {
      case d: JsSuccess[Document] =>
        Document.insert(d.get).map { _ =>
          ok(Json.toJson(d.get))
        }
      case JsError(e) =>
        futureBad("error parse json")
    }
  }

}
