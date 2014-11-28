package controllers

import models.entities.{Mask, User, Release, Document}
import play.api._
import play.api.libs.json.{Json, JsString, JsValue, Writes}
import play.api.mvc._
import reactivemongo.bson.BSONObjectID

import scala.concurrent.ExecutionContext.Implicits.global

object Application extends Controller {
  implicit val bsonIdWrites = new Writes[reactivemongo.bson.BSONObjectID] {
    def writes(bson: BSONObjectID): JsValue = JsString(bson.stringify)
  }

  implicit val documentWriter = Json.writes[Mask]

  implicit val documentWriter2 = Json.writes[Document]


  def index = Action.async {
    User.byId("54664063920000f600b2c23e").map(_.get).flatMap { implicit user =>
      Document.update("qrsn3k32qp6paml76jetqesm1v","Doc2",Map("footer" -> "text 2")).map{d =>
          Ok(Json.toJson(d.get))
        }
    }
  }

}