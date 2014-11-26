package controllers

import models.entities.{User, Release, Document}
import play.api._
import play.api.libs.json.{Json, JsString, JsValue, Writes}
import play.api.mvc._
import reactivemongo.bson.BSONObjectID

import scala.concurrent.ExecutionContext.Implicits.global

object Application extends Controller {
  implicit val bsonIdWrites = new Writes[reactivemongo.bson.BSONObjectID] {
    def writes(bson: BSONObjectID): JsValue = JsString(bson.stringify)
  }

  implicit val documentWriter = Json.writes[Document]

  def index = Action.async {
    User.byId("54664063920000f600b2c23e").flatMap {
      user =>
      Release.pushDoc("5474e0d890000090002a6b99","4qohq01csi4ek3js8n38vdhjk2",user.get).map{d =>
        Ok(Json.toJson(d.get))
      }
    }
  }

}