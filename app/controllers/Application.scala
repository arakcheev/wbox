package controllers

import models.entities._
import play.api._
import play.api.libs.json.{Json, JsString, JsValue, Writes}
import play.api.mvc._
import reactivemongo.bson.BSONObjectID

import scala.concurrent.ExecutionContext.Implicits.global

object Application extends JsonSerializerController with Secured {
//  implicit val bsonIdWrites = new Writes[reactivemongo.bson.BSONObjectID] {
//    def writes(bson: BSONObjectID): JsValue = JsString(bson.stringify)
//  }

//  implicit val documentWriter = Json.writes[Mask]

//  implicit val documentWriter2 = Json.writes[Document]


  def index = Auth.async(){ implicit user => implicit request =>
    Repository.del("5478e9b9afc633960287548f").map { s =>
      Ok(s+"")
    }
  }

  def login() = Action.async { implicit request =>
    User.byId("54664063920000f600b2c23e").map(_.get).map { implicit user =>
      Ok("").withCookies(Cookie(User.COOKIE_EMAIL, user.uuid.get, Some(86400)), Cookie(User.COOKIE_AUTH, user.uuid.get))
    }
  }

}