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


  def index = Action.async { implicit request =>
    User.byId("54664063920000f600b2c23e").map(_.get).flatMap { implicit user =>
      DocumentController.newMask("5474c80dafc633720194e801").apply(request).map { r =>
        r
      }
    }
  }

  def login() = Action.async { implicit request =>
    User.byId("54664063920000f600b2c23e").map(_.get).map { implicit user =>
      Ok("").withCookies(Cookie(User.COOKIE_EMAIL, user.uuid.get, Some(86400)), Cookie(User.COOKIE_AUTH, user.uuid.get))
    }
  }

}