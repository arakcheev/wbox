package controllers

import models.entities._
import play.api._
import play.api.libs.iteratee.Enumerator
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


  def index = Auth.auth() { implicit user => implicit request =>
    Ok.chunked(
      Enumerator("kiki", "foo", "bar").andThen(Enumerator.eof)
    )

  }

  def login() = Action.async { implicit request =>
    User.byId("54664063920000f600b2c23e").map(_.get).map { implicit user =>
      Ok("").withCookies(Cookie(User.COOKIE_EMAIL, user.uuid.get, Some(86400)), Cookie(User.COOKIE_AUTH, user.uuid.get))
    }
  }

  def login2() = Action.async { implicit request =>
    User.byId("54861e4e1a98fd47a3fc6065").map(_.get).map { implicit user =>
      Ok("").withCookies(Cookie(User.COOKIE_EMAIL, user.uuid.get, Some(86400)), Cookie(User.COOKIE_AUTH, user.uuid.get))
    }
  }
}