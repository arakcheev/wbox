package controllers


import models.entities.User
import play.api.libs.json._
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Created by artem on 23.11.14.
 */
trait Secured {
  self: JsonSerializerController =>

  trait AuthBuilder {
    self =>

    def auth[A](p: BodyParser[A] = parse.anyContent)(
      f: User => Request[A] => Result): Action[A] = {
      Action.async(p) { implicit request =>
        User.fromRequest(request).map {
          case Some(user) =>
            f(user)(request)
          case None => bad("Invalid Token cookie")
        }.recover(recover)
      }
    }

    def async[A](p: BodyParser[A] = parse.anyContent)(
      f: User => Request[A] => Future[Result]): Action[A] = {
      Action.async(p) { implicit request =>
        User.fromRequest(request).flatMap {
          case Some(user) =>
            f(user)(request).recover(recover)
          case None => futureBad("Invalid Token cookie")
        }.recover(recover)
      }
    }

  }

  object Auth extends AuthBuilder {

  }

}
