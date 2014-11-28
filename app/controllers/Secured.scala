package controllers


import models.entities.User
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/*
 * Copyright 2014(23.11.14) Arakcheev Artem (artem.arakcheev@phystech.edu)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
