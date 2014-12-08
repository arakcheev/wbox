package controllers


import models.entities.{Repository, User}
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

/**
 * Helper to wrap requests with authentication
 */
object Auth extends AuthBuilder with JsonSerializerController with Secured

/**
 * Helper to wrap requests with access rule
 */
object Accessible extends AccessBuilder with JsonSerializerController with Secured {

  def apply[A](rule: Int) = access[A](rule) _

}

/**
 * Define Accessible headers
 */
trait AccessHeaders {
  val X_REPOSITORY = "X-Repository"
}

/**
 * Define Accessible headers
 */
object AccessHeaders extends AccessHeaders

/**
 * Define access rules
 */
trait AccessRule {
  val CREATOR = 0

  val WRITE = 1

  val READ = 2

  val NONE = 3

}

/**
 * Define access rules
 */
object AccessRule extends AccessRule

/**
 * Secure params
 */
trait Secured extends AccessHeaders with AccessRule

/** * Standard authenticate methods */
trait AuthBuilder {
  self: JsonSerializerController with Secured =>

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

/** Accessible authenticate methods */
trait AccessBuilder {
  self: JsonSerializerController with Secured =>

  def access[A](rule: Int)(p: BodyParser[A])(
    f: ((User, Repository)) => Request[A] => Future[Result]) = {
    Action.async(p) { request =>
      Repository.byUUID(request.headers.get(X_REPOSITORY)).flatMap { r =>
        User.fromRequest(request).flatMap { u =>
          u.zip(r).headOption match {
            case Some((user, repo)) if repo.getRule(user.uuid) <= rule =>
              f((user, repo))(request)
            case Some((user, repo)) =>
              futureBad("Invalid access")
            case None =>
              futureBad("Not found")
          }
        }
      }
    }
  }

  def write = ???

}
