package controllers

import models.entities.{Repository => repo}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent}

import scala.language.higherKinds

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
object RepositoryController extends JsonSerializerController with Secured {

  /**
   * List of user repositories
   * @return
   */
  def list = Auth.async() { implicit user => implicit request => >>!(repo list)}

  /**
   * Create new repository with name ${name}
   * @param name
   * @return
   */
  def newRepo(name: String) = Auth.async() { implicit user => implicit request => !>>(repo gen name)}

  /**
   * Delete repository by uuid
   * @return
   */
  def delete = Accessible[AnyContent](WRITE)(parse.anyContent) { implicit a => implicit request => !>>(repo.del(a._2.uuid)(a._1))}

  // Auth.async() { implicit user => implicit request => !>>(repo del uuid)}

  /**
   * Update repository with Json params
   * @return
   */
  def update = Accessible[AnyContent](WRITE)(parse.anyContent) { implicit a => implicit request => !>>(((__ \ "name").read[String] ~
    (__ \ "ts").readNullable[Long] /*//todo: read not applied to single field ???*/)((name: String, ts: Option[Long]) => repo.update(a._2.uuid, name)(a._1)))
  }

  /**
   * Add access to repository( e.g. invite to repository)
   * @return
   */
  def invite = Accessible[AnyContent](CREATOR)(parse.anyContent) { implicit a => implicit request => !>>(((__ \ "to").read[String] ~
    (__ \ "rule").read[Int](verifying[Int](rule => rule > CREATOR)))((to: String, rule: Int) => repo.addAccess(a._2.uuid, rule, to)(a._1)))
  }

}
