package controllers

import models.entities.{Mask => mask}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

/*
 * Copyright 2014(29.11.14) Arakcheev Artem (artem.arakcheev@phystech.edu)
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

//TODO: field type (number,email,text,id,????)
//TODO: permission to creation mask
object MaskController extends JsonSerializerController with Secured {

  /**
   * Create new Mask
   * @param repo
   * @return
   */
  def gen(repo: String) = Auth.async() { implicit user => implicit request => !>>(((__ \ "name").read[String] ~
    (__ \ "title").read[String] ~ (__ \ "params").read[Map[String, String]])((name: String, title: String, params: Map[String, String]) => mask gen(name, repo, title, params)))
  }

  /**
   * List of all mask in repository
   * @param repo
   * @return
   */
  def list(repo: String) = Auth.async() { implicit user => implicit request => >>!(mask list repo)}

  /**
   * Update mask by ObjectId
   * @param id
   * @return
   */
  def update(id: String) = Auth.async() { implicit user => implicit request => !>>(((__ \ "name").read[String] ~
    (__ \ "title").read[String] ~ (__ \ "params").read[Map[String, String]])((name: String, title: String, params: Map[String, String]) => mask update(id, name, title, params)))
  }

  /**
   * Delete mask by ObjectId
   * @param id
   * @return
   */
  def delete(id: String) = Auth.async() { implicit user => implicit request => !>>(mask del id)}
}
