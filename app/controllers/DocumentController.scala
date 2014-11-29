package controllers

import models.entities.{Document => doc}
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

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

//TODO: 2) history 3) favorites 4) tags 5)
object DocumentController extends JsonSerializerController with Secured {

  /**
   * List of documents by maskId
   * @param maskId
   * @return
   */
  def list(maskId: String) = Auth.async(parse.anyContent) { implicit user => implicit request => >>!(doc list maskId)}

  /**
   * Get document by uuid
   * @param uuid
   * @return
   */
  def byuuid(uuid: String) = Auth.async(parse.anyContent) { implicit user => implicit request => !>>(doc byUUID uuid)}

  /**
   *
   * @param maskId
   * @return
   */
  def gen(maskId: String) = Auth.async() { implicit user => implicit request => !>>(((__ \ "name").read[String] ~
    (__ \ "params").read[Map[String, String]])((name: String, params: Map[String, String]) => doc gen(maskId, name, params)))
  }

  /**
   *
   * @param uuid
   * @return
   */
  def update(uuid: String) = Auth.async() { implicit user => implicit request => !>>(((__ \ "name").read[String] ~
    (__ \ "params").read[Map[String, String]])((name: String, params: Map[String, String]) => doc update(uuid, name, params)))
  }

  /**
   *
   * @param uuid
   * @return
   */
  def delete(uuid: String) = Auth.async() { implicit user => implicit request => !>>(doc del uuid)}

  /**
   * Return history of changes of document
   */
  def history(uuid: String) = Auth.async() { implicit user => implicit request => !>>(doc history uuid)}

}
