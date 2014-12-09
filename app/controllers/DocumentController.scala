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

//TODO: 3) favorites 4 attachments
object DocumentController extends JsonSerializerController with Secured {

  /**
   * List of documents by maskId
   * @return
   */
  def list(maskUuid: String) = Accessible(READ)(parse.anyContent) { implicit a => implicit repository => implicit request => >>!(doc.list(maskUuid))}

  /**
   * Get document by uuid
   * @param uuid
   * @return
   */
  def byuuid(uuid: String) = Accessible(READ)(parse.anyContent) { implicit a => implicit repository => implicit request => !>>(doc byUUID uuid)}

  /**
   *
   * @param maskId
   * @return
   */
  def gen(maskId: String) = Accessible(WRITE)(parse.anyContent) { implicit a => implicit repository => implicit request => !>>(((__ \ "name").read[String] ~
    (__ \ "params").read[Map[String, String]] ~ (__ \ "tags").read[List[String]] ~ (__ \ "pd").readNullable[Long] ~
    (__ \ "upd").readNullable[Long])((name: String, params: Map[String, String], tags: List[String], pd: Option[Long],
                                      upd: Option[Long]) => doc gen(maskId, name, params, tags, pd, upd)))
  }

  /**
   *
   * @param uuid
   * @return
   */
  def update(uuid: String) = Accessible(WRITE)(parse.anyContent) { implicit a => implicit repository => implicit request =>  !>>(((__ \ "name").read[String] ~
    (__ \ "params").read[Map[String, String]] ~ (__ \ "tags").read[List[String]] ~ (__ \ "pd").readNullable[Long] ~
    (__ \ "upd").readNullable[Long])((name: String, params: Map[String, String], tags: List[String], pd: Option[Long],
                                      upd: Option[Long]) => doc update(uuid, name, params, tags, pd, upd)))
  }

  /**
   *
   * @param uuid
   * @return
   */
  def delete(uuid: String) = Accessible(WRITE)(parse.anyContent) { implicit a => implicit repository => implicit request =>  !>>(doc del uuid)}

  /**
   * Return history of changes of document
   */
  def history(uuid: String) = Accessible(READ)(parse.anyContent) { implicit a => implicit repository => implicit request =>  >>!(doc history uuid)}

}
