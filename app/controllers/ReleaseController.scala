package controllers

import models.entities.{Release => rel}
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
//TODO: permission to methods
object ReleaseController extends JsonSerializerController with Secured {

  /**
   * List of all releases within repository
   * @return
   */
  def list(repo: String) = Auth.async() { implicit user => implicit request => >>!(rel list repo)}

  /**
   * Push document to release
   * @return
   */
  def pushToRelease = Auth.async() { implicit user => implicit request => !>>(((__ \ "release").read[String] ~
    (__ \ "doc").read[String])((releaseId, documentId) => rel pushDoc(releaseId, documentId)))
  }

  /**
   *
   * Pop document from release
   * @return
   */
  def popFromRelease = Auth.async() { implicit user => implicit request => !>>(((__ \ "release").read[String] ~
    (__ \ "doc").read[String])((releaseId, documentId) => rel popDoc(releaseId, documentId)))
  }

  /**
   * Create new Release
   * @param repoId
   * @return
   */
  def newRelease(repoId: String) = Auth.async() { implicit user => implicit request => !>>(((__ \ "publishDate").readNullable[Long] ~
    (__ \ "unpublishDate").readNullable[Long] ~ (__ \ "name").read[String])((pd: Option[Long], upd: Option[Long], name: String) => rel gen(repoId, name, pd, upd)))
  }

  /**
   *
   * @param id
   * @return
   */
  def updateRelease(id: String) = Auth.async() { implicit user => implicit request => !>>(((__ \ "publishDate").read[Long] ~
    (__ \ "unpublishDate").read[Long] ~ (__ \ "name").read[String])((pd: Long, upd: Long, name: String) => rel update(id, name, pd, upd)))
  }

  def deleteRelease(id: String) = ???

}
