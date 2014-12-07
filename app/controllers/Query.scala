package controllers

import models.{QueryAPI => Q}
import models.entities.{Document, Mask}
import play.api.libs.json.Json
import play.api.mvc.{Action, Controller}

import scala.concurrent.Future


/*
 * Copyright 2014(05.12.14) Arakcheev Artem (artem.arakcheev@phystech.edu)
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

object Query extends JsonSerializerController {

  def query(v: Double, repo: String, method: String) = Action.async { implicit request =>
    implicit val isApi = true
    //todo number format exception
    implicit val offset = request.queryString.get("offset").flatMap(_.headOption).map(_.toInt).getOrElse(0)
    implicit val limit = request.queryString.get("limit").flatMap(_.headOption).map(_.toInt)

    method match {
      case "mask.list" =>
        >>!(Q.MaskAPI list repo)
      case "document.list" =>
        >>!(Q.DocumentAPI list request.queryString.get("mask").flatMap(_.headOption))
      case "document.likeField" =>
        val name = request.queryString.get("name").flatMap(_.headOption)
        val value = request.queryString.get("value").flatMap(_.headOption)
        val mask = request.queryString.get("mask").flatMap(_.headOption)
        >>!(Q.DocumentAPI likeField(mask, name, value))
      case "document.byId" =>
        val id = request.queryString.get("id").flatMap(_.headOption)
        >>!(Q.DocumentAPI byId id)
      case _ =>
        futureBad("Method not found")
    }
  }

  /* def query(method: String, repo: String) = Action.async { implicit request =>
     val origin = request.headers.get(ORIGIN).getOrElse("*")
     (method match {
       case "entity.list" =>
         val offset = request.queryString.get("offset").flatMap(_.headOption).getOrElse("0").toInt
         val limit = request.queryString.get("limit").flatMap(_.headOption).getOrElse("10").toInt
         Mask.list(repo).map { xs =>
           ok(Json.toJson(xs.filter(_.repo == repo).slice(offset, limit)))
         }

       case "document.list" =>
         val offset = request.queryString.get("offset").flatMap(_.headOption).getOrElse("0").toInt
         val limit = request.queryString.get("limit").flatMap(_.headOption).getOrElse("10").toInt
         request.queryString.get("entity").flatMap(_.headOption) match {
           case Some(entity) =>
             Document.list(entity).map { xs =>
               ok(Json.toJson(xs.slice(offset, limit)))
             }
           case None => bad(
             "description" -> "Entity id is not set")
         }

       case "document.byField" =>
         val offset = request.queryString.get("offset").flatMap(_.headOption).getOrElse("0").toInt
         val limit = request.queryString.get("limit").flatMap(_.headOption).getOrElse("10").toInt
         val fieldName = request.queryString.get("field").flatMap(_.headOption).getOrElse("")
         val fieldValue = request.queryString.get("value").flatMap(_.headOption).getOrElse("")
         val entity = request.queryString.get("entity").flatMap(_.headOption).getOrElse("")
         Q.DocumentsAPI.documentsByField(entity, fieldName, fieldValue, offset, limit)

       case "document.likeField" =>
         val offset = request.queryString.get("offset").flatMap(_.headOption).getOrElse("0").toInt
         val limit = request.queryString.get("limit").flatMap(_.headOption).getOrElse("10").toInt
         val fieldName = request.queryString.get("field").flatMap(_.headOption).getOrElse("")
         val fieldValue = request.queryString.get("value").flatMap(_.headOption).getOrElse("")
         val entity = request.queryString.get("entity").flatMap(_.headOption).getOrElse("")
         Q.DocumentsAPI.documentsLikeField(entity, fieldName, fieldValue, offset, limit)


       case "document.byId" =>
         val id = request.queryString.get("id").flatMap(_.headOption).getOrElse("")
         val entity = request.queryString.get("entity").flatMap(_.headOption).getOrElse("")
         Q.DocumentsAPI.documentById(entity, id)

       case "document.getFiles" =>
         val docId = request.queryString.get("document").flatMap(_.headOption).getOrElse("")
         val entity = request.queryString.get("entity").flatMap(_.headOption).getOrElse("")
         Q.DocumentsAPI.documentGetFiles(entity, docId)


       case _ => bad("Method not found"
       )
     }).map(_.
       as("application/json").
       withHeaders(("Access-Control-Allow-Origin", origin))
       )
   }*/
}
