package controllers

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import models.entities.{Attachment => att}

/**
 * Created by artem on 29.11.14.
 */
object AttachmentController extends JsonSerializerController with Secured {


  /**
   * Generate new attachment
   * uuid of repository
   * uuid of entity
   * @return
   */
  def gen = Auth.async() { implicit user => implicit request => !>>(((__ \ "name").read[String] ~
    (__ \ "url").read[String] ~ (__ \ "entity").read[String] ~ (__ \ "repo").read[String])((name: String, url: String, entity: String, repo: String) => att gen(name, url, entity, repo)))
  }

  /**
   * List of attachments
   * @param repo - uuid of repository
   * @return
   */
  def list(repo: String) = Auth.async(parse.anyContent) { implicit user => implicit request => >>!(att list repo)}

  /**
   * List entities attachments
   * @param entity - uuid of entity
   * @return
   */
  def byEntity(entity: String) = Auth.async(parse.anyContent) { implicit user => implicit request => >>!(att byEntity entity)}

  /**
   * Update attachment by uuid
   * @return
   */
  def update = Auth.async() { implicit user => implicit request => !>>(((__ \ "uuid").read[String] ~ (__ \ "name").read[String] ~
    (__ \ "url").read[String])((uuid: String, name: String, url: String) => att update(uuid, name, url)))
  }

  /**
   * Delete attachment by uuid
   * @param uuid - uuid of attachment
   * @return
   */
  def del(uuid: String) = Auth.async() { implicit user => implicit request => !>>(att del uuid)}
}
