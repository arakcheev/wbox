package controllers

import models.entities.{Release, Document}
import play.api._
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

}