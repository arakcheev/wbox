package controllers

import models.entities.Document
import play.api._
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global

object Application extends Controller {

  def index = Action {
    val doc = Document.empty
    doc.name = "New doc"
    Document.insert(doc).map{mayBeDoc =>
      println(mayBeDoc)
    }
    Ok(views.html.index("Your new application is ready."))
  }

}