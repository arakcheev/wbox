package controllers

import play.api.libs.json.{JsValue, Json}
import play.api.mvc.{Result, Controller}

import scala.concurrent.Future

/**
 * Created by artem on 23.11.14.
 */
trait JsonSerializerController extends Controller {

  import play.api.Play.current
  import play.api.http.ContentTypes


  def recover: PartialFunction[Throwable, Result] = {
    case e: Exception =>
      if (play.api.Play.isDev) {
        InternalServerError(e.getMessage)
      } else {
        bad("Internal server error")
      }
  }


  def info(method: String, status: Int) = Json.obj("status" -> status, "method" -> method)

  def errors(error: String) = Json.obj("code" -> 1, "message" -> error)

  def result(data: JsValue = Json.obj(), info: JsValue = Json.obj(), errors: JsValue = Json.obj()) = Json.obj("result" -> Json.arr(
    Json.obj(
      "data" -> data
    ), info,
    errors
  ))

  def ok(js: JsValue)(implicit method: String = "") = {
    new Status(200).apply(result(js, info(method, 200))).as(ContentTypes.JSON)
  }

  def futureOk(js: JsValue)(implicit method: String = "") = {
    Future.successful(ok(js)(method))
  }

  def bad(cause: String)(implicit method: String = "") = {
    new Status(400).apply(result(info = info(method, 400), errors = errors(cause))).as(ContentTypes.JSON)
  }

  def futureBad(cause: String)(implicit method: String = "") = {
    Future.successful(bad(cause)(method))
  }

  def redirect(implicit method: String = "") = {
    new Status(300).apply(result(info = info(method, 300))).as(ContentTypes.JSON)
  }

  def futureRedirect(implicit method: String = "") = Future.successful(redirect)
}
