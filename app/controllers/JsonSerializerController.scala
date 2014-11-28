package controllers

import play.api.libs.json._
import play.api.mvc.{AnyContent, Request, Result, Controller}

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

  /**
   * //todo: Wrap all requests to one pattern
   * //TODO: replace json reads/writes to Json.format
   * //TODO: test shows that parse.tolerantJson will get invalid Json bad request.
   * @param request
   * @tparam T
   */
  def !>>[T, A](reads: Reads[Future[Option[T]]])(implicit request: Request[AnyContent], format: Writes[T],method: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    request.body.asJson.getOrElse(Json.obj()).validate(reads) match {
      case d: JsSuccess[Future[Option[T]]] =>
        d.get.map { doc =>
          ok(Json toJson doc)
        }
      case JsError(e) =>
        futureBad(s"Error parse json. ${e}")
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
