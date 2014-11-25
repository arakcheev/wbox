package controllers

import models.entities.User
import org.joda.time.DateTime
import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import play.api.{Logger, Application, GlobalSettings}
import play.api.libs.json.{JsArray, Json}
import play.api.mvc.Cookie
import play.api.test._
import play.api.test.Helpers._

/**
 * Created by artem on 25.11.14.
 */
@RunWith(classOf[JUnitRunner])
class DocumentControllerTest extends Specification {

  "Methods" should {
    val userUUID = "ms289meuc27rqjj50bq3djb8pb"
    var repoId = ""
    var maskId = ""
    "create new repo" in new WithApplication() {
      val name = "Test new Repo"
      val request = FakeRequest("POST", "/repository/new")
        .withCookies(Cookie(User.COOKIE_EMAIL, userUUID, Some(86400)), Cookie(User.COOKIE_AUTH, userUUID))
        .withFormUrlEncodedBody(("name", name))
      val repoResult = controllers.RepositoryController.newRepo(name)(request)
      status(repoResult) must equalTo(200)
      repoId = contentAsJson(repoResult).\("result").as[JsArray].value(0).\("data").\("id").as[String]
    }
    "create new mask" in new WithApplication() {
      val maskRequest = FakeRequest("POST", "/mask/new")
        .withCookies(Cookie(User.COOKIE_EMAIL, userUUID, Some(86400)), Cookie(User.COOKIE_AUTH, userUUID))
        .withJsonBody(Json.obj(
        "name" -> "test mask name",
        "title" -> "test mask title",
        "params" -> Json.obj(
          "page" -> "text",
          "footer" -> "text",
          "cost" -> "number"
        )
      )).withHeaders(("Content-Type", "text/javascript"))
      val maskResult = controllers.DocumentController.newMask(repoId)(maskRequest)
      Logger.logger.debug(s"${maskRequest.headers}" + contentAsString(maskResult))
      status(maskResult) must equalTo(200)
      maskId = contentAsJson(maskResult).\("result").as[JsArray].value(0).\("data").\("id").as[String]
    }
    "respond to the newRelease Action" in {
      val releaseRequest = FakeRequest("POST", s"/mask/$maskId/documents/releases/new")
        .withCookies(Cookie(User.COOKIE_EMAIL, userUUID, Some(86400)), Cookie(User.COOKIE_AUTH, userUUID))
        .withJsonBody(Json.obj(
        "name" -> "release 1",
        "publishDate" -> DateTime.now().getMillis,
        "unpublishDate" -> DateTime.now().plusDays(1).getMillis
      )).withHeaders(("Content-Type", "text/javascript"))
      val releaseResult = controllers.DocumentController.newRelease(maskId)(releaseRequest).run
      status(releaseResult) must equalTo(OK)
    }
  }


}
