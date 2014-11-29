package controllers

import models.entities.User
import org.joda.time.DateTime
import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import play.api.Logger
import play.api.libs.json.{JsArray, Json}
import play.api.mvc.Cookie
import play.api.test.Helpers._
import play.api.test._

/**
 * Created by artem on 25.11.14.
 */
@RunWith(classOf[JUnitRunner])
class DocumentControllerTest extends Specification {

  "Methods" should {
    val userUUID = "ms289meuc27rqjj50bq3djb8pb"
    var repoId = ""
    var maskId = ""
    var releaseId = ""
    var docId = ""
    "create new repo" in new WithApplication() {
      val name = "Test new Repo"
      val request = FakeRequest("POST", "/repository/new")
        .withCookies(Cookie(User.COOKIE_EMAIL, userUUID, Some(86400)), Cookie(User.COOKIE_AUTH, userUUID))
        .withFormUrlEncodedBody(("name", name))
      val repoResult = controllers.RepositoryController.newRepo(name)(request)
      repoId = contentAsJson(repoResult).\("result").as[JsArray].value(0).\("data").\("id").as[String]
      status(repoResult) must equalTo(200)
    }
    "create new mask" in {
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
      )).withHeaders(("Content-Type", "application/json"))
      val maskResult = controllers.DocumentController.newMask(repoId)(maskRequest)
      maskId = contentAsJson(maskResult).\("result").as[JsArray].value(0).\("data").\("id").as[String]
      status(maskResult) must equalTo(200)
    }
    "create new doc" in new WithApplication()  {
      val docRequest = FakeRequest("POST", s"/mask/$maskId/documents/new")
        .withCookies(Cookie(User.COOKIE_EMAIL, userUUID, Some(86400)), Cookie(User.COOKIE_AUTH, userUUID))
        .withJsonBody(Json.obj(
        "name" -> "test mask name",
        "title" -> "test mask title",
        "params" -> Json.obj(
          "page" -> "text",
          "footer" -> "text",
          "cost" -> "number"
        )
      )).withHeaders("Content-Type" -> "text/javascript")
      val docResult = controllers.DocumentController.gen(maskId)(docRequest)
      Logger.logger.debug(s"Doc result is ${contentAsString(docResult)}")
      docId = contentAsJson(docResult).\("result").as[JsArray].value(0).\("data").\("uuid").as[String]
      docId must !==("")
    }
    "create new release" in new WithApplication(){
      val releaseRequest = FakeRequest("POST", s"/mask/$maskId/documents/releases/new")
        .withCookies(Cookie(User.COOKIE_EMAIL, userUUID, Some(86400)), Cookie(User.COOKIE_AUTH, userUUID))
        .withJsonBody(Json.obj(
        "name" -> "release 2",
        "publishDate" -> DateTime.now().getMillis,
        "unpublishDate" -> DateTime.now().plusDays(2).getMillis
      )).withHeaders(("Content-Type", "text/javascript"))
      val releaseResult = controllers.DocumentController.newRelease(maskId)(releaseRequest)
      releaseId = contentAsJson(releaseResult).\("result").as[JsArray].value(0).\("data").\("id").as[String]
      status(releaseResult) must equalTo(OK)
      releaseId must !==("")
    }
    "add doc to release" in new WithApplication(){
      val addDocToReleaseRequest = FakeRequest("POST", s"/releases/documents/add")
        .withCookies(Cookie(User.COOKIE_EMAIL, userUUID, Some(86400)), Cookie(User.COOKIE_AUTH, userUUID))
        .withJsonBody(Json.obj(
        "release" -> releaseId,
        "doc" -> docId
      )).withHeaders(("Content-Type", "text/javascript"))
      val addDocToReleaseResult = controllers.DocumentController.pushToRelease()(addDocToReleaseRequest)
      Logger.logger.debug(s"Add doc to release ${contentAsString(addDocToReleaseResult)}")
      val newDocId = contentAsJson(addDocToReleaseResult).\("result").as[JsArray].value(0).\("data").\("id").as[String]
      status(addDocToReleaseResult) must equalTo(OK)
      newDocId must !==("")
    }
  }


}
