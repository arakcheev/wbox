package controllers

import models.entities.Repository
import play.api.libs.json.{JsString, Json, JsValue, Writes}
import play.api.mvc.{Action, Controller}

/**
 * Created by artem on 23.11.14.
 */
object RepositoryController extends JsonSerializerController with Secured {

  import scala.concurrent.ExecutionContext.Implicits.global

  implicit val writes = new Writes[Repository] {
    def writes(repo: Repository): JsValue = Json.obj(
      "id" -> JsString(repo.id.map(_.stringify).getOrElse("")),
      "name" -> repo.name,
      "status" -> repo.status,
      "user" -> repo.user
    )
  }

  def list = Auth.async(parse.anyContent) { user => implicit request =>
    implicit val method = "repoList"
    Repository.list(user).map { repos =>
      ok(Json.toJson(repos))
    }.recover(recover)
  }

  def newRepo(name: String) = Auth.async(parse.anyContent) { user => implicit request =>
    implicit val method = "repoCreate"
    val repo = Repository(name, user)
    Repository.save(repo).map { _ =>
      ok(Json.toJson(repo))
    }
  }


}
