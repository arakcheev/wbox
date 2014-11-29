package models.entities

import models.SecureGen
import models.db.MongoConnection
import play.api.Logger
import play.api.libs.Crypto
import play.api.libs.json.{JsString, Json}
import play.api.mvc.RequestHeader
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson._
import reactivemongo.core.commands.LastError

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Created by artem on 10.11.14.
 */
case class User(var id: Option[BSONObjectID], var uuid: Option[String], var email: Option[String],
                var password: Option[String], var creditCards: Option[BSONArray], var account: Option[BSONDocument],
                var avatar: Option[String], var status: Int, var subUser: Boolean, var users: Option[BSONArray]) {

  def getAccountAsJson = {
    account.map { doc =>
      Json.obj(
        "phone_work" -> JsString(doc.getAs[String]("phone_work").getOrElse("")),
        "phone_home" -> JsString(doc.getAs[String]("phone_home").getOrElse("")),
        "index" -> JsString(doc.getAs[String]("index").getOrElse("")),
        "country" -> JsString(doc.getAs[String]("country").getOrElse("")),
        "region" -> JsString(doc.getAs[String]("region").getOrElse("")),
        "city" -> JsString(doc.getAs[String]("city").getOrElse("")),
        "address" -> JsString(doc.getAs[String]("address").getOrElse("")),
        "company_name" -> JsString(doc.getAs[String]("company_name").getOrElse("")),
        "main_srceen_url" -> JsString(doc.getAs[String]("main_srceen_url").getOrElse(""))
      )
    }.getOrElse(Json.obj())
  }


  def getCreditCardsAsJson = {
    Json.toJson(creditCards.map { array =>
      array.values.map { doc =>
        val bsonDoc = doc.seeAsOpt[BSONDocument].getOrElse(BSONDocument.empty)
        Json.obj(
          "id" -> bsonDoc.getAs[BSONObjectID]("_id").map(_.stringify),
          "status" -> bsonDoc.getAs[Int]("status"),
          "number" -> bsonDoc.getAs[String]("number"),
          "validM" -> bsonDoc.getAs[Int]("validM"),
          "validY" -> bsonDoc.getAs[Int]("validY"),
          "name" -> bsonDoc.getAs[String]("name")
        )
      }.toList
    }.getOrElse(Nil))
  }

}

object User extends Entity[User] {

  import EntityRW._

  override type TT = User

  override val collection: BSONCollection = MongoConnection.db.collection("user")

  val COOKIE_EMAIL = "u_e"

  val COOKIE_AUTH = "u_a"


  //  def registerNewUser(email: String, password: String) = {
  //    byEmail(email).flatMap {
  //      case Some(user) =>
  //        Future.successful(("", Some("User already exists")))
  //      case None =>
  //        val uuid = SecureGen.nextSessionId()
  //        //todo Crypto with shared key
  //        collection.save(User(None, Some(uuid), Some(email), Some(Crypto.encryptAES(password)), None, None, None, 1, false, None)).map { le =>
  //          if (le.inError) {
  //            Logger.logger.error(s"Mongo error: ${le.message}")
  //            (uuid, Some("Internal server error"))
  //          } else {
  //            (uuid, None)
  //          }
  //        }
  //    }
  //  }

  def byEmail(email: String) = {
    collection.find(BSONDocument(
      "email" -> email
    )).one[User]
  }

  def byId(id: String) = {
    collection.find(BSONDocument(
      "_id" -> BSONObjectID(id)
    )).one[User]
  }

  def byUUID(uuid: String) = {
    collection.find(BSONDocument(
      "uuid" -> uuid
    )).one[User]
  }

  /**
   * Private method to authenticate crypto passwrod of user
   * @param user - user model
   * @param password - uncripted password
   * @return Future[Boolean]
   */
  private def auth(user: User, password: String) = {
    //todo Crypto with shared key
    if (user.password.filter(_ == Crypto.encryptAES(password)).isDefined) {
      Some(user)
    } else None

  }

  /**
   * Authenticate user by email and passwrod
   * @param email user email
   * @param password user uncripted password
   * @return
   */
  def authenticate(email: String, password: String) = {
    byEmail(email).map {
      case Some(user) => auth(user, password)
      case None => None
    }
  }

  /**
   * Authenticate user from request and password
   * @param request user request
   * @param password user uncripted password
   * @return
   */
  def authenticate(request: RequestHeader, password: String) = {
    request.cookies.get(COOKIE_EMAIL) match {
      case Some(cookie) =>
        byUUID(cookie.value).map {
          case Some(user) => auth(user, password)
          case None => None
        }
      case None => Future.successful(None)
    }
  }

  /**
   * Get user from request
   * @param request request headers
   * @return
   */
  def fromRequest(request: RequestHeader) = {
    request.cookies.get(COOKIE_AUTH) match {
      case Some(cookie) =>
        //todo user status
        byUUID(cookie.value) //.map(_.filter(_.status > 0))
      case None =>
        Future.successful(None)
    }
  }

  def setAccountValue(id: String, name: String, value: String) = {
    val selector = BSONDocument("_id" -> BSONObjectID(id))
    name match {
      case "phone_work" | "phone_home" | "index" | "country" | "region" | "city" | "address" | "company_name" | "main_srceen_url" =>
        val update = BSONDocument(
          "$set" -> BSONDocument(
            "account." + name -> value.toString
          )
        )
        collection.update(selector, update)

      case _ => Future.successful(LastError.apply(BSONDocument.empty).b)
    }
  }

  //  def uploadFile(user: User, file: File) = {
  //    S3.put(file, file.getName, "console/avatars")
  //  }

  /**
   * Change user password
   * @param user user ot change password
   * @param newPassword
   * @param oldPassword
   * @return Json with boolen result
   */
  def changePassword(user: User, newPassword: String, oldPassword: String) = {
    user.password.filter(_ == Crypto.encryptAES(oldPassword)).map { _ =>
      val selector = BSONDocument("_id" -> user.id.get)
      val update = BSONDocument(
        "$set" -> BSONDocument(
          "password" -> Crypto.encryptAES(newPassword)
        )
      )
      collection.update(selector, update).map { le =>
        if (le.inError) {
          Json.obj("updated" -> false)
        } else {
          Json.obj("updated" -> true)
        }
      }
    }.getOrElse(Future.successful(Json.obj("updated" -> false)))
  }

  /**
   * Add new credit card
   * @param user
   * @param number
   * @param validM
   * @param validY
   * @param name
   * @param cv2
   * @return
   */
  def addCreditCard(user: User, number: String, validM: Int, validY: Int, name: String, cv2: Int) = {
    val selector = BSONDocument("_id" -> user.id.get)
    val update = BSONDocument(
      "$addToSet" -> BSONDocument(
        "creditCards" -> BSONDocument(
          "_id" -> BSONObjectID.generate,
          "status" -> BSONInteger(0),
          "number" -> number,
          "validM" -> BSONInteger(validM),
          "validY" -> BSONInteger(validY),
          "name" -> name,
          "cv2" -> BSONInteger(cv2)
        )
      )
    )
    collection.update(selector, update)
  }

  /**
   * Update credit card
   * @param cardId card id
   * @param number new number
   * @param validM new valid month
   * @param validY new valid year
   * @param name new cardholder name
   * @param cv2 new cv2 code
   * @param status new status of card
   * @return
   */
  def updateCreditCard(cardId: String, number: String, validM: Int, validY: Int, name: String, cv2: Int, status: Int) = {
    val selector = BSONDocument("creditCards" -> BSONDocument("$elemMatch" -> BSONDocument("_id" -> BSONObjectID(cardId))))
    val update = BSONDocument(
      "$set" -> BSONDocument(
        "creditCards.$.status" -> BSONInteger(status),
        "creditCards.$.number" -> number,
        "creditCards.$.validM" -> BSONInteger(validM),
        "creditCards.$.validY" -> BSONInteger(validY),
        "creditCards.$.name" -> name,
        "creditCards.$.cv2" -> BSONInteger(cv2)
      )
    )

    collection.update(selector, update)
  }

}