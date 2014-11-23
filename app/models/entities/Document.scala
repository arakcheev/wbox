package models.entities

import models.db.MongoConnection
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.{BSONDocument, BSONObjectID}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/**
 * Created by artem on 23.11.14.
 */
case class Document(id: Option[BSONObjectID], name: String, mask: String,
                    params: Map[String, String], date: Long, publish: Boolean = false, status: Int) {

}

case class Mask(id: Option[BSONObjectID], var name: String, var title: String,
                var params: Map[String, String], repo: String, status: Int)

object Mask extends Entity[Document] {

  import EntityRW._
  import EntityRW.mask._

  override val collection: BSONCollection = MongoConnection.db.collection("mask")

  def empty(name: String, repo: String, title: String, params: Map[String, String]) ={
    Mask(Some(BSONObjectID.generate), name, title, params, repo, 1)
  }

  def newMask(name: String, repo: String, title: String, params: Map[String, String]) = {
    val mask = Mask(Some(BSONObjectID.generate), name, title, params, repo, 1)
    save(mask)
  }

  def save(mask: Mask) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.insert(mask)
  }

  def byId(id: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    BSONObjectID.parse(id) match {
      case Success(bsonId) =>
        collection.find(BSONDocument("_id" -> bsonId)).one[Mask]
      case Failure(e) =>
        Future.successful(None)
    }
  }

  def list(repo: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("repo" -> repo)).cursor[Mask].collect[List]()
  }

}

object Document extends Entity[Document] {

  import EntityRW._
  import EntityRW.document._

  override val collection: BSONCollection = MongoConnection.db.collection("documents")


  def save(doc: Document) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.insert(doc)
  }

  def byId(id: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    BSONObjectID.parse(id) match {
      case Success(bsonId) =>
        collection.find(BSONDocument("_id" -> bsonId)).one[Document]
      case Failure(e) =>
        Future.successful(None)
    }
  }

  def list(maskId: String) = {
    import scala.concurrent.ExecutionContext.Implicits.global
    collection.find(BSONDocument("mask" -> maskId)).cursor[Document].collect[List]()
  }

}

