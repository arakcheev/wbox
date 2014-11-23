package models.db

import models.Configuration
import reactivemongo.api.collections.bson.BSONCollection

/**
 * Created by artem on 23.11.14.
 */
object MongoConnection {

  import reactivemongo.api._
  import scala.concurrent.ExecutionContext.Implicits.global

  // gets an instance of the driver
  // (creates an actor system)
  val driver = new MongoDriver
  val connection = driver.connection(List(Configuration.mongo.host))

  // Gets a reference to the database "plugin"
  val db = connection("bf")

}

trait MongoDB{

  val collection: BSONCollection

}
