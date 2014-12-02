package models

import play.api.Play

/**
 * Created by artem on 23.11.14.
 */
object Configuration {

  import Play.current

  object mongo {

    def host = Play.configuration.getString("mongo.host").getOrElse(sys.error("Mongo host not set"))
  }


  object aws {

    val id = Play.configuration.getString("aws.key.id").getOrElse(sys.error("ID is empty"))
    val secret = Play.configuration.getString("aws.key.secret").getOrElse(sys.error("secret is empty"))

  }


}
