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

}
