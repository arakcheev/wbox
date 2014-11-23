package models

/**
 * Created by artem on 23.11.14.
 */
import java.math.BigInteger
import java.security.SecureRandom

object SecureGen {
  private val random = new SecureRandom()

  def nextSessionId(): String =  {
    new BigInteger(130, random).toString(32)
  }
}