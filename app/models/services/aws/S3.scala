package models.services.aws

import java.io.File

import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.AmazonS3Client
import com.amazonaws.services.s3.model._
import models.SecureGen

import scala.concurrent.{Future, Promise}
import scala.util.Try

/**
 * Created by artem on 02.12.14.
 */
object S3 {

  import models.Configuration.aws._

  val s3 = new AmazonS3Client(new BasicAWSCredentials(id, secret))

  val bucketName = "business-framework"

  /**
   * Put file
   * @param file file to put
   * @param name file name
   * @param folder folder to put
   * @return
   */
  def put(file: File, name: String = "", folder: String = ""): Future[(String, PutObjectResult)] = {
    val pat = """(.*)[.]([^.]*)""".r
    val id = SecureGen.nextSessionId()
    val key = folder + "/" + id + (name match {
      case pat(fn, ext) =>
        "." + ext
      case _ => ""
    })

    def makeUrl() ={
      s"https://s3.eu-central-1.amazonaws.com/business-framework/$key"
    }

    val result = Promise[(String, PutObjectResult)]()

    result.complete(Try((makeUrl(), s3.putObject(new PutObjectRequest(bucketName, key, file).withCannedAcl(CannedAccessControlList.PublicRead)))))
    result.future
  }

}
