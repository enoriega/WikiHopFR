package org.ml4ai.utils

import com.typesafe.scalalogging.LazyLogging
import org.apache.http.client.HttpClient
import org.apache.http.client.methods.{HttpPost, HttpPut}
import org.apache.http.entity.{ContentType, StringEntity}

import scala.io.Source
import scala.util.{Failure, Success, Try}

object HttpUtils extends LazyLogging{

  def httpPut(method:String, data:String)(implicit httpClient:HttpClient):String = {
    val request = new HttpPut(s"http://localhost:5000/$method") // TODO: Parameterize the endpoint
    val content = new StringEntity(data, ContentType.create("text/plain", "UTF-8"))

    request.setEntity(content)

    val response = httpClient.execute(request)

    Try {
      val entity = response.getEntity
      if (entity != null) {
        using(entity.getContent){
          stream =>
            Source.fromInputStream(stream).mkString
        }
      }
      else
        ""
    } match {
      case Success(content) =>
        content
      case Failure(exception) =>
        logger.error(exception.getMessage)
        ""
    }

  }

  def saveModel(name:String)(implicit httpClient:HttpClient):String = {
    val request = new HttpPost(s"http://localhost:5000/save?name=$name") // TODO: Parameterize the endpoint
//    val content = new StringEntity(data, ContentType.create("text/plain", "UTF-8"))
//    request.setEntity(content)

    val response = httpClient.execute(request)

    Try {
      val entity = response.getEntity
      if (entity != null) {
        using(entity.getContent){
          stream =>
            Source.fromInputStream(stream).mkString
        }
      }
      else
        ""
    } match {
      case Success(content) =>
        content
      case Failure(exception) =>
        logger.error(exception.getMessage)
        ""
    }

  }
}
