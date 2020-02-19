package org.ml4ai.utils

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.ml4ai.{HotPotInstance, WHConfig, WikiHopInstance}

import scala.io.Source

object HotPotParser {

  private val trainingPath = WHConfig.Files.HotPotQA.jsonPath

  private lazy val jsonTrain = using(Source.fromFile(trainingPath)) {
    trainStream =>
      parse(trainStream.bufferedReader())
  }

  //  private lazy val documentsTrain = for{ JObject(elem) <- jsonTrain
  //                          JField("supports", JArray(documents)) <- elem
  //                          JString(document) <- documents
  //                       } yield document

  private lazy val documentsTrain =
    (for {JObject(elem) <- jsonTrain
         JField("type", JString(tp)) <- elem
         if tp == "bridge"
         JField("context", JArray(documents)) <- elem
         JArray(JString(pageName)::List(JArray(sentences))) <- documents
    } yield (pageName, (sentences map { case JString(text) => text }).mkString("\n"))).toSet



  lazy val allDocuments:Set[(String, String)] = documentsTrain


  lazy val trainingInstances:Seq[HotPotInstance] = {
    for{
      JObject(elem) <- jsonTrain
      JField("type", JString(tp)) <- elem
      if tp == "bridge"
      JField("_id", JString(id)) <- elem
      JField("question", JString(question)) <- elem
      JField("supporting_facts", JArray(facts)) <- elem
      JField("answer", JString(answer)) <- elem
      JField("context", JArray(supportDocs)) <- elem
    } yield HotPotInstance(id,
        question,
        facts.head match { case JArray(JString(start)::_) => start},
        Some(answer),
        supportDocs map { c => (c: @unchecked) match{ case JArray(JString(pageName)::List(JArray(sentences))) => sentences.mkString("\n") }})
  }

  lazy private val trainingMap = trainingInstances.map(i => i.id -> i).toMap


  def get(instanceId:String): HotPotInstance = {
    trainingMap(instanceId)
  }

}
