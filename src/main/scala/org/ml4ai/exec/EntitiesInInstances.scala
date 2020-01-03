package org.ml4ai.exec

import org.ml4ai.WHConfig
import org.ml4ai.inference.CoocurrenceKnowledgeGraph
import org.ml4ai.utils.{AnnotationsLoader, WikiHopParser}
import org.ml4ai.utils.{lemmatize, md5Hash, using}

import scala.io.Source

object EntitiesInInstances extends App {

  // Instantiate the annotations loader
  implicit val annotationsLoader = new AnnotationsLoader(WHConfig.Files.annotationsFile, cache = false)

  def splitAndFilter(text: String): Seq[String] = lemmatize(text.toLowerCase().split(" "))

  def readInstances(path: String) =
    using(Source.fromFile(path)){
      source =>
        source.getLines().toSet
    }

  val relevant = readInstances("small_instances.txt") ++ readInstances("testing_instances.txt")

  splitAndFilter("Test")

//  val rows =
    WikiHopParser.trainingInstances.withFilter( i => relevant.contains(i.id)).map { instance =>
      val queryEntities: Seq[String] = splitAndFilter(instance.query.split(" ").tail.mkString(" "))
      val answerEntities: Seq[String] = splitAndFilter(instance.answer.get)
      val entities: Seq[String] = instance.supportDocs.flatMap {
        doc =>
          val docHash = md5Hash(doc)
          // Instantiate a knowledge graph for this doc to get the relations
          val kg = new CoocurrenceKnowledgeGraph(Seq(docHash))
          // Extract the relations and then the attributing elements, which has the actual data we need
          kg.entities.flatten.flatMap(splitAndFilter)
      }
      println(s"${instance.id}\t${queryEntities.mkString("\t")}\t${answerEntities.mkString("\t")}\t${entities.mkString("\t")}")
    }

}
