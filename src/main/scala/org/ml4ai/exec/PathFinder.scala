package org.ml4ai.exec

import java.io.{PrintWriter, StringWriter}

import com.typesafe.scalalogging.LazyLogging
import org.clulab.utils.Serializer
import org.ml4ai.WHConfig
import org.ml4ai.inference.{CoocurrenceKnowledgeGraph, NamedEntityLinkKnowledgeGraph, OpenIEKnowledgeGraph, VerboseRelation}
import org.ml4ai.utils.{AnnotationsLoader, SupportDocs, WikiHopParser}

import scala.util.{Failure, Success, Try}

object PathFinder extends App with LazyLogging{

  abstract class Outcome

  case class Successful(paths: Iterable[Seq[VerboseRelation]]) extends Outcome

  case object NoPaths extends Outcome

  case class Unsuccessful(e: Throwable) extends Outcome

  val instances = WikiHopParser.trainingInstances

  implicit val loader: AnnotationsLoader = new AnnotationsLoader(WHConfig.Files.annotationsFile)

  // For each of the training instances
  val results =
    (for (instance <- instances.par) yield {

      val documentUniverse = WHConfig.Environment.documentUniverse match {
        case "Local" =>
          SupportDocs.localDocs(instance)
        case "Random" =>
          SupportDocs.randomDocs(instance)

        case "Related" =>
          SupportDocs.relatedDocs(instance)
        case unsupported =>
          throw new UnsupportedOperationException(s"Document universe of $unsupported kind is not supported")
      }

      //val kg = new CoocurrenceKnowledgeGraph(instance.supportDocs)
      val kgt = Try(WHConfig.PathFinder.knowledgeGraphType match {
        case "NamedEntityLink" => new NamedEntityLinkKnowledgeGraph(documentUniverse)
        case "Cooccurrence" => new CoocurrenceKnowledgeGraph(documentUniverse)
        case "OpenIE" => new OpenIEKnowledgeGraph(documentUniverse)
        case unknown => throw new UnsupportedOperationException(s"KnowledgeGraph type not implemented: $unknown")
      })

      kgt match {
        case Success(kg) =>
          val source = instance.query.split(" ").drop(1).mkString(" ")
          val destination = instance.answer.get

          val result = Try(kg.findPath(source, destination)) recoverWith {
            case tr:java.util.NoSuchElementException =>
              val buf = new StringWriter()
              val writer = new PrintWriter(buf)
              tr.printStackTrace(writer)
              logger.error(buf.toString)
              Failure(tr)
          }

          val ret: Outcome = result match {
            case Success(paths) if paths.nonEmpty => Successful(paths.toSet)
            case Success(_) => NoPaths
            case Failure(exception) => Unsuccessful(exception)
          }

          instance.id -> ret

        case Failure(ex) =>
          instance.id -> Unsuccessful(ex)
      }


    }).toMap.seq


  Serializer.save(results, WHConfig.PathFinder.outputFile)

  val x = results.values.count {
    case Successful(_) => true
    case _ => false
  }

  println(s"Successes: $x")
  println(getOutcomeCounts(results))
  println(getErrorCounts(results))

  def getOutcomeCounts(res: Map[String, Outcome]) = {
    var successes = 0
    var errors = 0
    var noPaths = 0
    for (o <- res.values) {
      o match {
        case Successful(_) => successes += 1;
        case NoPaths => noPaths += 1;
        case Unsuccessful(_) => errors += 1
      }
    }
    Map("Successes" -> successes, "NoPaths" -> noPaths, "Errors" -> errors)
  }

  def getErrorCounts(res: Map[String, Outcome]) = {
    res.values.collect{ case Unsuccessful(e) => e }.groupBy(_.getClass).map{ case (k, v) => k.toString -> v.size}
  }


}
