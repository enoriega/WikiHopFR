package org.ml4ai.exec

import com.typesafe.scalalogging.LazyLogging
import org.ml4ai.exec.InstanceFinderApp.logger
import org.ml4ai.mdp.WikiHopEnvironment
import org.ml4ai.utils.{SupportDocs, WikiHopParser, using}

import scala.collection.parallel.ForkJoinTaskSupport
import scala.concurrent.ExecutionContext
import scala.io.Source

object MinimumPapersNecessary extends App with LazyLogging{
  import ExecutionContext.Implicits.global
  val instances = WikiHopParser.trainingInstances

  val relevantIds =
    using(Source.fromFile("small_instances.txt"))(_.getLines().toList) :::
      using(Source.fromFile("testing_instances.txt"))(_.getLines().toList)



  val relevantInstances = (instances filter (relevantIds contains _.id)).par
  relevantInstances.tasksupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(12))

  val results =
    for(instance <- relevantInstances) yield {
      val id = instance.id
      logger.info(s"Starting $id")
      val docUniverse = SupportDocs.relatedDocs(instance)
      val env = new WikiHopEnvironment(id, Some(docUniverse))
      env.readDocumentUniverse()
      val path = env.outcome
      if(path.nonEmpty){
        val sourcePapers = path.head.flatMap(_.attributions.map(_.document)).distinct
        val outputString = s"$id\t" + sourcePapers.mkString("\t")
        Some(outputString)
      }
      else{
        logger.error(s"Should've been able to find a path for $id")
        None
      }
    }

  results collect {
    case Some(s) => s
  } foreach println

}
