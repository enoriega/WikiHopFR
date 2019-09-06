package org.ml4ai.exec

import java.io.{FileOutputStream, PrintWriter}

import com.typesafe.scalalogging.LazyLogging
import org.clulab.embeddings.word2vec.Word2Vec
import org.ml4ai.WHConfig
import org.ml4ai.mdp.WikiHopEnvironment
import org.ml4ai.utils.{SupportDocs, WikiHopParser}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Extracts the vocabulary to be used by the word embeddings
  */
object EmbeddingsVocabularyExtractor extends App with LazyLogging{

  val dictionaryPath = WHConfig.Embeddings.vocabularyFile

  // Load the training instances with the environment configuration
  logger.info("Reading the training instances.")
  val instances = WikiHopParser.trainingInstances

  logger.info("Extracting entities from the training instances.")
  val entities =
    (for(instance <- instances.par) yield {
      logger.debug(s"Reading ${instance.id}")
      val docs = SupportDocs.localDocs(instance)
      val env = new WikiHopEnvironment(instance.id, instance.query.split(" ").last, instance.answer.get, Some(docs))
      env.readDocumentUniverse()
      env.entities.flatten
    }).seq.flatten.toSet

  logger.info(s"Finished extracting entities. Found ${entities.size} instances.")

  logger.info("Sanitizing the entities.")

  val postProcessedEntities =
    entities map (e => Word2Vec.sanitizeWord(e.toLowerCase))

  logger.info(s"A total of ${postProcessedEntities.size} post-processed entities where found.")

  logger.info(s"Saving the dictionary into $dictionaryPath")

  val pw = new PrintWriter(new FileOutputStream(dictionaryPath))
  postProcessedEntities foreach (e => pw.println(e))
  pw.close()

  logger.info("Finished.")


}
