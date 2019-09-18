package org.ml4ai.learning

import com.typesafe.scalalogging.LazyLogging
import edu.cmu.dynet.{Dim, Expression, FloatVector, LookupParameter, ParameterCollection, ParameterInit}
import org.clulab.embeddings.word2vec.Word2Vec
import org.ml4ai.WHConfig
import org.ml4ai.WHConfig.Files.glovePath
import org.ml4ai.utils.{SupportDocs, WikiHopParser}
import org.ml4ai.utils._

import scala.io.Source

class EmbeddingsHelper(pc:ParameterCollection) extends LazyLogging {

  /**
    * Loads the Glove embeddings for all the words that could be entities in an instance of FR
    * @return
    */
  private def loadGloveEntities:(Word2Vec, Iterable[String]) = {
    // Instantiate all the environments and pull the entities
    val instances = WikiHopParser.trainingInstances


    val sanitizedTerms = using(Source.fromFile(WHConfig.Embeddings.vocabularyFile)){
      source =>
        source.getLines().toSet
    }

    // Sanitize them and use them as a filter to load the embeddings
    val embeddings = new Word2Vec(glovePath, Some(sanitizedTerms))

    val existingTerms = embeddings.matrix.keySet

    val missingTerms = (sanitizedTerms diff existingTerms) union Set("xnumx", "OOV")

    (embeddings, missingTerms)
  }

  // All the pretrained embeddings for the possible terms
  private val (originalEmbeddings, missingTerms) = loadGloveEntities

  private val pretrainedIndex = originalEmbeddings.matrix.keys.zipWithIndex.toMap
  private val missingIndex = missingTerms.zipWithIndex.toMap

  val embeddingsDim:Int = originalEmbeddings.dimensions

  // Make lookup parameters from the embeddings
  private val ev = FloatVector.Seq2FloatVector(originalEmbeddings.matrix.values.flatten.map(_.toFloat).toSeq)
  private val pretrainedEmbeddings = pc.addLookupParameters(originalEmbeddings.matrix.size, Dim(embeddingsDim), init = ParameterInit.fromVector(ev))
  private val missingEmbeddings = pc.addLookupParameters(missingIndex.size, Dim(embeddingsDim))

  def lookup(entity:Seq[String]):Seq[Expression] = {
    val result = entity map { term =>
      val sanitizedTerm = Word2Vec.sanitizeWord(term)

      if(pretrainedIndex contains sanitizedTerm) {
        // Fetch from the pretrained embeddings
        Expression.lookup(pretrainedEmbeddings, pretrainedIndex(sanitizedTerm))
      } else {
        // Fetch from the missing embeddings
//        Expression.lookup(missingEmbeddings, missingIndex(Word2Vec.sanitizeWord(term)))
        Expression.lookup(missingEmbeddings, missingIndex("OOV"))
      }
    }

    result
  }
}
