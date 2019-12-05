package org.ml4ai.inference

import org.clulab.odin.{Mention, TextBoundMention}
import org.clulab.processors.{Document, Sentence}
import org.ml4ai.ie.OdinEngine
import org.ml4ai.utils.entityGroundingHash
import org.ml4ai.utils.{AnnotationsLoader, filterUselessLemmas}

abstract class NERBasedKnowledgeGraph(documents:Iterable[(String,Document)]) extends KnowledgeGraph(documents) {

  /**
    * Provides a precomputed map of all the entities' lemma hash and a text representation for all the entities present
    * in the document support set of this graph
    *
    * @return The hashes of the set of post-processed lemmas on the collection
    */
  override protected def buildEntityLemmaHashes: Map[Set[String], Int] = {
    // For each of the documents...
    documents.flatMap{
      case (id, d) =>
        // Extract the entities with the grammar and keep only entity mentions
        getOdinEntitiesFromDoc(d).map{
          entity =>
            val lemmas = filterUselessLemmas(entity.lemmas.get)
            lemmas.toSet -> entityGroundingHash(lemmas)
        }
    }.toMap
  }

  protected def getOdinEntitiesFromDoc(d:Document):Iterable[TextBoundMention] = {
    // Odin extractor engine to get the entities
    OdinEngine.extractorEngine.extractFrom(d).collect{
      case tb:TextBoundMention if tb.labels.contains("Entity") => tb
    }
  }


  protected def getNERLabel(tb:TextBoundMention):String = {
    val validLabels = tb.labels filter NERBasedKnowledgeGraph.entityLabels.contains
    assert(validLabels.size == 1, "There's a named entity with more than one entity type")
    validLabels.head
  }
}

object NERBasedKnowledgeGraph {
  val entityLabels = Set("Person", "Organization", "Location", "Date", "Time", "Misc", "CommonNoun")
}
