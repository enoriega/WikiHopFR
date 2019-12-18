package org.ml4ai.inference

import org.clulab.processors.{Document, RelationTriple}
import org.ml4ai.mdp.EntityOrigin
import org.ml4ai.utils.{AnnotationsLoader, filterUselessLemmas}

import scala.collection.mutable

class CoocurrenceKnowledgeGraph(documents:Iterable[(String,Document)]) extends NERBasedKnowledgeGraph(documents) {

  /**
    * Convenience constructor
    * @param documents Md5 hashes for the documents to mine
    * @param loader Implicit AnnotationsLoader instance
    * @return Instance of KnowledgeGraph built from info from the specified documents
    */
  def this(documents:Iterable[String])(implicit loader:AnnotationsLoader) =
    this(documents map (h => (h, loader(h))))


  /**
    * Extracts relation instances from a document object.
    * The endpoints should be expressed as "lemma hashes" and each relation must carry its attributing element
    *
    * @param hash Wikipedia doc md5 hash string
    * @param doc  Processor's doc annotated instance
    * @return An iterable with triples which have the following signature: Source hash, destination has, attribution
    */
  override protected def extractRelationsFromDoc(hash: String, doc: Document): Iterable[(Int, Int, AttributingElement)] = {
    val entities = getOdinEntitiesFromDoc(doc)

    entities.groupBy(_.sentence).flatMap{
      case (sIx, es) =>
        // Compute the entity lemmas
        val entityLemmas = es map (e => filterUselessLemmas(e.lemmas.get).toSet)
        // Compute the entity intervals
        val eOrigins = es map (e => EntityOrigin(hash, sIx, e.tokenInterval))
        // Compute the entity hashes
        val entityHashes = entityLemmas map groupedEntityHashes

        for(((e, lemmas), origin) <- es zip entityLemmas zip eOrigins){
          // Keep track of the lemmas
          val label = getNERLabel(e)
          if(entityTypes contains lemmas)
            entityTypes(lemmas) += label
          else
            entityTypes += (lemmas -> Set(label))

          // Keep track of entity origins
          if(entitySources contains lemmas)
            entitySources(lemmas) += origin
          else
            entitySources += (lemmas -> Set(origin))
        }


        // Get all the pairs of entity hashes and compute their attribution
        (for{
          (a, originA) <- entityHashes zip eOrigins
          (b, originB) <- entityHashes zip eOrigins
          if a != b && a != 0 && b != 0
        } yield {
          val triple = new RelationTriple(1.0f, originA.wordInterval, None, originB.wordInterval)
          (a, b, AttributingElement(Some(triple), sIx, hash))
        }).toSet
    }

  }

}
