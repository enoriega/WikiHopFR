package org.ml4ai.inference

import org.clulab.odin.TextBoundMention
import org.clulab.processors.Document
import org.clulab.utils.DependencyUtils
import org.ml4ai.utils.{AnnotationsLoader, any, filterUselessLemmas}

class NamedEntityLinkKnowledgeGraph(documents:Iterable[(String,Document)]) extends NERBasedKnowledgeGraph(documents) {

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
        // Compute the entity hashes
        val entityHashes = es map (e => (e, groupedEntityHashes(filterUselessLemmas(e.lemmas.get).toSet)))
        // Get all the pairs of entity hashes and compute their attribution
        (for{
          (a, hashA) <- entityHashes
          (b, hashB) <- entityHashes
          if hashA != 0 && hashB != 0 && hashA != hashB
          if hasConnectingPath(a, b)
        } yield {
          (hashA, hashB, AttributingElement(None, sIx, hash))
        }).toSet
    }
  }

  protected def hasConnectingPath(a:TextBoundMention, b:TextBoundMention):Boolean = {
    // Sanity check
    assert(a.sentenceObj == b.sentenceObj)

    val sentence = a.sentenceObj

    sentence.dependencies match {
      case Some(deps) =>
//        val pathExistences =
//          for{
//            i <- a.tokenInterval
//            j <- b.tokenInterval
//          } yield deps.shortestPath(i, j).nonEmpty
//
//        any(pathExistences)

        val headsA = DependencyUtils.findHeadsStrict(a.tokenInterval, a.sentenceObj)
        val headsB = DependencyUtils.findHeadsStrict(b.tokenInterval, b.sentenceObj)

        val pathExistences =
          for{
            i <- headsA
            j <- headsB
          } yield deps.shortestPath(i, j).nonEmpty

        any(pathExistences)
      case None =>
        throw new IllegalStateException(s"No dependency annotations for sentence ${sentence.getSentenceText}")
    }
  }

}
