package org.ml4ai.exec

import org.ml4ai.WHConfig
import org.ml4ai.inference.CoocurrenceKnowledgeGraph
import org.ml4ai.utils.AnnotationsLoader

object SentencesWithRelations extends App {

  // Instantiate the annotations loader
  implicit val annotationsLoader = new AnnotationsLoader(WHConfig.Files.annotationsFile, cache = false)

  // Iterate over each document
  for(docHash <- annotationsLoader.rawDocuments) {
    // Instantiate a knowledge graph for this doc to get the relations
    val kg = new CoocurrenceKnowledgeGraph(Seq(docHash))
    // Extract the relations and then the attributing elements, which has the actual data we need
    val relations = kg.relations
    val attributions = relations.flatMap(_.attributions)
    // Get the sentence indices with relations, sorted for convenience, altough it doesn't really matter
    val sentenceIndices = attributions.map(_.sentenceIx).toSeq.sorted.distinct
    // Get the processors document instance
    val doc = kg.documents.head._2
    // Fetch the sentences text
    for(six <- sentenceIndices){
      val sen = doc.sentences(six)
      // Get the tokenized words
      val words = sen.words
      // Put together as a TSV row
      val text = words.mkString("\t")
      val rawText = sen.getSentenceText
      // Prepend the key
      val row = s"$docHash\t$six\t$text\t$rawText"
      // Print it to stdout
      println(row)
    }
  }

}
