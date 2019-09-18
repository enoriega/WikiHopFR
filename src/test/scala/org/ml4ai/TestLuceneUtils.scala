package org.ml4ai

import org.scalatest.{FlatSpec, Matchers}
import org.apache.lucene.index.{DirectoryReader, IndexWriter, IndexWriterConfig}
import org.apache.lucene.store.{FSDirectory, RAMDirectory}
import org.ml4ai.ir.LuceneHelper
import LuceneHelper.addToIndex
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.search.IndexSearcher
import org.ml4ai.mdp.{Cascade, Exploitation, Exploration, ExplorationDouble}
import org.ml4ai.utils.md5Hash

class TestLuceneUtils extends FlatSpec with Matchers{

  // Build in-memory index as fixture
  import java.nio.file.Files
  import java.nio.file.Path

  val indexPath: Path = Files.createTempDirectory("tempIndex")
  val directory = new RAMDirectory()//FSDirectory.open(indexPath)
  val iwriter = new IndexWriter(directory, new IndexWriterConfig(new StandardAnalyzer))

  // These are two lexical-disjoint documents and a one that shares some terms
  val doc1 = "Enrique Noriega is working on his dissertation"
  val doc2 = "WikiHop is so cool!. I love it"
  val doc3 = "Enrique uses WikiHop to test hypotheses for his dissertation. Very cool"

  // Hash values to test retrieval functionality
  val hash1 = md5Hash(doc1)
  val hash2 = md5Hash(doc2)
  val hash3 = md5Hash(doc3)

  // Add the documents to the index
  addToIndex(iwriter, doc1)
  addToIndex(iwriter, doc2)
  addToIndex(iwriter, doc3)

  iwriter.close()

  val searcher = new IndexSearcher(DirectoryReader.open(directory))
  ///////////////////////////////////

  "The index" should "contain three documents" in {
    val reader = DirectoryReader.open(directory)
    reader.numDocs() should be (3)
  }

  it should "be case-insensitive" in {
    val termUpperCase = Seq("Enrique", "Noriega")
    val termLowerCase = Seq("enrique", "noriega")

    val actionUpperCase = Exploration(termUpperCase)
    val actionLowerCase = Exploration(termLowerCase)

    val resultsUpperCase = LuceneHelper.retrieveDocumentNames(actionUpperCase, searcher = searcher)
    val resultsLowerCase = LuceneHelper.retrieveDocumentNames(actionLowerCase, searcher = searcher)

    resultsUpperCase should contain (hash1)
    resultsLowerCase should be (resultsUpperCase)
  }

  it should "filter the results to a subset of the index" in {
    val term = Seq("Enrique")

    val action = Exploration(term)

    val resultUnfiltered = LuceneHelper.retrieveDocumentNames(action, searcher = searcher)
    val resultsFiltered = LuceneHelper.retrieveDocumentNames(action, Some(Set(hash3)), searcher = searcher)

    resultUnfiltered should contain allOf (hash1, hash3)
    resultsFiltered should contain only hash3
  }

  "The \"Exploration(Enrique)\" action" should "return two documents" in {
    val term = Seq("Enrique")

    val action = Exploration(term)
    val results = LuceneHelper.retrieveDocumentNames(action, searcher = searcher)

    results should contain allOf (hash1, hash3)

  }

  "The \"ExplorationDouble(Enrique, WikiHop)\" action" should "return three documents" in {
    val termA = Seq("Enrique")
    val termB = Seq("WikiHop")

    val action = ExplorationDouble(termA, termB)
    val results = LuceneHelper.retrieveDocumentNames(action, searcher = searcher)

    results should contain allOf (hash1, hash2, hash3)
  }

  "The \"Exploitation(Enrique, WikiHop)\" action" should "return one" in {
    val termA = Seq("Enrique")
    val termB = Seq("WikiHop")

    val action = Exploitation(termA, termB)
    val results = LuceneHelper.retrieveDocumentNames(action, searcher = searcher)

    results should contain only hash3
  }

  "The \"Cascade(Enrique, WikiHop)\" action" should "return one" in {
    val termA = Seq("Enrique")
    val termB = Seq("WikiHop")

    val action = Cascade(termA, termB)
    val results = LuceneHelper.retrieveDocumentNames(action, searcher = searcher)

    results should contain only hash3
  }

  "The \"Cascade(love it, test)\" action" should "return two" in {
    val termA = Seq("love", "it")
    val termB = Seq("test")

    val action = Cascade(termA, termB)
    val results = LuceneHelper.retrieveDocumentNames(action, searcher = searcher)

    results should contain allOf (hash2, hash3)
  }

}
