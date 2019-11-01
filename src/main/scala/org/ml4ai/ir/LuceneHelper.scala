package org.ml4ai.ir

import java.io.File

import com.typesafe.scalalogging.LazyLogging
import org.apache.lucene.analysis.standard.StandardAnalyzer
import org.apache.lucene.index.{DirectoryReader, IndexWriter, Term}
import org.apache.lucene.search._
import org.apache.lucene.store.NIOFSDirectory
import org.ml4ai.mdp.{Cascade, Exploitation, Exploration, ExplorationDouble, RandomAction}
import org.sarsamora.actions.Action
import org.apache.lucene.analysis.tokenattributes.CharTermAttribute
import org.apache.lucene.document.{Document, Field, StringField, TextField}
import org.ml4ai.WHConfig
import org.ml4ai.utils.md5Hash

import collection.JavaConverters._
import scala.collection.mutable

/**
  * Utilities to interface with the lucene index
  */
object LuceneHelper extends LazyLogging {

  // State values
  private val indexDir = new File(WHConfig.Lucene.directoryIndex)
  private val index = new NIOFSDirectory(indexDir.toPath)
  private val reader = DirectoryReader.open(index)
  private val defaultSearcher = new IndexSearcher(reader)
  private val maxResults = if(WHConfig.Environment.maxPapersFetched > 0) WHConfig.Environment.maxPapersFetched else reader.numDocs // Make the upper bound be the number of documents in the index
  private val scoresCache = new mutable.HashMap[Action, Float]()


  lazy val analyzer = new StandardAnalyzer()

  /**
    * Returns the documents that contain any of the terms but not necessarily related to the query term
    * @param entityA Terms of the start of the search
    * @param entityB Terms of the end of the search
    * @param searcher Instance of lucene searcher (the index)
    * @return
    */
  def getLexicallySimilarDocuments(entityA:Set[String], entityB:Set[String], searcher:IndexSearcher = defaultSearcher):Seq[String] = {
    val allTerms = entityA union entityB
    val query = entityTermsDisjunction(allTerms)

    // Execute the query
    val topDocs = searcher.search(query, maxResults)

    //Generate the result in descending order by their TF-IDF score
    for(hit <- topDocs.scoreDocs.sortBy(_.score).reverse) yield {
      val doc = searcher.doc(hit.doc, Set("hash").asJava)
      doc.get("hash")
    }
  }

  private val cachedQueries = new mutable.HashSet[(Set[String], Set[String])]()

  // Do the actual IR
  def retrieveDocumentNames(action: Action, instanceToFilter:Option[Set[String]] = None, searcher:IndexSearcher = defaultSearcher, checkForEquivalence:Boolean = true):Set[String] = {

        // Build the query
        val query = actionToQuery(action)

        // Execute the query
        val topDocs = searcher.search(query, maxResults)

        //Generate the result
        val result =
          (for(hit <- topDocs.scoreDocs) yield {
            val doc = searcher.doc(hit.doc, Set("hash").asJava)
            doc.get("hash")
          }).toSet

        val ret =
          instanceToFilter match {
            case Some(criteria) =>
              result intersect criteria
            case None =>
              result
          }

        if(checkForEquivalence){
          action match {
            case ExplorationDouble(ea, eb) =>
              if(!(cachedQueries contains (ea, eb)) && !(cachedQueries contains (eb, ea))) {
                cachedQueries += Tuple2(ea, eb)
                val otherDocs = retrieveDocumentNames(Exploitation(ea, eb), instanceToFilter, searcher, false)
                val size = ret.intersect(otherDocs).size
                logger.debug(s"Query intersection debugging:\t$ea\t$eb\t$size")
              }
            case Exploitation(ea, eb) =>
              if(!(cachedQueries contains (ea, eb)) && !(cachedQueries contains (eb, ea))) {
                cachedQueries += Tuple2(ea, eb)
                val otherDocs = retrieveDocumentNames(ExplorationDouble(ea, eb), instanceToFilter, searcher, false)
                val size = ret.intersect(otherDocs).size
                logger.debug(s"Query intersection debugging:\t$ea\t$eb\t$size")
              }
          }
        }


        ret
  }

  /**
    * Computes the average score from the lucene index. Uses a cache for efficiency
    */
  def scoreAction(action: Action, instanceToFilter:Option[Set[String]] = None, searcher:IndexSearcher = defaultSearcher):Float = {
//    scoresCache.getOrElseUpdate(action,
//      {
          // Build the query
          val query = actionToQuery(action)

          // Execute the query
          val hits = searcher.search(query, maxResults).scoreDocs

          if(hits.isEmpty)
            0.0f
          else
            ((hits map (_.score)).sum / hits.size).toFloat

//      })
  }

  /**
    * Creates a lucene query parameterized by the action
    * @param action That defines the desired query
    * @return Lucene query characterized by action
    */
  private def actionToQuery(action: Action):Query = action match {
    case Exploration(a) => entityTermsConjunction(a)
    case ExplorationDouble(a, b) =>
      val queryA = entityTermsConjunction(a)
      val queryB = entityTermsConjunction(b)

      val builder = new BooleanQuery.Builder
      builder.add(new BooleanClause(queryA, BooleanClause.Occur.SHOULD))
      builder.add(new BooleanClause(queryB, BooleanClause.Occur.SHOULD))

      builder.build()
    case Exploitation(a, b) =>
      val queryA = entityTermsConjunction(a)
      val queryB = entityTermsConjunction(b)

      val builder = new BooleanQuery.Builder
      builder.add(new BooleanClause(queryA, BooleanClause.Occur.MUST))
      builder.add(new BooleanClause(queryB, BooleanClause.Occur.MUST))

      builder.build()
    case RandomAction =>
      logger.error("Can't build a query for the RandomAction. Should have been dealt with before this point")
      throw new UnsupportedOperationException("No lucene functionality for a random query")
    case _:Cascade =>
      logger.error("Can't build a query for the Cascade action. Should have been dealt with before this point")
      throw new UnsupportedOperationException("No lucene functionality for a cascade query")
  }


  private def entityTermsBooleanClause(entityTerms:Set[String], clause:BooleanClause.Occur):BooleanQuery = {
    val analyzedTerms = analyzeTerms(entityTerms)
    val termQueries = analyzedTerms map (t => new TermQuery(new Term("contents", t)))

    val queryBuilder = new BooleanQuery.Builder

    termQueries foreach {
      term =>
        queryBuilder.add(term, clause)
    }

    queryBuilder.build()
  }


  private def entityTermsConjunction(entityTerms:Set[String]):BooleanQuery =
    entityTermsBooleanClause(entityTerms, BooleanClause.Occur.MUST)

  private def entityTermsDisjunction(entityTerms:Set[String]):BooleanQuery =
    entityTermsBooleanClause(entityTerms, BooleanClause.Occur.SHOULD)


  // Taken from https://lucene.apache.org/core/7_2_1/core/org/apache/lucene/analysis/package-summary.html
  private def analyzeTerms(entityTerms:Set[String]):Set[String] = entityTerms flatMap {
    term =>
      val tokenStream = analyzer.tokenStream("contents", term)
      val termAtt: CharTermAttribute = tokenStream.addAttribute(classOf[CharTermAttribute])
      val tokens = new mutable.HashSet[String]
      tokenStream.reset()
      while(tokenStream.incrementToken()){
        tokens += termAtt.toString
      }
      tokenStream.end()
      tokenStream.close()
      tokens.toSet
  }

  /**
    * Adds a document to a provided index writer using the correct schema and analyzer
    * @param writer Previously configured index writer
    * @param value Text of the document to add to the index
    */
  def addToIndex(writer: IndexWriter, value: String):Unit = {
    // The hash will be the "title" of the document
    val hash = md5Hash(value)

    try {
      val doc = new Document
      doc.add(new TextField("contents", value, Field.Store.YES))
      doc.add(new StringField("hash", hash, Field.Store.YES))

      writer.addDocument(doc)
    } catch {
      case exception: Exception =>
        logger.error(s"Problem indexing: $value")
        logger.error(exception.getMessage)
    }
  }


}
