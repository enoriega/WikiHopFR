package org.ml4ai.ir

import java.io.File

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import org.ml4ai.utils.WikiHopParser
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.store.NIOFSDirectory
import org.apache.lucene.index.IndexWriter
import org.ml4ai.ir.LuceneHelper.{addToIndex, analyzer}


object IndexerApp extends App with LazyLogging {

  val config = ConfigFactory.load()
  val indexDir = new File(config.getString("lucene.directoryIndex"))

  if(!indexDir.exists()){
    indexDir.mkdirs()
  }

  val documents = WikiHopParser.allDocuments
  val index = new NIOFSDirectory(indexDir.toPath)

  val ixWConfig = new IndexWriterConfig(analyzer)

  val w = new IndexWriter(index, ixWConfig)

  documents.zipWithIndex foreach {
    case (d, i) =>
      logger.info(s"indexing $i")
      addToIndex(w, d)
  }

  w.commit()
  w.close()

}
