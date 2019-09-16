package org.ml4ai.utils


import org.scalatest._

import scala.concurrent.{ExecutionContext, Future, TimeoutException}
import scala.concurrent.duration._

/* Borrowed from http://justinhj.github.io/2017/07/16/future-with-timeout.html */
class TestFutureUtils extends AsyncFlatSpec with Matchers with OptionValues with Inside with Inspectors {

  implicit override def executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  "futureWithTimeout" should "complete the users future when it returns before the timeout" in {

    def myFuture = Future[Int] {
      Thread.sleep(2.seconds.toMillis)
      100
    }

    FutureUtils.futureWithTimeout(myFuture, 3.seconds).map {
      result => if(result == 100) succeed else fail
    }
  }

  it should "not complete the future when it returns after the timeout" in {

    lazy val myFuture = Future[Int] {
      Thread.sleep(4.seconds.toMillis)
      100
    }

    recoverToSucceededIf[TimeoutException] {
      FutureUtils.futureWithTimeout(myFuture, 2.seconds)
    }
  }


}
