package com.yahoo.sdvornik

import java.nio.charset.StandardCharsets
import java.time.ZoneId

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, Uri}
import akka.stream.ActorMaterializer

import scala.concurrent.{Await, Future}
/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */
trait Query {
  /* 1 - 1 year historic prices given a ticker */
  def dailyPrices(ticker: String): Future[List[Double]]

  /* 2- daily returns, where return = ( Price_Today – Price_Yesterday)/Price_Yesterday */
  def returns(ticker:String): Future[List[Double]]

  /* 3 – 1 year mean returns */
  def meanReturn(ticker:String): Future[Option[Double]]
}

object EntryPoint extends App with Query {

    implicit val system = ActorSystem("client")
    import system.dispatcher

    implicit val materializer = ActorMaterializer()

    private val http = Http()
    private implicit val helper = TimeHelper(ZoneId.of("UTC"))
    val imp = new ReaderImplicits
    import imp._

    private def requestData(ticker: String): Future[List[FinancialData]] = {
      val source = Uri(s"https://finance.google.com/finance/historical?q=NASDAQ:$ticker&output=csv")
      http.singleRequest(HttpRequest(uri = source))
        .flatMap(_.entity.dataBytes.runReduce((a, b) => a ++ b))
        .map(_.decodeString(StandardCharsets.UTF_8))
        .map(content => {

          CSVParser.parse(content) match {
            case head :: tail =>
              val order = FinancialData.getOrder(head)
              val dataReader = new CaseClassReader[FinancialData](order)
              tail.map(list => dataReader.read(list))
            case _ => List.empty
          }
        }).map(_.foldRight(List.empty[FinancialData]) {
          case (Some(x), acc) => x :: acc
          case (_, acc) => acc
        })
    }


  override def dailyPrices(ticker: String): Future[List[Double]] = {
    val f = requestData(ticker)
    null
  }

  override def returns(ticker: String): Future[List[Double]] = {
    val f = requestData(ticker)
    null
  }

  override def meanReturn(ticker: String): Future[Option[Double]] = {
    val f = requestData(ticker)
    null
  }


  val ticker = "GOOG"
  val f = requestData(ticker)
  import scala.concurrent.duration._
  Await.result(f, 1.second)
  println(f.value.get.get.mkString("\n"))

}
