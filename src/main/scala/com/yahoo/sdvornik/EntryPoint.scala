package com.yahoo.sdvornik

import java.nio.charset.StandardCharsets
import java.time.Instant

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, Uri}
import akka.stream.ActorMaterializer

import scala.concurrent.Future
import scala.util.{Failure, Success}
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

case class FinancialData(date: Instant, open: Double, high: Double, low: Double, close: Double, volume: Long)

object EntryPoint extends App with Query {

  implicit val system: ActorSystem = ActorSystem("client")
  import system.dispatcher

  implicit val materializer: ActorMaterializer = ActorMaterializer()

  private val http = Http()

  private def requestData(ticker: String): Future[List[FinancialData]] = {
    val source = Uri(s"https://finance.google.com/finance/historical?q=NASDAQ:$ticker&output=csv")
    implicit val recordStringReader = new RecordStringReader[FinancialData]
    http.singleRequest(HttpRequest(uri = source))
      .flatMap(_.entity.dataBytes.runReduce((a, b) => a ++ b))
      .map(_.decodeString(StandardCharsets.UTF_8))
      .map(new CSVParser().parse)
      .map(_.foldRight(List.empty[FinancialData]) {
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
  f.onComplete( {
    case Success(x) =>
      println(x.mkString("\n"))
      http.system.terminate()
    case Failure(e) =>
      println(e.getMessage)
      http.system.terminate()
  })


}
