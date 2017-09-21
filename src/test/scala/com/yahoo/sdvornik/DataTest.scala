package com.yahoo.sdvornik
import java.time.temporal.ChronoUnit
import java.time.{Instant, ZoneId}

import org.scalatest._
import org.junit.runner.RunWith
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.check.Checkers
import org.scalatest.junit.JUnitRunner

/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */
@RunWith(classOf[JUnitRunner])
class DataTest extends FunSuite with Checkers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 500, minSize = 0, sizeRange = 500)

  private val headerList: List[String] = List("Open", "High", "Low", "Close", "Date", "Volume")

  private val helper = TimeHelper(ZoneId.of("UTC"))
  // TODO
  private val order = null //FinancialData.getOrder(headerList)

  private val dataReader = new CaseClassReader[FinancialData]()

  private val now = System.currentTimeMillis()

  import scala.concurrent.duration._
  private val interval = 1.day.toMillis * 365

  private val priceGen = Gen.choose[Double](1.toDouble, 100.toDouble)
  private val volumeGen = Gen.choose[Long](1e3.toLong, 1e6.toLong)

  private val dataGen: Gen[(FinancialData, List[String])] = for (
    instant <- Gen.choose[Long](now - interval, now).map(Instant.ofEpochMilli).map(_.truncatedTo(ChronoUnit.DAYS));
    open <- priceGen;
    high <- priceGen;
    low <- priceGen;
    close <- priceGen;
    volume <- volumeGen
  ) yield (
    FinancialData(instant, open: Double, high: Double, low: Double, close: Double, volume: Long),
    List(open.toString, high.toString, low.toString, close.toString, helper.toStringRep(instant), volume.toString)
  )

  private implicit def dataGenArb(implicit a: Arbitrary[Long]): Arbitrary[(FinancialData, List[String])] = Arbitrary(dataGen)

  import CaseClassReader._
  test("Check data case class") {
    check { (d: (FinancialData, List[String])) => {
      val data = d._1
      val list = d._2
      val newData = dataReader.read(order, list).get
      data == newData
    }}
  }
}