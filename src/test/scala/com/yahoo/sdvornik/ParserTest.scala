package com.yahoo.sdvornik

import org.scalatest._

import scala.io.Codec
import org.scalatest.check.Checkers
/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */
class ParserTest  extends FunSuite with Checkers {

  implicit val codec: Codec = Codec.UTF8
  private val bufferedSource = scala.io.Source.fromResource("ticks")
  private val content = bufferedSource.mkString
  bufferedSource.close()

  test("Check parser") {
    val res: List[Option[FinancialData]] = CSVParser.parse(content)
    println(res.mkString("\n"))
    assert(res.forall(_.nonEmpty))
  }
}
