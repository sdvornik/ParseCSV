package com.yahoo.sdvornik

import java.time.ZoneId

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

    val res: List[Option[FinancialData]] = CSVParser.parse(content) match {
      case head :: tail =>
        implicit val helper = TimeHelper(ZoneId.of("UTC"))
        val order = FinancialData.getOrder(head)
        val dataReader = new CaseClassReader[FinancialData](order)

        val imp = new ReaderImplicits
        import imp._

        tail.map(list => dataReader.read(list)).map(x => {println(x);x})
    }
    assert(res.foldLeft(true)(_ && _.nonEmpty))

  }



}
