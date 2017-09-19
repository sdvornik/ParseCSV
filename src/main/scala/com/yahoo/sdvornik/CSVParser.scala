package com.yahoo.sdvornik

import java.time.ZoneId

import com.yahoo.sdvornik.TimeHelper.TimeHelper

import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.language.postfixOps
/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */

object CSVParser extends RegexParsers {

  override protected val whiteSpace: Regex = """[ \t]""".r

  val CR = '\u000D'
  val LF = '\u000A'
  val BOM = '\uFEFF'
  val FIELD_DELIMITER = ','
  val CRLF = s"$CR$LF"

  val EOF: Parser[Any] = s"""[$CRLF]+""".r

  val RECORD_DELIMITER: Parser[Any]  = Parser(CRLF) | Parser(CR) | Parser(LF)

  val TEXT: Parser[String] = s"""[^$LF$CR$FIELD_DELIMITER$BOM]""".r

  def field: Parser[String] = (TEXT*) ^^ (_.mkString(""))

  def header: Parser[List[Int]] = rep1sep(field, FIELD_DELIMITER) ^^ (x => FinancialData.getOrder(x))

  def record: Parser[List[String]] = rep1sep(field, FIELD_DELIMITER)

  def block: Parser[List[Option[FinancialData]]] =
    opt(BOM) ~>
      header.flatMap(order => {
        import CaseClassReader._
        val dataReader = new CaseClassReader[FinancialData](order)
        repsep(record, RECORD_DELIMITER) ^^ (_.map(list => dataReader.read(list)))
      }) <~ opt(EOF)

  def parse(str: String): List[Option[FinancialData]] = parseAll(block, str) match {
    case Success(res, _) => res
    case _ => List.empty
  }
}