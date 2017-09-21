package com.yahoo.sdvornik

import scala.util.matching.Regex
import scala.util.parsing.combinator._

/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */

class CSVParser(implicit val dataReader: RecordStringReader[FinancialData]) extends RegexParsers {

  import RecordReader ._

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

  def header: Parser[List[Int]] = rep1sep(field, FIELD_DELIMITER) ^^ dataReader.getOrder

  def record: Parser[List[String]] = rep1sep(field, FIELD_DELIMITER)

  def block = opt(BOM) ~>
      header.flatMap(order => {
        repsep(record, RECORD_DELIMITER) ^^ (_.map(list => dataReader.read(order, list)))
      }) <~ opt(EOF)

  def parse(str: String) = parseAll(block, str) match {
    case Success(res, _) => res
    case _ => List.empty
  }
}