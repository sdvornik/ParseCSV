package com.yahoo.sdvornik

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
  val CRLF = s"$CR$LF"

  val FIELD_DELIMITER: Parser[Char]   = ','
  val RECORD_DELIMITER: Parser[Any]  = Parser(CRLF) | Parser(CR) | Parser(LF)

  val TEXT: Parser[String] = s"""[^$LF,$CR]""".r

  def field: Parser[String] = (TEXT*) ^^ (_.mkString(""))

  def record: Parser[List[String]] = rep1sep(field, FIELD_DELIMITER)

  def block: Parser[List[List[String]]] = opt(BOM) ~> repsep(record, RECORD_DELIMITER) <~ opt(CRLF)

  def parse(str: String): List[List[String]] = parseAll(block, str) match {
    case Success(res, _) => res
    case _ => List[List[String]]()
  }
}

