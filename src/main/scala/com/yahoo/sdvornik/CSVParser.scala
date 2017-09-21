package com.yahoo.sdvornik

import java.util.Locale

import shapeless.LabelledGeneric
import shapeless.ops.record.Keys

import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.language.postfixOps
/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */
import scala.reflect.runtime.universe._

class CSVParser extends RegexParsers {

  val dataReader = new CaseClassReader[FinancialData]
  import RecordReader ._

  private val map = typeOf[FinancialData]
    .decl(termNames.CONSTRUCTOR)
    .asMethod.paramLists
    .reduceLeft(_ ++ _)
    .map { s => s.name.toString }
    .zipWithIndex.toMap

  private def getOrder(headerList: List[String]): List[Int] = {
    val res = headerList
      .foldRight(List.empty[Int])((x, acc) => map(x.toLowerCase(Locale.US)) :: acc)
    println(res)
    res
  }

  def getOrder2(headerList: List[String]): List[Int] = {
    val label = LabelledGeneric[FinancialData]
    val paramMap = Keys[label.Repr].apply.toList.map(_.name).zipWithIndex.toMap
    headerList.map(_.trim.toLowerCase(Locale.US)).map(paramMap(_))
  }

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

  def header: Parser[List[Int]] = rep1sep(field, FIELD_DELIMITER) ^^ getOrder

  def record: Parser[List[String]] = rep1sep(field, FIELD_DELIMITER)

  def block: Parser[List[Option[FinancialData]]] =
    opt(BOM) ~>
      header.flatMap(order => {
        import CaseClassReader._

        repsep(record, RECORD_DELIMITER) ^^ (_.map(list => dataReader.read(order, list)))
      }) <~ opt(EOF)

  def parse(str: String): List[Option[FinancialData]] = parseAll(block, str) match {
    case Success(res, _) => res
    case _ => List.empty
  }
}