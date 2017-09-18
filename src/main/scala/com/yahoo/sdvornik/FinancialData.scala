package com.yahoo.sdvornik

import java.time.Instant
import java.util.Locale

import shapeless.LabelledGeneric
import shapeless.ops.record.Keys

/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */
object FinancialData {
  def getOrder(headerList: List[String]): List[Int] = {
    val label = LabelledGeneric[FinancialData]
    val paramMap = Keys[label.Repr].apply.toList.map(_.name).zipWithIndex.toMap
    headerList.map(_.trim.toLowerCase(Locale.US)).map(paramMap(_))
  }
}

case class FinancialData(date: Instant, open: Double, high: Double, low: Double, close: Double, volume: Long)
