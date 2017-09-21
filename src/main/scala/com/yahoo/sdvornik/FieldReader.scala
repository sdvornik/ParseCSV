package com.yahoo.sdvornik

/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */
import java.time.{Instant, ZoneId}
import java.util.Locale

import com.yahoo.sdvornik.TimeHelper.TimeHelper
import shapeless._
import shapeless.ops.record.Keys
import shapeless.syntax.std.traversable._
import shapeless.ops.traversable._

import scala.util.Try

trait FieldReader[Field] {
  def read(str: String): Option[Field]
}

trait RecordReader[Record] {

  type hlist <: HList

  def read(list: hlist): Option[Record]
}

object RecordReader {

  type Aux[List, Record] = RecordReader[Record] {
    type hlist = List
  }

  implicit val hnilReader = new RecordReader[HNil] {
    type hlist = HNil

    def read(value: HNil) = Some(HNil)
  }

  implicit def hlistReader[Field, H <: HList](implicit fieldReader: FieldReader[Field], listReader: RecordReader[H]) =
    new RecordReader[Field :: H] {
      type hlist = String :: listReader.hlist

      def read(value: hlist): Option[Field :: H] = value match {
        case x :: xs => for {
          head <- fieldReader.read(x)
          tail <- listReader.read(xs)
        } yield head :: tail
      }
    }

  implicit val helper: TimeHelper = TimeHelper(ZoneId.of("UTC"))

  implicit val readInt: FieldReader[Int] = (str: String) => Try(str.toInt).toOption

  implicit val readLong: FieldReader[Long] = (str: String) => Try(str.toLong).toOption

  implicit val readDouble: FieldReader[Double] = (str: String) => Try(str.toDouble).toOption

  implicit val readInstant: FieldReader[Instant] = (str: String) => Try(helper.toInstant(str)).toOption

  implicit val readString: FieldReader[String] = (str: String) => Some(str)
}

class RecordStringReader[Record] {

  def getOrder(headerList: List[String]): List[Int] = {
    val label = LabelledGeneric[FinancialData]
    val paramMap = Keys[label.Repr].apply.toList.map(_.name).zipWithIndex.toMap
    headerList.map(_.trim.toLowerCase(Locale.US)).map(paramMap(_))
  }

  def read[A <: HList, B <: HList](order: List[Int], list: List[String])(implicit
                                                       generic: Generic.Aux[Record, B],
                                                       reader: RecordReader.Aux[A, B],
                                                       fromTr: FromTraversable[A]
  ): Option[Record] =
    list.zip(order).sortBy(_._2).map(_._1).toHList[A].flatMap(reader.read).map(generic.from)
}

