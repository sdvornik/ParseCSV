package com.yahoo.sdvornik

/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */
import java.time.{Instant, ZoneId}

import com.yahoo.sdvornik.TimeHelper.TimeHelper
import shapeless._
import shapeless.syntax.std.traversable._
import shapeless.ops.traversable._

import scala.util.Try

trait Reader[A] {
  def read(str: String): Option[A]
}

trait HListReader[A] {

  type in <: HList

  def read(list: in): Option[A]
}

object HListReader {

  type Aux[A, B] = HListReader[B] { type in = A }

  implicit val hnilReader = new HListReader[HNil] {
    type in = HNil
    def read(value: HNil) = Some(HNil)
  }

  implicit def hlistReader[T, H <: HList](implicit fieldReader: Reader[T], listReader: HListReader[H]) =
    new HListReader[T :: H] {
      type in = String :: listReader.in

      def read(value: in): Option[T :: H] = value match {
        case x :: xs => for {
          head <- fieldReader.read(x)
          tail <- listReader.read(xs)
        } yield head :: tail
      }
    }
}

object CaseClassReader {

  implicit val helper: TimeHelper = TimeHelper(ZoneId.of("UTC"))

  implicit val readInt: Reader[Int] = (str: String) => Try(str.toInt).toOption

  implicit val readLong: Reader[Long] = (str: String) => Try(str.toLong).toOption

  implicit val readDouble: Reader[Double] = (str: String) => Try(str.toDouble).toOption

  implicit val readInstant: Reader[Instant] = (str: String) => Try(helper.toInstant(str)).toOption

  implicit val readString: Reader[String] = (str: String) => Some(str)

}

class CaseClassReader[T](val order: List[Int]) {

  def read[A <: HList, B <: HList](list: List[String])(implicit
                                                       generic: Generic.Aux[T, B],
                                                       reader: HListReader.Aux[A, B],
                                                       fromTr: FromTraversable[A]
  ): Option[T] =
    list.zip(order).sortBy(_._2).map(_._1).toHList[A].flatMap(reader.read).map(generic.from)
}

