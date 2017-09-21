package com.yahoo.sdvornik

object Solver extends App {

  def findCoincidence(line: Vector[Int]): List[List[Int]] = line
    .zipWithIndex
    .foldLeft(Map.empty[Int,List[Int]]) {
      case(acc, (value, index)) => acc + ((value, index :: acc.getOrElse(value, List.empty[Int])))
    }
    .toList
    .filter { case(_, list) => list.length > 1}
    .map { case (_, list) => list }

  def findLineNumbers(list: List[List[Int]]): List[Int] = {

    def findLineNumbersRec(list: List[List[Int]], acc: List[Int]): List[Int] = list match {
      case x::xs =>
        val newAcc: List[Int] = acc.flatMap(accElm => x.map(v => v | accElm))
        findLineNumbersRec(xs, newAcc)
      case Nil => acc
    }
    findLineNumbersRec(list, List(0))

  }

println(findCoincidence(Vector(1,2,2,6,7,3,2,3)))
  println(findLineNumbers(findCoincidence(Vector(1,2,2,6,7,3,2,3))))

}
