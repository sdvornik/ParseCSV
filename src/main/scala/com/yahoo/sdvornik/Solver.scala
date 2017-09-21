package com.yahoo.sdvornik


object Solver extends App {

  type Matrix = Vector[Vector[Byte]]

  /*
  Find coincide number in row or column. Return indexes of this elements grouped in list.
   */
  def findCoincidence(line: Vector[Int]): List[List[Int]] = line
    .zipWithIndex
    .foldLeft(Map.empty[Int,List[Int]]) {
      case(acc, (value, index)) => acc + ((value, index :: acc.getOrElse(value, List.empty[Int])))
    }
    .toList
    .filter { case(_, list) => list.length > 1}
    .map { case (_, list) => list }

  /*
  Encode rows or columns in bit representation (1-black, 0-white).
   */
  def findLineNumbers(list: List[List[Int]]): List[Int] = {

    def findLineNumbersRec(list: List[List[Int]], acc: List[Int]): List[Int] = list match {
      case x::xs =>
        val newAcc: List[Int] = acc.flatMap(accElm =>
          x.map(v => 1 << v | accElm )
        )
        findLineNumbersRec(xs, newAcc)
      case Nil => acc
    }
    findLineNumbersRec(list, List(0))
  }

  def transposeMatrix(matrix: Matrix): Matrix = {
    null
  }

  println(findCoincidence(Vector(1,2,2,1)))
  println(findLineNumbers(findCoincidence(Vector(1, 2, 2, 1))))

}
