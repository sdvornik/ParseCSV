package com.yahoo.sdvornik
import java.time.{Instant, ZoneId}

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._
import org.junit.runner.RunWith
import org.scalatest.check.Checkers
import org.scalatest.junit.JUnitRunner

/**
  * @author Serg Dvornik <sdvornik@yahoo.com>
  */
@RunWith(classOf[JUnitRunner])
class TimeFormatterTest extends FunSuite with Checkers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 500, minSize = 0, sizeRange = 500)

  private val helper = TimeHelper(ZoneId.systemDefault())
  private val now = System.currentTimeMillis()

  import scala.concurrent.duration._
  private val interval = 1.day.toMillis * 365

  private val longGen: Gen[Long] = Gen.choose[Long](now - interval, now)

  private implicit def timeGen(implicit a: Arbitrary[Long]): Arbitrary[Long] = Arbitrary(longGen)

  test("Time string to long parsing") {
    check { (l: Long) => {
        import helper._
        val str: String = Instant.ofEpochMilli(l)
        val lNew: Instant = str
        val strNew: String = lNew

      strNew == str
    }}
  }
}