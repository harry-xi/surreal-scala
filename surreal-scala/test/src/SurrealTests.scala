package top.harryxi.surreal

import utest._
import scala.util.Using

object SurrealCast extends TestSuite {
  import scala.concurrent.duration.Duration
  import java.util.concurrent.TimeUnit
  import java.time.ZonedDateTime
  def tests = Tests {
    case class ClassA(n: List[String])
    test("list") {
      assert(Using(Surreal("memory")) { db =>
        db.queryWith(
          "return $s",
          ("s" -> ClassA(List("A", "L", "L", "A", "Y")))
        )(0)
          .to[ClassA]
      }.get == ClassA(List("A", "L", "L", "A", "Y")))
    }
    test("duration"){
      assert(Using(Surreal("memory")) { db =>
        db.queryWith(
          "RETURN $a",
          ("a" -> Duration.create(1000,TimeUnit.SECONDS))
        )(0)
          .to[Duration]
      }.get ==  Duration(1000,TimeUnit.SECONDS))
    }
    test("data time"){
      val zdt = ZonedDateTime.now()
      val back = Using(Surreal("memory")) { db =>
        db.queryWith(
          "RETURN $a",
          ("a" -> zdt)
        )(0)
          .to[ZonedDateTime]
      }.get
      assert(zdt.isEqual(back))
    }
    test("some"){
      assert(Using(Surreal("memory")) { db =>
        db.queryWith(
          "return $a",
          ("a" -> Some("hello"))
        )(0)
          .to[String]
      }.get == "hello")
    }
    test("none"){
      val res = Using(Surreal("memory")) { db =>
        db.queryWith(
          "return $a",
          ("a" -> None)
        )(0)
      }.get
      assert(res.isNull())
    }
  }
}
