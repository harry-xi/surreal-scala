package top.harryxi.surreal

import utest._
import scala.util.Using

object SurrealCast extends TestSuite {
  import scala.concurrent.duration.Duration
  import java.util.concurrent.TimeUnit
  import java.time.ZonedDateTime
  import SurreaEncoder.given
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
    test("bytes") {
      assert(Using(Surreal("memory")) { db =>
        db.queryWith(
          "RETURN $a",
          ("a" -> Array[Byte](104, 101, 108, 108, 111))
        )(0)
          .toString()
      }.get == "encoding::base64::decode(\"aGVsbG8\")")
    }
    test("the sdk forget to impl getBytes") {
      intercept[java.lang.UnsatisfiedLinkError] {
        Using(Surreal("memory")) { db =>
          db.queryWith(
            "RETURN $a",
            ("a" -> Array[Byte](104, 101, 108, 108, 111))
          )(0)
            .to[Array[Byte]]
        }
      }
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
    test("set"){
      assert(
      Using(Surreal("memory")){ db =>
        db.queryWith("return $a",("a"->Set("a","b","c","c")))(0).to[Set[String]]
      }.get == Set("a","b","c")
    )}
  }
}
