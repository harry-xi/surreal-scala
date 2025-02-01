package top.harryxi.surreal

import utest._
import scala.util.Using

object FooTests extends TestSuite {
  def tests = Tests {
    case class ClassA(n:Int)
    test("simple") {
      assert(Using(Surreal("memory")) { db =>
        import SurreaEncoder.given
        db.queryWith("return $bb",("bb"->ClassA(10)))(0).to[ClassA]
      }.get ==  ClassA(10))
    }
  }
}
