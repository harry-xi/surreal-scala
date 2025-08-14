package top.harryxi.surreal
import scala.util.Using
import com.surrealdb.RecordId
import top.harryxi.surreal.HelperOps.*

case class A(a:Int)

def q(db: Surreal) =
  db
    .using(a = Some(10),b=A(20))
    .query("return $a")(0).to[Int]

def q2(db: Surreal) =
  db.useNs("test")
    .useDb("test")
  db.queryWith("return $a;",("a"->None))
  db.create(
    "string",
    A(10)
  )



@main def main =
  println("hello world")
  Using(Surreal("memory")) { db =>
    println(q(db))
  } |> println
  Using(Surreal("memory")) { db =>
    println(q2(db))
  } |> println
