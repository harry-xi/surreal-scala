package top.harryxi.surreal
import scala.util.Using
import com.surrealdb.RecordId


extension [T](x: T) inline def |>[U](f: T => U) = f(x)

case class A(a:Int)

def q(db: Surreal) =
  db
    .using(a = Some(10),b=A(20))
    .query("return $a")

def q2(db: Surreal) =
  db.queryWith("return $a;",("a"->None))

@main def main =
  println("hello world")
  Using(Surreal("memory")) { db =>
    println(q(db))
  } |> println
  Using(Surreal("memory")) { db =>
    println(q2(db))
  } |> println