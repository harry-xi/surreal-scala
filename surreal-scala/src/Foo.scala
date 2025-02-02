package top.harryxi.surreal
import scala.util.Using
import scala.collection.convert.*
import com.surrealdb.RecordId
import scala.annotation.targetName
import SurreaEncoder.given


extension [T](x: T) inline def |>[U](f: T => U) = f(x)

case class ClassA(n:Option[Int])

def q(db:Surreal) = 
  db.queryWith("RETURN <set>$a",("a"->List(1,2,3,4,5,6,6,7)))(0).to[List[Long]]


@main def main =
    println("hello world")
    Using(Surreal("surrealkv://db")) { db =>
      println(q(db))
    } |> println
