package top.harryxi.surreal
import scala.util.Using
import scala.collection.convert.*
import com.surrealdb.RecordId
import scala.annotation.targetName
import SurreaEncoder.given

extension [T](x: T) inline def |>[U](f: T => U) = f(x)

case class ClassA(n:Option[Int])

def q(db:Surreal) = 
  db.queryWith("Return $xx",("xx"->ClassA(Some(10))))(0).to[ClassA]

@main def main =
    println("hello world")
    Using(Surreal("surrealkv://db")) { db =>
      println(q(db))
    } |> println

