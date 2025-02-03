package top.harryxi.surreal
import scala.util.Using
import scala.collection.convert.*
import com.surrealdb.RecordId
import scala.annotation.targetName
import SurreaEncoder.given


extension [T](x: T) inline def |>[U](f: T => U) = f(x)

case class ClassA(id:RecordId,n:Option[Int])
case class ClassB(id:String,n:Option[Int])
def q(db:Surreal) = 
  db.query("return book:{aa:b:`a⟩l⟨l`}")(0).to[RecordId]

@main def main =
    println("hello world")
    Using(Surreal("surrealkv://db")) { db =>
      println(q(db))
    } |> println
