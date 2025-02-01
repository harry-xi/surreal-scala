package top.harryxi.surreal


import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror
import com.surrealdb.Response


object Surreal {
  def apply(url:String,ns:String="root",db:String="root") = 
    new Surreal(com.surrealdb.Surreal()
    .connect(url)
    .useNs(ns).useDb(db))
  
  extension (res:Response)
    def toSeq = Range(0,res.size()).map(res.take(_))

  inline def summonVars[T <: Tuple](args: T): List[(String, String)] =
    inline args match
      case _: EmptyTuple => Nil
      case tup: (Tuple2[String, t] *: ts) =>
        (
          tup.head.asInstanceOf[(String, t)]._1,
          summonInline[SurrealEncoder[t]]
            .encode(tup.head.asInstanceOf[(String, t)]._2)
        )
          :: summonVars[Tuple.Tail[T]](args.tail)
      case _ =>
        scala.compiletime.error(
          "the vars should be a tuple of (string,?:SurrealEncoder)"
        )

  def buildPerDef(list: List[(String, String)]): String =
    list.map((name, value) => s"let $$$name = $value;\n").mkString("")
  }


class Surreal(val db:com.surrealdb.Surreal) extends AutoCloseable {
  import Surreal.*
  
  inline def queryWith[Vars <: Tuple](sql: String, vars: Vars) =
    inline vars match
      case _: EmptyTuple => db.query(sql).toSeq
      case tup: Tuple2[String, t] =>
        db.query(
          buildPerDef(
            List(
              (
                tup.head.asInstanceOf[String],
                summonInline[SurrealEncoder[t]]
                  .encode(tup._2)
              )
            )
          ) + sql
        ).toSeq.drop(1)
      case _: (Tuple2[String, t] *: ts) =>
        db.query(buildPerDef(summonVars(vars)) + sql).toSeq.drop(vars.size)
      case _ =>
        scala.compiletime.error(
          "the vars should be a tuple of (string,?:SurrealEncoder)"
        )
  def query(sql: String) = db.query(sql)



  override def close(): Unit =
    db.close()
}
