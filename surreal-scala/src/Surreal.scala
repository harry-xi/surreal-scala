package top.harryxi.surreal

import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror
import scala.NamedTuple

import scala.collection.JavaConverters.*

import com.surrealdb.{Response, RecordId, ValueMut, EntryMut}
import top.harryxi.surreal.SurrealEncoder.{summonNames, summonInstances}

object Surreal {

  def apply(url: String, ns: String = "root", db: String = "root") =
    new Surreal(
      com.surrealdb
        .Surreal()
        .connect(url)
        .useNs(ns)
        .useDb(db)
    )

  extension (res: Response) def toSeq = Range(0, res.size()).map(res.take(_))

  inline def summonVars[T <: Tuple](args: T): List[(String, ValueMut)] =
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

}

class Surreal(val db: com.surrealdb.Surreal) extends AutoCloseable {
  import Surreal.*

  inline def queryWith[Vars <: Tuple](sql: String, vars: Vars) =
    inline vars match
      case _: EmptyTuple => db.query(sql).toSeq
      case tup: Tuple2[String, t] =>
        val value = summonInline[SurrealEncoder[t]].encode(tup._2)
        db.queryBind(sql, Map(tup._1 -> value).asJava).toSeq
      case _: (Tuple2[String, t] *: ts) =>
        db.queryBind(sql, summonVars[Vars](vars).toMap.asJava).toSeq
      case _ =>
        scala.compiletime.error(
          "the vars should be a tuple of (string,?:SurrealEncoder)"
        )

  inline def using[
      Names <: Tuple,
      Values <: Tuple,
      Vars <: NamedTuple.NamedTuple[Names, Values]
  ](vars: Vars) =
    val elemNames = summonNames[Names]
    val elemInstances = summonInstances[Values]
    val classInfo = elemNames.zip(elemInstances)
    val fieldIterator = vars.asInstanceOf[Product].productIterator

    val queryArgs = fieldIterator
      .zip(classInfo)
      .map { (value, info) =>
        val (name, instance) = info
        (name, instance.asInstanceOf[SurrealEncoder[Any]].encode(value))
      }
      .toMap
      .asJava
    QueryContext(db, queryArgs)

  def query(sql: String) = db.query(sql).toSeq

  class QueryContext(
      private val db: com.surrealdb.Surreal,
      private val ctx: java.util.Map[String, ?]
  ) {
    def query(query: String) = db.queryBind(query, ctx).toSeq
  }
  override def close(): Unit =
    db.close()
}
