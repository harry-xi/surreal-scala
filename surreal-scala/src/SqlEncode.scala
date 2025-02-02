package top.harryxi.surreal

import scala.compiletime.ops.string
import org.apache.commons.text.StringEscapeUtils
import com.surrealdb.RecordId
import java.util.UUID
import scala.concurrent.duration.Duration
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

trait SurrealEncoder[T]:
  def encode(v: T): String

case class Raw(str: String)

given SurrealEncoder[Raw] with
  def encode(v: Raw) = v.str

object SurreaEncoder {
  import scala.deriving.*
  import scala.compiletime.*

  given SurrealEncoder[Double] with
    def encode(v: Double) = v.toString()

  given SurrealEncoder[Float] with
    def encode(v: Float) = v.toString()

  given SurrealEncoder[Int] with
    def encode(v: Int) = v.toString()

  given SurrealEncoder[Long] with
    def encode(v: Long) = v.toString()

  given SurrealEncoder[BigDecimal] with
    def encode(v: BigDecimal) = v.toString()

  given SurrealEncoder[Boolean] with
    def encode(v: Boolean): String =
      v.toString()

  given SurrealEncoder[Array[Byte]] with
    def encode(v: Array[Byte]): String =
      val base64 = java.util.Base64.getEncoder().encodeToString(v).dropRight(1)
      s"encoding::base64::decode(\"$base64\")"

  given SurrealEncoder[String] with
    // todo: make sure it is safe
    def encode(v: String) =
      "\"" + StringEscapeUtils.ESCAPE_JSON.translate(v) + "\""

  given SurrealEncoder[UUID] with 
    def encode(v: UUID): String = 
      "u\""+v.toString()+"\""

  given [D<:Duration]:SurrealEncoder[D] with
    def encode(v: D): String = 
      v.toNanos + "ns"

  given SurrealEncoder[ZonedDateTime] with
    def encode(v: ZonedDateTime): String = 
      "d\"" +v.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)+"\""

  given SurrealEncoder[RecordId] with
    def encode(v: RecordId): String =
      RecordIdHelper.toSqlStr(v)

  given [T](using s: SurrealEncoder[T]): SurrealEncoder[Option[T]] with
    def encode(v: Option[T]) = v match
      case Some(value) => s.encode(value)
      case None        => "null"

  given [T](using s: SurrealEncoder[T]): SurrealEncoder[List[T]] with
    override def encode(x: List[T]): String =
      if x.isEmpty then "[]" else x.map(s.encode).mkString("[", ", ", "]")

  given [T](using s: SurrealEncoder[T]): SurrealEncoder[Set[T]] with
    override def encode(x: Set[T]): String =
      if x.isEmpty then "<set>[]"
      else x.map(s.encode).mkString("<set>[", ", ", "]")

  inline given derived[T](using m: Mirror.Of[T]): SurrealEncoder[T] =
    val elemNames = summonNames[m.MirroredElemLabels]
    val elemInstances = summonInstances[m.MirroredElemTypes]
    val jsonInfo = elemNames.zip(elemInstances)
    inline m match {
      case _: Mirror.ProductOf[t] => productClass(jsonInfo)
      case s: Mirror.SumOf[T]     => sumClass(s, jsonInfo)
    }

  inline def summonNames[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => constValue[t].asInstanceOf[String] :: summonNames[ts]

  inline def summonInstances[T <: Tuple]: List[SurrealEncoder[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        summonInline[SurrealEncoder[t]] :: summonInstances[ts]

  def productClass[T](
      classInfo: => List[(String, SurrealEncoder[?])]
  ): SurrealEncoder[T] =
    new SurrealEncoder[T] {
      override def encode(v: T): String =
        val fieldIterator = v.asInstanceOf[Product].productIterator

        fieldIterator
          .zip(classInfo)
          .map { (value, info) =>
            val (name, instance) = info
            s"\"${name}\": ${instance.asInstanceOf[SurrealEncoder[Any]].encode(value)}"
          }
          .mkString("{", ", ", "}")
    }

  def sumClass[T](
      s: Mirror.SumOf[T],
      claassInfo: => List[(String, SurrealEncoder[?])]
  ): SurrealEncoder[T] =
    new SurrealEncoder[T] {
      override def encode(x: T): String = {
        val ord = s.ordinal(x)
        val (_, instance) = claassInfo(ord)
        instance.asInstanceOf[SurrealEncoder[Any]].encode(x)
      }
    }
}
