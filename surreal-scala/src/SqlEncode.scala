package top.harryxi.surreal

import scala.compiletime.ops.string
import org.apache.commons.text.StringEscapeUtils

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

  @deprecated
  given SurrealEncoder[String] with
    // todo: make sure it is safe
    def encode(v: String) = "\"" + StringEscapeUtils.ESCAPE_ECMASCRIPT.translate(v) + "\""

  given SurrealEncoder[Boolean] with
    def encode(v: Boolean) = v.toString()

  given [T](using s: SurrealEncoder[T]): SurrealEncoder[Option[T]] with
    def encode(v: Option[T]) = v match
      case Some(value) => s.encode(value)
      case None        => "null"

  given [T](using s: SurrealEncoder[T]): SurrealEncoder[List[T]] with
    override def encode(x: List[T]): String =
      if x.isEmpty then "[]" else x.map(s.encode).mkString("[", ", ", "]")
  given [T](using s: SurrealEncoder[T]): SurrealEncoder[Set[T]] with
    override def encode(x: Set[T]): String =
      if x.isEmpty then "<set>[]" else x.map(s.encode).mkString("<set>[", ", ", "]")

  inline given derived[T](using m: Mirror.Of[T]): SurrealEncoder[T] =
    lazy val elemNames = summonNames[m.MirroredElemLabels]
    lazy val elemInstances = summonInstances[m.MirroredElemTypes]
    lazy val jsonInfo = elemNames.zip(elemInstances)
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
