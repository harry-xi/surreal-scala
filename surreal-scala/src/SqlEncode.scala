package top.harryxi.surreal

import com.surrealdb.RecordId
import com.surrealdb.ValueMut
import com.surrealdb.EntryMut
import java.util.UUID
import scala.concurrent.duration.Duration
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.collection.JavaConverters.*

trait SurrealEncoder[T]:
  def encode(v: T): ValueMut

object SurreaEncoder {
  import scala.deriving.*
  import scala.compiletime.*

  given SurrealEncoder[Double] with
    def encode(v: Double) = ValueMut.createDouble(v)

  given SurrealEncoder[Float] with
    def encode(v: Float) = ValueMut.createDouble(v.toDouble)

  given SurrealEncoder[Int] with
    def encode(v: Int) = ValueMut.createLong(v.toLong)

  given SurrealEncoder[Long] with
    def encode(v: Long) = ValueMut.createLong(v)

  given SurrealEncoder[BigDecimal] with
    def encode(v: BigDecimal) = ValueMut.createBigDecimal(v.bigDecimal)

  given SurrealEncoder[Boolean] with
    def encode(v: Boolean) = ValueMut.createBoolean(v)

  given stringEncoedr: SurrealEncoder[String] with
    def encode(v: String) = ValueMut.createString(v)

  given [D <: Duration]: SurrealEncoder[D] with
    def encode(v: D) = 
      v.toNanos 
      |> java.time.Duration.ofNanos
      |> ValueMut.createDuration

  given SurrealEncoder[ZonedDateTime] with
    def encode(v: ZonedDateTime) = ValueMut.createDatetime(v)

  given SurrealEncoder[RecordId] with
    def encode(v: RecordId) = ValueMut.createThing(v)

  given [T](using s: SurrealEncoder[T]): SurrealEncoder[Option[T]] with
    def encode(v: Option[T]) = v match
      case Some(value) => s.encode(value)
      case None        => null // todo: make sure it result in a surreal Null not None

  given [T](using s: SurrealEncoder[T]): SurrealEncoder[List[T]] with
    override def encode(x: List[T]) =
      x.map(s.encode).asJava |> ValueMut.createArray

  inline given derived[T](using m: Mirror.Of[T]): SurrealEncoder[T] =
    val elemNames = summonNames[m.MirroredElemLabels]
    val elemInstances = summonInstances[m.MirroredElemTypes]
    val info = elemNames.zip(elemInstances)
    inline m match {
      case _: Mirror.ProductOf[t] => productClass(info)
      // case s: Mirror.SumOf[T]     => sumClass(s, jsonInfo)
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
      override def encode(v: T) =
        val fieldIterator = v.asInstanceOf[Product].productIterator

        fieldIterator
          .zip(classInfo)
          .map { (value, info) =>
            val (name, instance) = info
            EntryMut.newEntry(name,instance.asInstanceOf[SurrealEncoder[Any]].encode(value))
          }.toList.asJava
        |> ValueMut.createObject
    }

  def sumClass[T](
      s: Mirror.SumOf[T],
      claassInfo: => List[(String, SurrealEncoder[?])]
  ): SurrealEncoder[T] =
    new SurrealEncoder[T] {
      override def encode(x: T) = {
        val ord = s.ordinal(x)
        val (_, instance) = claassInfo(ord)
        instance.asInstanceOf[SurrealEncoder[Any]].encode(x)
      }
    }
}
