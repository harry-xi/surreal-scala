package top.harryxi.surreal

import com.surrealdb.Value

extension (v: Value) def to[T](using s: ValueDecoder[T]) = s.decode(v)

trait ValueDecoder[T]:
  def decode(value: Value): T

object ValueDecoder:
  import scala.deriving.*
  import scala.compiletime.*

  inline def summonNames[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => constValue[t].asInstanceOf[String] :: summonNames[ts]

  inline def summonInstances[T <: Tuple]: List[ValueDecoder[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) =>
        summonInline[ValueDecoder[t]] :: summonInstances[ts]

  given ValueDecoder[Int] with
    def decode(value: Value): Int =
      value.getLong().toInt

  given ValueDecoder[Long] with
    def decode(value: Value): Long =
      value.getLong()
  given ValueDecoder[Double] with
    def decode(value: Value): Double =
      value.getDouble()
  given ValueDecoder[Float] with
    def decode(value: Value): Float =
      value.getDouble().toFloat
  given ValueDecoder[Boolean] with
    def decode(value: Value): Boolean =
      value.getBoolean()

  given [T](using d:ValueDecoder[T]):ValueDecoder[Option[T]] with
    def decode(value: Value): Option[T] = 
      if value.isNull() then None
      else Some(d.decode(value))

  def newDecoderProduct[T](
      names: List[String],
      instances: List[ValueDecoder[?]]
  )(using m: Mirror.ProductOf[T]): ValueDecoder[T] =
    val info = names.zip(instances)

    new ValueDecoder[T]:
      override def decode(value: Value): T =
        val obj = value.getObject()
        info.map { (name, instance) =>
          instance.decode(obj.get(name))
        }.toArray 
        |> Tuple.fromArray 
        |> m.fromProduct

  inline given derived[T](using m: Mirror.Of[T]): ValueDecoder[T] =
    val names = summonNames[m.MirroredElemLabels]
    val instances = summonInstances[m.MirroredElemTypes]

    inline m match
      case p: Mirror.ProductOf[T] =>
        newDecoderProduct[T](names, instances)(using p)
