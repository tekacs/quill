package io.getquill.source.sql.mirror

import java.util.Date
import io.getquill.source.mirror.Row
import scala.reflect.ClassTag

trait MirrorDecoders {
  this: mirrorSource.type =>

  private def decoder[T: ClassTag]: Decoder[T] = new Decoder[T] {
    def apply(index: Int, row: Row) =
      row[T](index)
  }

  implicit val stringDecoder: Decoder[String] = decoder[String]
  implicit val bigDecimalDecoder: Decoder[BigDecimal] = decoder[BigDecimal]
  implicit val booleanDecoder: Decoder[Boolean] = decoder[Boolean]
  implicit val byteDecoder: Decoder[Byte] = decoder[Byte]
  implicit val shortDecoder: Decoder[Short] = decoder[Short]
  implicit val intDecoder: Decoder[Int] = decoder[Int]
  implicit val longDecoder: Decoder[Long] = decoder[Long]
  implicit val floatDecoder: Decoder[Float] = decoder[Float]
  implicit val doubleDecoder: Decoder[Double] = decoder[Double]
  implicit val byteArrayDecoder: Decoder[Array[Byte]] = decoder[Array[Byte]]
  implicit val dateDecoder: Decoder[Date] = decoder[Date]
}
