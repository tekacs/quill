package io.getquill.quotation

import scala.reflect.macros.whitebox.Context

import io.getquill.ast.PropertyAlias

case class EntityConfig(
  alias:      Option[String]      = None,
  properties: List[PropertyAlias] = List()
)

trait EntityConfigParsing {
  this: Parsing =>
  val c: Context

  import c.universe.{ Function => _, Ident => _, _ }

  def parseEntityConfig(t: Tree): EntityConfig =
    t match {
      case q"$e.entity(${ name: String })" =>
        parseEntityConfig(e).copy(alias = Some(name))
      case q"$e.columns(..$propertyAliases)" =>
        parseEntityConfig(e).copy(properties = propertyAliases.map(propertyAliasParser(_)))
      case _ =>
        EntityConfig()
    }

  private implicit val propertyAliasParser: Parser[PropertyAlias] = Parser[PropertyAlias] {
    case q"(($x1) => $pack.Predef.ArrowAssoc[$t]($x2.$prop).$arrow[$v](${ alias: String }))" =>
      PropertyAlias(prop.decodedName.toString, alias)
  }
}
