package org.monarchinitiative.boomer

import io.circe.{Encoder, Json}
import io.circe.generic.semiauto._
import org.semanticweb.owlapi.model.{OWLAnnotationValue, OWLLiteral}

final case class OBOGraphs(graphs: List[OBOGraph])

final case class OBOGraph(id: ID, nodes: List[Node], edges: List[Edge], equivalentNodesSets: List[NodeSet])

final case class Node(id: ID, `type`: String = "CLASS", lbl: Option[String])

final case class Edge(sub: ID, pred: ID, obj: ID, meta: Meta)

final case class NodeSet(nodeIds: List[ID])

final case class ID(id: String)

object ID {

  implicit val encoder: Encoder[ID] = Encoder.encodeString.contramap(_.id)

}

final case class Meta(basicPropertyValues: List[PropertyValue])

final case class PropertyValue(pred: ID, `val`: OWLAnnotationValue)

object PropertyValue {

  implicit val valueEncoder: Encoder[OWLAnnotationValue] = new Encoder[OWLAnnotationValue] {

    override def apply(value: OWLAnnotationValue): Json =
      value match {
        case literal: OWLLiteral =>
          if (literal.isBoolean) Encoder.encodeBoolean(literal.parseBoolean())
          else if (literal.isInteger) Encoder.encodeInt(literal.parseInteger())
          else if (literal.isFloat) Encoder.encodeFloat(literal.parseFloat())
          else if (literal.isDouble) Encoder.encodeDouble(literal.parseDouble())
          else if (literal.isLiteral) Encoder.encodeString(literal.getLiteral)
          else Encoder.encodeString(literal.toString)
        case other => Encoder.encodeString(value.toString)
      }

  }

  implicit val encoder: Encoder[PropertyValue] = deriveEncoder

}
