package bazhanau.iis.microexpert.entities

import bazhanau.iis.microexpert.entities.TypeDef.{Rules, TargetsStack, Context}

/**
  * Created by a.bazhanau on 27.03.16.
  */
case class Attribute (value: String)
case class Predicate(value : String)
case class Statement(attribute: Attribute, value: String, predicate: Predicate = Predicate.Is){
  override def toString: String = s"${attribute.value} ${predicate.value} $value"
}
case class Sentence(statements : Set[Statement])
case class Rule(number : Int, condition : Sentence, conclusion: Sentence)

trait ConsultationResult
case class Answer(statement: Statement) extends ConsultationResult
case class NoAnswer() extends ConsultationResult
case class Question(targets: TargetsStack, context: Context, rules: Rules) extends ConsultationResult {
  def currentTarget = targets.head.attribute
}

case class Target(attribute: Attribute, ruleNumber: Int = 0)

package object TypeDef {
  type Rules = Set[Rule]
  type TargetsStack = List[Target]
  type Context = Map[Attribute, Statement]
}

object Predicate{
  val Is = Predicate("IS")
}
