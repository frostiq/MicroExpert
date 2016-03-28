package bazhanau.iis.microexpert.core

import bazhanau.iis.microexpert.core.CoreTypes._
import bazhanau.iis.microexpert.entities._

/**
  * Created by a.bazhanau on 27.03.16.
  *
  */

package object CoreTypes {
  type TargetsStack = List[(Attribute, Int)]
  type Context = Map[Attribute, Statement]

  trait ConsultationResult

  case class Answer(statement: Statement) extends ConsultationResult

  case class NoAnswer() extends ConsultationResult

  case class Question(targets: TargetsStack, context: Context, rules: Set[Rule]) extends ConsultationResult
}

class Core(rules: Set[Rule]) {

  def consult(attribute: Attribute): ConsultationResult = {
    consult(List((attribute, 0)), Map(), rules)
  }

  def consult(q: Question, answer: Value): ConsultationResult = {
    val ans = Statement(q.targets.head._1, answer)
    consult(q.targets.tail, q.context + (ans.attribute -> ans), q.rules, afterAnswer = true)
  }

  def getTargets: Set[Attribute] = rules.flatMap(_.conclusion.statements).map(_.attribute)

  def getOptions(question: Question): Set[Value] =
    rules.flatMap(_.condition.statements).filter(_.attribute == question.targets.head._1).map(_.value)

  private def consult(targets: TargetsStack, context: Context, rules: Set[Rule], afterAnswer: Boolean = false): ConsultationResult = {
    getRuleByTarget(targets.head, rules, afterAnswer) match {
      case Some(rule) => checkRule(rule, context) match {
        case Checked(true) => targets match {
          case _ :: Nil => getAnswer(rule.conclusion, targets.head._1)
          case _ => consult(targets.tail, applyRule(rule, context), rules - rule)
        }
        case Checked(false) => consult(targets, context, rules - rule)
        case Unknown(attr) => consult((attr, rule.number) :: targets, context, rules)
      }
      case None => Question(targets, context, rules)
    }
  }

  private def getRuleByTarget(target: (Attribute, Int), rules: Set[Rule], afterAnswer: Boolean): Option[Rule] =
    if (!afterAnswer) rules.find(_.conclusion.statements.exists(_.attribute == target._1))
    else rules.find(_.number == target._2)

  trait CheckResult extends Product with Serializable
  case class Checked(isTruth: Boolean) extends CheckResult
  case class Unknown(attribute: Attribute) extends CheckResult

  private def checkRule(rule: Rule, context: Context): CheckResult =
    rule.condition.statements.map(s => context get s.attribute match {
      case Some(x) => Checked(x.value == s.value)
      case None => Unknown(s.attribute)
    })
      .fold(Checked(true))((a: CheckResult, b: CheckResult) => a match {
        case Unknown(attr) => Unknown(attr)
        case Checked(false) => Checked(false)
        case _ => b
      })


  private def applyRule(rule: Rule, context: Context): Context = {
    context ++ rule.conclusion.statements.map(s => (s.attribute, s))
  }

  private def getAnswer(conclusion: Sentence, target: Attribute): ConsultationResult =
    conclusion.statements.find(_.attribute == target) match {
      case Some(statement) => Answer(statement)
      case _ => NoAnswer()
    }

}
