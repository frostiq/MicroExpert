package bazhanau.iis.microexpert.core

import bazhanau.iis.microexpert.entities.TypeDef.{Context, TargetsStack}
import bazhanau.iis.microexpert.entities._

/**
  * Created by a.bazhanau on 27.03.16.
  *
  */

class Core(rules: Set[Rule]) {

  def consult(attribute: Attribute): ConsultationResult = {
    consult(List(Target(attribute)), Map(), rules)
  }

  def consult(q: Question, answer: Value): ConsultationResult = {
    val ans = Statement(q.currentTarget, answer)
    consult(q.targets.tail, q.context + (ans.attribute -> ans), q.rules, afterAnswer = true)
  }

  def getTargets: Set[Attribute] = rules.flatMap(_.conclusion.statements).map(_.attribute)

  def getOptions(question: Question): Set[Value] =
    rules.flatMap(_.condition.statements).filter(_.attribute == question.currentTarget).map(_.value)

  private def consult(targets: TargetsStack, context: Context, rules: Set[Rule], afterAnswer: Boolean = false): ConsultationResult = {
    getRuleByTarget(targets.head, rules, afterAnswer) match {
      case Some(rule) => checkRule(rule, context) match {
        case Checked(true) => targets match {
          case _ :: Nil => getAnswer(rule.conclusion, targets.head.attribute)
          case _ => consult(targets.tail, applyRule(rule, context), rules - rule)
        }
        case Checked(false) => consult(targets, context, rules - rule)
        case Unknown(attr) => consult(Target(attr, rule.number) :: targets, context, rules)
      }
      case None => Question(targets, context, rules)
    }
  }

  private def getRuleByTarget(target: Target, rules: Set[Rule], afterAnswer: Boolean): Option[Rule] =
    if (afterAnswer && target.ruleNumber != 0) rules.find(_.number == target.ruleNumber)
    else rules.find(_.conclusion.statements.exists(_.attribute == target.attribute))

  trait CheckResult extends Product with Serializable

  case class Checked(isTruth: Boolean) extends CheckResult

  case class Unknown(attribute: Attribute) extends CheckResult

  private def checkRule(rule: Rule, context: Context): CheckResult =
    rule.condition.statements
      .map(s => context get s.attribute match {
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
