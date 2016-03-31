package bazhanau.iis.microexpert.core

import bazhanau.iis.microexpert.entities.TypeDef.{Rules, Context, TargetsStack}
import bazhanau.iis.microexpert.entities._

/**
  * Created by a.bazhanau on 27.03.16.
  *
  */

class Core(rules: Rules) {

  def consult(attribute: Attribute): ConsultationResult = {
    consult(List(Target(attribute)), Map(), rules)
  }

  def consult(q: Question, answer: Value): ConsultationResult = {
    val ans = Statement(q.currentTarget, answer)
    consult(q.targets.tail, q.context + (ans.attribute -> ans), q.rules, q.targets.head.ruleNumber)
  }

  def getTargets: Set[Attribute] = rules.flatMap(_.conclusion.statements).map(_.attribute)

  def getOptions(question: Question): Set[Value] =
    rules.flatMap(_.condition.statements).filter(_.attribute == question.currentTarget).map(_.value)

  private def consult(targets: TargetsStack, context: Context, rules: Rules, ruleNumAfterAnswer : Int = 0): ConsultationResult = {
    val ruleToProcess = if (ruleNumAfterAnswer != 0) rules.find(_.number == ruleNumAfterAnswer) else getRuleByTarget(targets.head, rules)
    ruleToProcess match {
      case Some(rule) => checkRule(rule, context) match {
        case Checked(true) => targets match {
          case _ :: Nil => getAnswer(rule.conclusion, targets.head.attribute)
          case _ => consult(targets.tail, applyRule(rule, context), rules - rule)
        }
        case Checked(false) => consult(targets, context, rules - rule)
        case Unknown(attr) => consult(Target(attr, rule.number) :: targets, context, rules)
      }
      case None => if (targets.tail.nonEmpty) Question(targets, context, rules) else NoAnswer()
    }
  }

  private def getRuleByTarget(target: Target, rules: Rules): Option[Rule] =
    rules.find(_.conclusion.statements.exists(_.attribute == target.attribute))

  private trait CheckResult extends Product with Serializable

  private case class Checked(isTruth: Boolean) extends CheckResult

  private case class Unknown(attribute: Attribute) extends CheckResult

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
