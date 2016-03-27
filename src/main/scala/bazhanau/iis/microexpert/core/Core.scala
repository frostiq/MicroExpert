package bazhanau.iis.microexpert.core

import bazhanau.iis.microexpert.entities._

/**
  * Created by a.bazhanau on 27.03.16.
  */
class Core(rules : Set[Rule]) {

  case class Result()
  case class Question(targets : TargetsStack, context: Context, rules : Set[Rule]) extends Result
  case class Answer(statement: Statement) extends Result
  case class NoAnswer() extends Result

  case class CheckResult()
  case class Checked(isTruth : Boolean) extends CheckResult
  case class Unknown(attribute: Attribute) extends CheckResult


  type TargetsStack = List[(Attribute, Int)]
  type Context = Map[Attribute, Statement]

  def consult(question: Question) = {
    consult(question.targets, question.context, question.rules)
  }

  private def consult(targets : TargetsStack, context: Context, rules : Set[Rule]) : Result = {
      getRuleByTarget(targets.head, rules).headOption match {
        case Some(rule) => checkRule(rule, context) match {
          case Checked(true) => targets match{
            case _ :: Nil => getAnswer(rule.conclusion, targets.head._1)
            case _ => consult(targets.tail, applyRule(rule, context), rules - rule)
          }
          case Checked(false) => consult(targets, context, rules - rule)
          case Unknown(attr) => consult((attr, rule.number) :: targets, context, rules)
        }
        case None => Question(targets, context, rules)
      }
  }

  private def getRuleByTarget(target: (Attribute, Int), rules: Set[Rule]) : Set[Rule] =
    if(target._2 == 0) rules.filter(_.conclusion.statements.exists(_.attribute == target))
    else Set(rules.find(_.number == target._2).get)

  private def checkRule(rule: Rule, context: Context) : CheckResult =
    rule.condition.statements.map(s => context get s.attribute match {
      case Some(x) => Checked(x.value == s.value)
      case None => Unknown(s.attribute)
    })
    .fold(Checked(true))((a : CheckResult, b : CheckResult) => a match {
      case Unknown(attr) => Unknown(attr)
      case Checked(false) => Checked(false)
      case _ => b
    })


  def applyRule(rule: Rule, context: Context) : Context = {
    context ++ rule.conclusion.statements.map(s => (s.attribute, s))
  }

  def getAnswer(conclusion: Sentence, target: Attribute) : Result =
    conclusion.statements.find(_.attribute == target) match {
      case Some(statement) => Answer(statement)
      case _ => NoAnswer()
    }

}
