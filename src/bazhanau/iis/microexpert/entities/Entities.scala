package bazhanau.iis.microexpert.entities

/**
  * Created by a.bazhanau on 27.03.16.
  */
case class Value (value: String)
case class Attribute (value: String)
case class Predicate(value : String)
case class Sentence (attribute: Attribute, value: Value, predicate: Predicate = Predicate.Is)
case class Statement (sentences : Set[Sentence])
case class Rule(number : Int, condition : Statement, conclusion: Statement)

object Predicate{
  val Is = Predicate("IS")
}
