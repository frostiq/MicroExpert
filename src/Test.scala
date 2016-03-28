import bazhanau.iis.microexpert.core.Core
import bazhanau.iis.microexpert.entities.Attribute
import bazhanau.iis.microexpert.parsers.RulesParser
import bazhanau.iis.microexpert.entities.Value

val str = scala.io.Source.fromFile("C:\\Source\\university\\IIS_MicroExpert\\src\\main\\resources\\rules.txt").mkString
val rules = RulesParser.apply(str)
val core = new Core(rules.get.toSet)
val attr = Attribute("type")
val qu = core.consult(attr)
