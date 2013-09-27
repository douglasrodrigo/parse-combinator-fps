package fps

import scala.util.parsing.combinator.RegexParsers

import fps.model._

import org.joda.time.format._
import org.joda.time.LocalDateTime

object Parser extends RegexParsers {

  private val dateTimeFormat = DateTimeFormat.forPattern("dd/MM/yyyy HH:mm:ss")
  
  def time: Parser[LocalDateTime] =  """\d{2}/\d{2}/\d{4} \d{2}:\d{2}:\d{2}""".r ^^ {LocalDateTime.parse(_, dateTimeFormat) }

  def timeSep = time ~ "-"

  def matchId = """(\d+)""".r

  def newMatch = timeSep ~> "New match" ~> matchId <~  "has started"

  def endMatch = timeSep ~> "Match" ~> matchId <~ "has ended"

  def player: Parser[Player] = """(\S+)""".r ^^ { Player(_)}

  def weapon: Parser[Weapon] = """(\w+)""".r ^^ { Weapon(_)}

  def kill: Parser[PlayerKill]  = timeSep ~ player ~ "killed" ~ player ~ "using" ~ weapon ^^ 
    {case (time ~ _) ~ killer ~ _ ~ dead  ~ _ ~ weapon => PlayerKill(time, killer, dead, weapon)}

  def killByWorld: Parser[WorldKill]  = timeSep ~ "<WORLD> killed" ~  player ~ "by DROWN" ^^ 
    {case (time ~ _) ~ _ ~  dead  ~ _  => WorldKill(time, dead)}

  def kills: Parser[List[Kill]] = rep(kill | killByWorld) ^^ { (kills: List[Kill]) => kills }

  def fullMatch: Parser[Match] = newMatch ~ kills ~  endMatch ^^ {
    case newMatchId ~ kills ~ endMatchId if newMatchId == endMatchId => Match(newMatchId, kills)
  }

  def matches: Parser[List[Match]] = rep(fullMatch)

}
