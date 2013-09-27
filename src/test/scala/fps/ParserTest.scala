package fps

import org.specs2.mutable._
import org.specs2.matcher.ParserMatchers
import fps.Parser.{time, kill, killByWorld, newMatch, endMatch, kills, fullMatch}

class ParserTest extends Specification with ParserMatchers {
  val parsers = fps.Parser

  "Parser time" should {
    "succeed to recognize a time" in {
      time("23/04/2013 15:34:22") must beASuccess
    }

    "fail on invalid datetime format" in {
      time must failOn("39393")
    }
  }

  "Parser kill" should {
    "succeed to recognize a kill" in {
      kill("23/04/2013 15:34:22 - Roman killed Nick using M16") must beASuccess
    }

    "succeed to recognize a killl with a alphanumber and special chars player " in {
      kill("23/04/2013 15:34:22 - Roman98$ killed Nick using M16") must beASuccess
    }

    "fail on invalid sentences" in {
      kill must failOn("23/04/2013 15:34:22 - Roman k Nick us M16")
    }
  }

  "Parser killByWorld" should {
    "succeed to recognize a kill by world" in {
      killByWorld("23/04/2013 15:34:22 - <WORLD> killed Nick by DROWN") must beASuccess
    }

    "fail on invalid sentences" in {
      killByWorld must failOn("23/04/2013 15:34:22 - killed by")
    }
  }

  "Parser kills" should {
    "succeed to recognize kills" in {
      kills("""23/04/2013 15:34:22 - Roman killed Nick using M16
            23/04/2013 15:34:22 - <WORLD> killed Nick by DROWN""") must beASuccess
    }

    "fail on invalid kills" in {
      kills must failOn("""23/04/2013 15:34:22 - Ro by DROWN""")
    }
  }

  "Parser newMatch" should {
    "succeed to recognize a new match" in {
      newMatch("23/04/2013 15:34:22 - New match 11348965 has started") must beASuccess
    }

    "fail on invalid sentences" in {
      newMatch must failOn("23/04/2013 15:34:22 - new match")
    }
  }

  "Parser endMatch" should {
    "succeed to recognize a end match" in {
      endMatch("23/04/2013 15:34:22 - Match 11348965 has ended") must beASuccess
    }

    "fail on invalid sentences" in {
      endMatch must failOn("23/04/2013 15:34:22 - Match ended")
    }
  }

  "Parser fullMatch" should {
    "succeed to recognize full match" in {
      fullMatch("""23/04/2013 15:34:22 - New match 11348965 has started
            23/04/2013 15:35:22 - Roman killed Nick using M16
            23/04/2013 15:35:23 - <WORLD> killed Nick by DROWN
            23/04/2013 15:36:22 - Match 11348965 has ended""") must beASuccess
    }

    "succeed to recognize full match without kills" in {
      fullMatch("""23/04/2013 15:34:22 - New match 11348965 has started
            23/04/2013 15:34:22 - Match 11348965 has ended""") must beASuccess
    }

  }

}
