package fps.model

import fps.model._
import org.specs2.mutable._

class MatchTest extends Specification {
  
  val black = Player("Black")
  val mocoto = Player("Mocoto")
  val borg = Player("Borg")
  
  val ak47 = Weapon("ak47")
  val r16 = Weapon("r16")

  val blackOnBorgKill = PlayerKill(null, killer=black, dead=borg, weapon=r16)
  val borgOnBlackKill = PlayerKill(null, killer=borg, dead=black, weapon=ak47)
  val blackOnMocotoKill = PlayerKill(null, killer=black, dead=mocoto, weapon=r16)

  val simpleMatch = Match("123", List[Kill](blackOnBorgKill, borgOnBlackKill, blackOnMocotoKill))

  "Match ranking" should {
    val ranking =  simpleMatch.ranking

    "contain 3 statistics of players in a ranking" in {
      ranking must have size(3)
    }

    "ranking ordered by kills" in {
      ranking.head.player must beEqualTo(black)
    }
  }
}
