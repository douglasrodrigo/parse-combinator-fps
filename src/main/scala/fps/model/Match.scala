package fps.model

import org.joda.time.LocalDateTime

case class Player(name: String)
case class Weapon(name: String)

trait Kill {
  def dead: Player
  def time: LocalDateTime
}

case class PlayerKill(time: LocalDateTime, killer: Player, dead: Player, weapon: Weapon) extends Kill
case class WorldKill(time: LocalDateTime, dead: Player) extends Kill


case class Match(id: String, kills: List[Kill]) {

  case class PlayerStatistics(player: Player)(relatedKills: List[PlayerKill], relatedDeaths: List[Kill]) {
    def kills = relatedKills size
    
    def deaths = relatedDeaths size
    
    def notDiedAward = relatedDeaths isEmpty

    def preferredWeapon = relatedKills.groupBy(_.weapon).mapValues(_.size).maxBy(_._2)._1
  }

  lazy val statistics = {
    val deads = kills.groupBy(_.dead) 
    val killers = kills.groupBy({case kill: PlayerKill => kill.killer}).asInstanceOf[Map[Player, List[PlayerKill]]]
    
    (deads.keySet ++ killers.keySet) map (player => 
      PlayerStatistics(player)(killers.getOrElse(player, List.empty), deads.getOrElse(player, List.empty))
    )
  }

  def ranking = statistics.toList sortBy(_.kills) reverse
  
  def winnerPreferedWeapon = ranking.head.preferredWeapon
}
