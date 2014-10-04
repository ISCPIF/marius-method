/*
 * Copyright (C) 2014 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.geocites.marius.model

import java.io.File

import fr.geocites.marius.calibration.BonusFixedCostModel
import fr.geocites.marius.model.MariusFile._

import scalax.io.Resource

object TestModel extends BonusFixedCostModel(
  bonusMultiplier = 197.948890779081,
  fixedCost = 0.256524806806571,
  distanceDecay = 0.672263161474518,
  sizeEffectOnSupply = 1.00175638801509,
  sizeEffectOnDemand = 1.07926078029368,
  economicMultiplier = 0.34380934416368,
  populationToWealthExponent = 1.08660127543912,
  wealthToPopulationExponent = 0.380435604357353
)

object  Run extends App {

  val path = new File("/tmp/mariusmodel_log.csv")
  path.delete

  val out = Resource.fromFile(path)
  out.append("step, arokato, population, wealth \n")

  for {
    (s, i) <- TestModel.states.zipWithIndex
    ss <- s
  } {
    val cities = ss.cities

    for {
      (city, arokato) <- (cities zip arokatos)
    } {
      def line = Seq(i, arokato, city.population, city.wealth)
      out.append(line.mkString("", ",", "\n"))
    }
    val totalWealth = cities.map(_.wealth).sum
    val totalPop = cities.map(_.population).sum

    println(s"State $i, total wealth $totalWealth, total population $totalPop")
  }
}
