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

import fr.geocites.marius.model.DenseMatrix.Cell

/**
 * Bonus component which could be injected in the model
 */
trait Bonus { marius: Marius =>
  /** Factor adjusting bonus value */
  def bonusMultiplier: Double

  /**
   * Compute the bonuses given the diversity and the volume of transactions.
   *
   * @param t the transactions
   * @return a sequence of bonuses indexed by city indexes
   */
  override def bonuses(t: Transacted): Seq[Double] = {
    /** Measure of the exchange partners diversities */
    def diversities = {
      def transactedWith(transacted: Seq[Cell]) =
        transacted.filter { case Cell(_, v) => v > 0 }.map { case Cell(to, _) => to }

      (t.transacted.lines zip t.transposedTransacted.lines) map {
        case (from, to) =>
          (transactedWith(from).toSet union transactedWith(to).toSet).size / t.nbCities.toDouble
      }
    }

    /** Volume of incoming transactions */
    def importVolumes =
      for {
        (demand, i) <- t.demands.zipWithIndex
      } yield t.transactedToSum(i)

    /** Volume of outgoing transactions */
    def exportVolumes =
      for {
        (supply, i) <- t.supplies.zipWithIndex
      } yield t.transactedFromSum(i)

    (importVolumes zip exportVolumes zip diversities) map {
      case ((importVolume, exportVolume), diversity) =>
        bonusMultiplier * (importVolume + exportVolume) * diversity
    }
  }

}
