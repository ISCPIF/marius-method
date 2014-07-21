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

package fr.geocites.marius.calibration

import fr.geocites.marius.model._
import MariusFile._
import scala.util.{ Failure, Success, Try }
import math._

object Target {

  /**
   * Sum of square differences between logs of data
   *
   * @param d1 data set 1
   * @param d2 data set 2
   * @return the difference
   */
  def logSquaresError(d1: Seq[Double], d2: Seq[Double]) =
    (d1 zip d2) map {
      case (e, o) =>
        pow(log10(o) - log10(e), 2)
    } sum

  /**
   * Convert the simulation step to the actual simulation date
   *
   * @param step the simulation step
   * @return the actual simulated date
   */
  def date(step: Int) = dates.head + step

  /**
   * Compute the error between simulated and empirical data if any
   *
   * @param step the simulation step
   * @param cities the states of the simulated cities
   * @return the distance to data (0 if empirical data aren't filled)
   */
  def distanceToData(step: Int, cities: Seq[City]) =
    populations(dates.head + step).map {
      empirical => logSquaresError(cities.map(_.population).sorted, empirical.sorted)
    }.getOrElse(0.0)

  /**
   * Sum over an iterator of vector
   *
   * @param it the iterator
   * @return the sum of the contained vectors
   */
  def sum(it: Iterator[Seq[Double]]) = it.foldLeft(Seq(0.0, 0.0, 0.0)) { (s, v) => (s zip v).map { case (x, y) => x + y } }

}

import Target._

/**
 * First target of calibration in the paper
 */
object Target1 {

  /**
   * Compute a distance between simulated and empirical distribution of city populations over all simulation steps.
   *
   * @param marius the model to evaluate
   * @return the calibration error (infinity if an invalid state is reached by the model)
   */
  def error(marius: Marius): Double =
    Try {
      val fitness =
        (for { (state, step) <- marius.states.zipWithIndex } yield state match {
          case Success(s) => distanceToData(step, s.cities)
          case Failure(_) => Double.PositiveInfinity
        }).sum
      if (fitness.isNaN) Double.PositiveInfinity else fitness
    }.getOrElse(Double.PositiveInfinity)
}

/**
 * Second target of calibration in the paper
 */
object Target2 {

  /**
   * Compute a multi objective evaluation of a model. The 3 objectives are:
   *  - the number of cities with a nil wealth over all the simulation steps,
   *  - the distance between simulated and empirical distribution of city populations over all simulation steps,
   *  - the overflow of supply and demand over wealth summed over all the simulation steps.
   * @param marius the model to evaluate
   * @return an evaluation composed of 3 objectives
   */
  def error(marius: Marius): Array[Double] = Try {

    def totalOverflowRatio(cities: Seq[City]) =
      cities.map {
        c =>
          overflowRatio(c.wealth, marius.supply(c.population)) + overflowRatio(c.wealth, marius.demand(c.population))
      }.sum

    def overflowRatio(wealth: Double, flow: Double) = {
      val ratio = flow / wealth
      if (ratio < 1.0) 0.0 else ratio - 1
    }

    val fitness =
      sum(
        for { (state, step) <- marius.states.zipWithIndex } yield {
          state match {
            case Success(s) =>
              val overflow = totalOverflowRatio(s.cities)
              val deadCities = s.cities.count(_.wealth <= 0.0)
              val distance = distanceToData(step, s.cities)
              Seq(deadCities, distance, overflow)
            case Failure(_) => Seq(Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity)
          }
        }
      )

    fitness.map(x => if (x.isNaN) Double.PositiveInfinity else x)
  }.getOrElse(Seq(Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity)).toArray

}
