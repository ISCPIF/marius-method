/*
 * Copyright (C) 21/05/13 Romain Reuillon
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package fr.geocites.marius.model

import scala.io.Source

object MariusFile {

  /** Read the content of the file */
  def content = {
    val input =
      Source.fromInputStream(this.getClass.getClassLoader.getResourceAsStream("fr/geocites/marius/marius.csv"))

    input.getLines.map {
      l => l.split(",").toSeq
    }
  }

  /** Read the header of the csv file */
  def header = content.next

  /** Read the data part of the csv file */
  def data = content.drop(1).toList

  /** The number of columns of census data */
  def numberOfDates = 6

  /** The dates of the census */
  lazy val dates = MariusFile.header.takeRight(numberOfDates).map(_.toInt)

  /** The cities with known populations for all dates */
  def startingCities =
    data.filter {
      _.takeRight(numberOfDates).forall(!_.isEmpty)
    }

  /** Number of cities taken into account */
  def nbCities = startingCities.size

  /** Read the position of the cities */
  def positions =
    startingCities.map {
      l => Position(l(5).toDouble, l(4).toDouble)
    }

  /** Number of column before the census columns */
  def columnsBeforeDates = header.size - numberOfDates

  /**
   * Column of population at a given date
   *
   * @param date date of observation
   * @return an option containing the population if provided, none otherwise
   */
  def populations(date: Int): Option[Seq[Double]] =
    (dates.indexOf(date) match {
      case -1 => None
      case i => Some(i + columnsBeforeDates)
    }).map {
      c => startingCities.map(_(c).toDouble)
    }

  /** Id of cities */
  def arokatos = startingCities.map(_(0))

  /** Names of the cities */
  def names = startingCities.map(_(1))

  /** Latitudes of the cities in decimal degrees */
  def latitudes = startingCities.map(_(4))

  /** Longitudes of the cities in decimal degrees */
  def longitudes = startingCities.map(_(5))

  /** Populations of the cities at the first date */
  def initialPopulations = populations(dates.head).get

  /** Cities with hydrocarbons */
  def hydrocarbonDistribution = startingCities.map(l => toBoolean(l(8)))

  /** Regions of the cities */
  def regions = startingCities.map(_(2)).toIterator

  /** A vector of boolean, true in case a city is a regional capital */
  def regionCapitals = startingCities.map(l => toBoolean(l(7))).toIterator

  /** A vector of boolean, true in case a city is a national capital */
  def nationalCapitals = startingCities.map(l => toBoolean(l(11))).toIterator

  /** States cities belong to */
  def states = startingCities.map(_(3)).toIterator

  /** A converter function from string to boolean */
  private def toBoolean(s: String) =
    s match {
      case "1" => true
      case _ => false
    }
}