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

object DenseMatrix {
  def apply(_content: Array[Array[Double]]) = new DenseMatrix {
    override def content: Array[Array[Double]] = _content
  }

  case class Cell(row: Int, value: Double)

}

import DenseMatrix._

trait DenseMatrix {
  def content: Array[Array[Double]]

  def side: Int = content.size
  def lines: Seq[Seq[Cell]] =
    content.map(_.zipWithIndex.map { case (v, i) => Cell(i, v) }.toIndexedSeq).toIndexedSeq

  def transpose = DenseMatrix(content.transpose)
  def linesContent: Seq[Seq[Double]] = content.map(_.toIndexedSeq).toIndexedSeq

  def map(f: (Int, Int, Double) => Double) = {
    val newContent = Array.tabulate(side, side)((i, j) => f(i, j, content(i)(j)))
    DenseMatrix(newContent)
  }

}
