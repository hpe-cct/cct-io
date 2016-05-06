/*
 * (c) Copyright 2016 Hewlett Packard Enterprise Development LP
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cogio.fieldstate

import libcog._

/** A sensor for injecting FieldState objects into a ComputeGraph
  *  injected state will persist through a ComputeGraph reset
  * @author Matthew Pickett
  */
private [fieldstate] class VectorStateInjector(initialState:FieldState) {
  val fieldShape = initialState.fieldType.fieldShape
  val tensorShape = initialState.fieldType.tensorShape
  require(initialState.fieldType.tensorOrder == 1)

  private var resetTo:Option[FieldState] = Some(initialState)
  private var updateTo = resetTo

  private def next() = {
    val iter = updateTo.map(_.toVectorIterator)
    updateTo = None
    iter
  }

  private def reset(){updateTo = resetTo}

  def inject(x:FieldState) {
    require(initialState.fieldType == x.fieldType)
    updateTo = Some(x)
    // Last injected value is the value to reset to
    resetTo = updateTo
  }

  val sensor = new VectorSensor(fieldShape, tensorShape, next _, reset _)
}

object VectorStateInjector{
  def apply(initialState:FieldState) = new VectorStateInjector(initialState)
}
