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

import java.util.concurrent.BlockingQueue
import libcog._

/** An actuator of VectorFields that maintains a queue of output values.
  *
  * This class was formerly called VectorQueueActuator, but was renamed to be consistent with the naming scheme
  * used throughout the rest of the Cog project: the default behavior of Sensors and Actuators is pipelined
  * (for better or worse).  Those classes with unpipelined behavior have an "Unpiplelined" prefix.
  *
  * @author Matthew Pickett
  */
private [fieldstate] class UnpipelinedVectorQueueActuator(input:VectorField, queue:BlockingQueue[FieldState], period:Int) {
  var count = 0
  val actuator = new UnpipelinedVectorActuator(input, putNext _, reset _)

  def reset(): Unit = { count = 0 }

  def putNext(iter: Iterator[Float]): Unit = {
    if (count % period == 0) {
      val state = FieldState(input.fieldType, iter.toVector)
      queue put state
    }
    count = count + 1
  }
}

object UnpipelinedVectorQueueActuator{
  def apply(input:VectorField, queue:BlockingQueue[FieldState], period:Int) =
    new UnpipelinedVectorQueueActuator(input, queue, period).actuator
}