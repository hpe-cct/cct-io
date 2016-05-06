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

/** A sensor that reads FieldState object from a BlockingQueue
  * @author Matthew Pickett
  */
private [fieldstate] class VectorQueueSensor(fieldShape:Shape, tensorShape:Shape, queue: BlockingQueue[FieldState], period:Int) {
  var count = 0
  val sensor = new VectorSensor(fieldShape, tensorShape, getNext _, reset _)

  def reset(){count = 0}

  def getNext:Option[Iterator[Vector]] = {
    val iter =
      if(count % period == 0){

        val cur = queue.take()
        if(cur.fieldType == new FieldType(fieldShape, tensorShape, Float32))
          Some(cur.toVectorIterator)
        else throw new IllegalArgumentException()
      }
      else
        None
    count = count + 1
    iter
  }
}

object VectorQueueSensor{
  def apply(fieldShape:Shape, tensorShape:Shape, queue: BlockingQueue[FieldState], period:Int) =
    new VectorQueueSensor(fieldShape, tensorShape, queue, period).sensor
}
