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
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import java.util.concurrent.LinkedBlockingQueue
import scala.language.reflectiveCalls

/**
  * 
  * @author Matthew Pickett
  */
@RunWith(classOf[JUnitRunner])
class UnpipelinedVectorQueueActuatorSpec extends FunSuite{

  test("0D VectorField"){
    new UnpipelinedVectorQueueProblem(0)
  }
  test("1D VectorField"){
    new UnpipelinedVectorQueueProblem(1)
  }
  test("2D VectorField"){
    new UnpipelinedVectorQueueProblem(2)
  }
  test("3D VectorField"){
    new UnpipelinedVectorQueueProblem(3)
  }


}

/** A test problem that generates a randomly sized vector field then scales it
  * each tick. The UnpipelinedVectorQueueActuator is tested by ensuring that the expected
  * scaling behavior is realized in order in the resulting queue
  *
  * @param fieldDims number of field dimensions to use
  */
class UnpipelinedVectorQueueProblem(fieldDims:Int){
  //generate a random field shape given the specified dimensions
  val maxDimSize = 20
  val rng = new Random
  val fieldShape = Shape(Array.tabulate(fieldDims){
    (i) => rng.nextInt(maxDimSize) + 1
  })
  val tensorShape = Shape(rng.nextInt(maxDimSize) + 1)

  //generate a random period at which to enqueue the test state
  val qPeriod = rng.nextInt(100) + 1
  val q = new LinkedBlockingQueue[FieldState]()

  //generate a compute graph that is supposed to output a random field scaled
  // by the current tick number
  val cg = new ComputeGraph{
    val rand = VectorField.random(fieldShape, tensorShape)
    val cnt = ScalarField(1f)
    cnt <== cnt + 1f
    val out = rand*cnt
    val actuator = UnpipelinedVectorQueueActuator(out, q, qPeriod)

    probe(rand)
  }

  //run the graph for 5 total periods (resulting in 6 enqueue operations
  // including the initial enqueue)
  val periods = 5
  val totalTicks = periods*qPeriod
  for(i<-0 until totalTicks) cg.step
  val rand = FieldState.read(cg.read(cg.rand))
  cg.release

  //ensure that the ticks scaling behavior is realized correctly
  require(q.size() == periods + 1)
  val eq = List.tabulate(periods+1){(i)=>
    val curState = q.take()
    val expectedState = rand.map(x=>x*(i*qPeriod+1))
    curState == expectedState
  }.reduce(_&&_)
  require(eq)
}
