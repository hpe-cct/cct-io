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
import org.scalatest.{Matchers, FunSuite}
import scala.language.reflectiveCalls

/**
  *
  * @author Matthew Pickett
  */
@RunWith(classOf[JUnitRunner])
class VectorStateInjectorSpec extends FunSuite with Matchers  {
  def gen(fieldDims:Int){
    val rng = new Random()
    val len = rng.nextInt(30)+1
    val sizes = Array.tabulate(fieldDims){(i)=>rng.nextInt(30)+1}
    val fieldShape = Shape(sizes)
    val cg1 = new ComputeGraph{
      val x = VectorField.random(fieldShape, Shape(len))
      x <== x + 1
      probe(x)
    }
    cg1.reset
    val xState1 = FieldState.read(cg1.read(cg1.x))
    cg1.step(100)
    val xState2 = FieldState.read(cg1.read(cg1.x))
    cg1.release

    val cg2 = new ComputeGraph{
      val injector = new VectorStateInjector(xState1)
      val y = injector.sensor
      probe(y)
    }

    cg2.step(5)
    val yState1 = FieldState.read(cg2.read(cg2.y))
    cg2.injector.inject(xState2)

    // Step twice:
    // First step brings xState2 into Sensor master latch
    // Second step brings xState2 into probeable Sensor slave latch
    cg2.step(2)

    val yState2 = FieldState.read(cg2.read(cg2.y))
    cg2.reset
    val yState3 = FieldState.read(cg2.read(cg2.y))

    cg2.release

    xState1 should equal (yState1)
    xState2 should equal (yState2)
    xState2 should equal (yState3)
  }

  test("0D"){ gen(0) }
  test("1D"){ gen(1) }
  test("2D"){ gen(2) }
  test("3D"){ gen(3) }
}
