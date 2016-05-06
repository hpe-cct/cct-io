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
import java.nio.file.{Files, Paths}
import scala.language.reflectiveCalls


/**
  * 
  * @author Matthew Pickett
  */

class readRestoreTest(fieldDims:Int, tensorDims:Int){
  //generate a random field shape given the specified dimensions
  val maxDimSize = 20
  val rng = new Random
  val fieldShape = Shape(Array.tabulate(fieldDims){
    (i) => rng.nextInt(maxDimSize) + 1
  })
  val tensorShape = Shape(Array.tabulate(tensorDims){
    (i) => rng.nextInt(maxDimSize) + 1
  })

  //initiate and step a CG which just generates a field of the specified shape
  val gen = new ComputeGraph {
    val rand = tensorDims match {
      case 0 => ScalarField.random(fieldShape)
      case 1 => VectorField.random(fieldShape, tensorShape)
      case 2 => MatrixField.random(fieldShape, tensorShape)
      case _ => throw new RuntimeException("Bad tensor dimension")
    }

    probe(rand)
  }
  gen.step
  val randReader = gen.read(gen.rand)
  val randState = FieldState.read(randReader)

  //save a serialezed state object and an externally-readable binary file
  val tmp = Paths.get(System.getProperty("java.io.tmpdir"))
  val objPath = Paths.get(tmp.toString, "tmpTest.obj")
  val fldPath = Paths.get(tmp.toString, "tmpTest.fld")
  randState.serializeToFile(objPath.toFile)
  randState.saveToFile(fldPath.toFile)

  val restoredObjState = FieldState.deserializeFromFile(objPath.toFile)
  val restoredFldState = FieldState.loadFromFile(fldPath.toFile)

  Files.delete(objPath)
  Files.delete(fldPath)

  val restore = new ComputeGraph {
    val restoredObj = restoredObjState.toField
    val restoredFld = restoredFldState.toField

    probe(restoredObj)
    probe(restoredFld)
  }
  restore.step

  val restoredObjReader = FieldState.read(restore.read(restore.restoredObj))
  val restoredFldReader = FieldState.read(restore.read(restore.restoredFld))
}

@RunWith(classOf[JUnitRunner])
class FieldStateSpec extends FunSuite with Matchers   {
  test("Read and restore 0D Scalar Field"){
    val t = new readRestoreTest(0, 0)
    t.randState should equal (t.restoredObjReader)
    t.randState should equal (t.restoredFldReader)
    t.gen.release
    t.restore.release
  }

  test("Read and restore 1D Scalar Field"){
    val t = new readRestoreTest(1, 0)
    t.randState should equal (t.restoredObjReader)
    t.randState should equal (t.restoredFldReader)
    t.gen.release
    t.restore.release
  }

  test("Read and restore 2D Scalar Field"){
    val t = new readRestoreTest(2, 0)
    t.randState should equal (t.restoredObjReader)
    t.randState should equal (t.restoredFldReader)
    t.gen.release
    t.restore.release
  }

  test("Read and restore 3D Scalar Field"){
    val t = new readRestoreTest(3, 0)
    t.randState should equal (t.restoredObjReader)
    t.randState should equal (t.restoredFldReader)
    t.gen.release
    t.restore.release
  }

  test("Read and restore 0D Vector Field"){
    val t = new readRestoreTest(0, 1)
    t.randState should equal (t.restoredObjReader)
    t.randState should equal (t.restoredFldReader)
    t.gen.release
    t.restore.release
  }

  test("Read and restore 1D Vector Field"){
    val t = new readRestoreTest(1, 1)
    t.randState should equal (t.restoredObjReader)
    t.randState should equal (t.restoredFldReader)
    t.gen.release
    t.restore.release
  }

  test("Read and restore 2D Vector Field"){
    val t = new readRestoreTest(2, 1)
    t.randState should equal (t.restoredObjReader)
    t.randState should equal (t.restoredFldReader)
    t.gen.release
    t.restore.release
  }

  test("Read and restore 3D Vector Field"){
    val t = new readRestoreTest(3, 1)
    t.randState should equal (t.restoredObjReader)
    t.randState should equal (t.restoredFldReader)
    t.gen.release
    t.restore.release
  }

  test("Read and restore 0D Matrix Field"){
    val t = new readRestoreTest(0, 2)
    t.randState should equal (t.restoredObjReader)
    t.randState should equal (t.restoredFldReader)
    t.gen.release
    t.restore.release
  }

  test("Read and restore 1D Matrix Field"){
    val t = new readRestoreTest(1, 2)
    t.randState should equal (t.restoredObjReader)
    t.randState should equal (t.restoredFldReader)
    t.gen.release
    t.restore.release
  }

  test("Read and restore 2D Matrix Field"){
    val t = new readRestoreTest(2, 2)
    t.randState should equal (t.restoredObjReader)
    t.randState should equal (t.restoredFldReader)
    t.gen.release
    t.restore.release
  }

  test("Read and restore 3D Matrix Field"){
    val t = new readRestoreTest(3, 2)
    t.randState should equal (t.restoredObjReader)
    t.randState should equal (t.restoredFldReader)
    t.gen.release
    t.restore.release
  }
}
