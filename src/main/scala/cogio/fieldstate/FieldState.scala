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
import java.io._
import scala.collection.parallel.immutable.ParVector
import java.nio.file.Files

/** A CPU memory representation of a field and its data
  * @param fieldType the field type
  * @param data an immutable scala vector that contains the field data
  * @author Matthew Pickett
  */
// I changed the equals method in a way that didn't break serialization, but had to set the
// SerialVersionUID to the Java-computed value it had before the change to preserve previously
// serialized FieldStates. -RJC
//
// This class' serialization format is not preserved across Scala minor releases.  For example,
// serialization breaks in going from Scala 2.10.x -> Scala 2.11.y
@SerialVersionUID(-4284406133882223059L)
case class FieldState(fieldType:FieldType, data:scala.Vector[Float]){
  require(data.length == fieldType.fieldShape.points*fieldType.tensorShape.points,
    "data inconsistent with fieldType")

  // The init function object instance created by toField(), saved for possible reuse.  This permits the compiler
  // to perform common subexpression elimination, resulting in a consolidation of identical constant input fields.
  // Don't worry, if feedback is added to these like-initialized fields, they will not be combined.
  // This cacheing mechanism of init functions is not used for 0D fields since those are initialized by a
  // value, not a function.
  private var initFunction: AnyRef = null

  // The initFunction saving mechanism as described above.
  private def saveInit(f: AnyRef) = {
    if (initFunction == null)
      initFunction = f
    initFunction
  }

  // Convert this field state to a field initializer within a compute graph.  Multiple calls will result in
  // multiple Fields, but these will be consolidated by the Cog compiler if feedback is not involved.
  def toField:Field = {
    val fieldShape = fieldType.fieldShape
    val L = fieldType.layers
    val R = fieldType.rows
    val C = fieldType.columns
    val M = fieldType.tensorRows
    val N = fieldType.tensorColumns

    // convert a row and column index to a linear index
    def index2D(row:Int, column:Int) = row * C  + column

    // convert a layer, row and column index to a linear index
    def index3D(layer:Int, row:Int, column:Int) =
      layer * R * C + row * C  + column
    
    val fieldDims = fieldShape.dimensions
    val tensorDims = fieldType.tensorShape.dimensions
    tensorDims match {
      case 0 => fieldDims match {
        case 0 => ScalarField(data(0))
        case 1 => ScalarField(C, saveInit((c: Int) =>
          data(c)).asInstanceOf[Function1[Int, Float]])
        case 2 => ScalarField(R, C, saveInit((r: Int, c: Int) =>
          data(index2D(r,c))).asInstanceOf[Function2[Int, Int, Float]])
        case 3 => ScalarField(L, R, C, saveInit((l: Int, r: Int, c: Int) =>
          data(index3D(l,r,c))).asInstanceOf[Function3[Int, Int, Int, Float]])
        case _ => throw new RuntimeException("Unsupported number of field dims")
      }
      case 1 => fieldDims match {
        case 0 => VectorField(Vector(N, (n)=>data(n)))
        case 1 => VectorField(C, saveInit((c: Int)=>
          Vector(N, (n) => data(c*N + n))).asInstanceOf[Function1[Int, Vector]])
        case 2 => VectorField(R, C, saveInit((r: Int, c: Int) =>
          Vector(N, (n) => data(index2D(r,c)*N + n))).asInstanceOf[Function2[Int, Int, Vector]])
        case 3 => VectorField(L, R, C, saveInit((l: Int, r: Int, c: Int) =>
          Vector(N, (n) => data(index3D(l,r,c)*N + n))).asInstanceOf[Function3[Int, Int, Int, Vector]])
        case _ => throw new RuntimeException("Unsupported number of field dims")
      }
      case 2 => fieldDims match {
        case 0 => MatrixField(Matrix(M,N, (m,n)=>data(m*N + n)))
        case 1 => MatrixField(C, saveInit((c: Int) =>
          Matrix(M, N, (m,n) => data(c*M*N + m*N + n))).asInstanceOf[Function1[Int, Matrix]])
        case 2 => MatrixField(R, C, saveInit((r: Int, c: Int) =>
          Matrix(M, N, (m,n) => data(index2D(r,c)*M*N + m*N + n))).asInstanceOf[Function2[Int, Int, Matrix]])
        case 3 => MatrixField(L, R, C, saveInit((l: Int, r: Int, c: Int) =>
          Matrix(M, N, (m,n) => data(index3D(l,r,c)*M*N+ m*N + n))).asInstanceOf[Function3[Int, Int, Int, Matrix]])
        case _ => throw new RuntimeException("Unsupported number of field dims")
      }
      case _ => throw new RuntimeException("Unsupported number of tensor dims")
    }
  }

  // create an iterator returning the tensor data at each fieldPoint as a
  // Cog Vector.
  def toVectorIterator = new Iterator[Vector] {
    private val tensorPoints = fieldType.tensorShape.points
    private val vec = new Vector(tensorPoints)
    private val vecData = vec.asArray
    private val dataIter = data.iterator
    def hasNext = dataIter.hasNext
    def next(): Vector = {
      for(i <- 0 until tensorPoints)
        vecData(i) = dataIter.next()
      vec
    }
  }

  //apply a funciton pointwise across the data
  def map(f:(Float)=>Float) = FieldState(fieldType, data.map(f))

  //define equality as having identical type and data
  override def equals(other: Any): Boolean =
    other match {
      case other: FieldState =>
        if (this eq other)
          true
        else {
          val typesEqual = fieldType == other.fieldType
          val dataEqual = data.zip(other.data).map(x=>x._1==x._2).reduce(_&&_)
          typesEqual && dataEqual
        }
      case _ => false
    }

  // override hashCode to support FieldState collections
  override def hashCode: Int = {
    def combineHashes(hc1: Int, hc2: Int): Int = {
      val longHash = hc1.toLong * 41 + hc2
      ((longHash ^ longHash>>32) & 0xffffffff).toInt
    }
    val dataHash = data.map(_.hashCode).foldLeft(0)(combineHashes(_,_))
    combineHashes(dataHash, fieldType.hashCode)
  }

  //serialize field state to a file
  def serializeToFile(file:File){
    val fs = new BufferedOutputStream(new FileOutputStream(file))
    val os = new ObjectOutputStream(fs)
    os.writeObject(this)
    os.close()
    fs.close()
  }

  /** write field state to a file in an externally readable format:
    * header of 7 Ints: fieldDims, rows, columns, layers, tensorDims, rows, columns
    * data block of Floats with row, column, layer field point order with
    * row, column tensor point order at each field point.
    */
  def saveToFile(file:File){
    val os = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(file)))

    //write header of 7 Ints: fieldDims, rows, columns, layers, tensorDims, rows, columns
    val fieldDims = fieldType.fieldShape.dimensions
    val fieldSizes = fieldType.fieldShape.toArray.padTo(3, 0)
    os.writeInt(fieldDims)
    fieldSizes.foreach(os.writeInt)

    val tensorDims = fieldType.tensorShape.dimensions
    val tensorSizes = fieldType.tensorShape.toArray.padTo(2, 0)
    os.writeInt(tensorDims)
    tensorSizes.foreach(os.writeInt)

    //write data
    data.foreach(os.writeFloat)
    os.close()
  }

  override def toString =
    s"FieldState($fieldType)"

  def print() {
    toVectorIterator.foreach(println(_))
  }
}


object FieldState{

  def random(fieldShape:Shape, tensorShape:Shape) = {
    val rng = new Random
    val points = fieldShape.points*tensorShape.points
    val data = scala.Vector.tabulate(points){(i)=>rng.nextFloat()}
    val tpe = new FieldType(fieldShape, tensorShape, Float32)
    FieldState(tpe, data)
  }

  def apply(fieldShape:Shape, tensorShape:Shape):FieldState = {
    val points = fieldShape.points*tensorShape.points
    val data = scala.Vector.tabulate(points){(i)=>0f}
    val tpe = new FieldType(fieldShape, tensorShape, Float32)
    FieldState(tpe, data)
  }

  //deserialize field state from a file
  def deserializeFromFile(file:File):FieldState = {
    val fs = new BufferedInputStream(new FileInputStream(file))
    val os = new ObjectInputStream(fs)
    val o = os.readObject().asInstanceOf[FieldState]
    os.close()
    fs.close()
    o
  }

  /** read field state from a file saved in the externally readable format:
    * header of 7 Ints: fieldDims, rows, columns, layers, tensorDims, rows, columns
    * data block of Floats with row, column, layer field point order with
    * row, column tensor point order at each field point.
    *
    */
  def loadFromFile(file:File):FieldState = {
    val is = new DataInputStream(new BufferedInputStream(new FileInputStream(file)))

    //read header
    val fieldDims = is.readInt
    require(fieldDims >=0 && fieldDims <= 3,
      "Bad number of field dimensions in file header")
    val fieldSizes = List.tabulate(3){
      (i) => is.readInt
    }.take(fieldDims)
    fieldSizes.foreach(x =>
      require(x>0 && x < 100000, "Bad dimension sizes in file header")
    )
    val fieldPoints = if(fieldDims == 0) 1 else fieldSizes.reduce(_*_)

    val tensorDims = is.readInt()
    require(tensorDims >=0 && tensorDims <= 2,
      "Bad number of tensor dimensions in file header")
    val tensorSizes = List.tabulate(2){
      (i) => is.readInt
    }.take(tensorDims)
    tensorSizes.foreach(x =>
      require(x>0 && x < 100000, "Bad dimension sizes in file header")
    )
    val tensorPoints = if(tensorDims == 0) 1 else tensorSizes.reduce(_*_)

    //check to make sure file size is consistent with header data
    val points = fieldPoints*tensorPoints
    val fileSize = Files.size(file.toPath)
    require(fileSize == points*4 +7*4,
      file + " is not the correct size given its header. Found " + fileSize +
        "bytes, expected " + (points * 4 + 7 * 4) + " bytes. Rows = " + fieldSizes(0) +
        ", columns = " + fieldSizes(1))

    val fieldType = new FieldType(Shape(fieldSizes:_*),Shape(tensorSizes:_*), Float32)
    val data = scala.Vector.tabulate(fieldPoints*tensorPoints){
      (i) => is.readFloat
    }
    is.close()

    new FieldState(fieldType, data)
  }

  //read the state of an active field via the FieldReader interface
  def read(reader:FieldReader) = {
    val fieldType = reader.fieldType
    val fieldDims = reader.fieldShape.dimensions
    val fieldPoints = reader.fieldShape.points
    val fieldSizes = reader.fieldShape.toArray.padTo(3, 1)
    val tensorSizes = reader.tensorShape.toArray.padTo(2, 1)
    val fieldIndices = ParVector.tabulate(fieldPoints){
      (i) => {
        val row = i / (fieldSizes(1)*fieldSizes(2))
        val col = (i - row*fieldSizes(1)*fieldSizes(2))/fieldSizes(2)
        val lay = i - row*fieldSizes(1)*fieldSizes(2) - col*fieldSizes(2)
        (row,col,lay)
      }
    }
    val data:scala.Vector[Float] = reader match {
      case r: ScalarFieldReader =>
        fieldIndices.map(i => readScalar(r, i, fieldDims)).toVector
      case r: VectorFieldReader =>
        fieldIndices.map(i=>
          readVector(r, i, fieldDims, tensorSizes(0))).flatten.toVector
      case r: MatrixFieldReader =>
        fieldIndices.map(i=>
          readMatrix(r, i, fieldDims, tensorSizes(0), tensorSizes(1))).flatten.toVector
      case _ => throw new RuntimeException("Reader type not supported")
    }

    new FieldState(fieldType, data)
  }


  private def readScalar(r:ScalarFieldReader, index:(Int,Int,Int), dims:Int):Float ={

    dims match {
      case 0 => r.read()
      case 1 => r.read(index._1)
      case 2 => r.read(index._1, index._2)
      case 3 => r.read(index._1, index._2, index._3)
      case _ => throw new RuntimeException("Bad number of dimensions")
    }
  }
  private def readVector(r:VectorFieldReader, index:(Int,Int,Int), dims:Int,
                         vecLen:Int):scala.Vector[Float] = {
    val vec = Vector(vecLen, (i)=>0f)

    dims match {
      case 0 => r.read(vec); vec.toArray.toVector
      case 1 => r.read(index._1, vec); vec.toArray.toVector
      case 2 => r.read(index._1, index._2, vec); vec.toArray.toVector
      case 3 => r.read(index._1, index._2, index._3, vec); vec.toArray.toVector
      case _ => throw new RuntimeException("Bad number of dimensions")
    }
  }

  private def readMatrix(r:MatrixFieldReader, index:(Int,Int,Int), dims:Int,
                         matRows:Int, matCols:Int):scala.Vector[Float] = {
    val mat = Matrix(matRows, matCols, (i,j)=>0f)

    dims match {
      case 0 => r.read(mat); mat.asArray.toVector
      case 1 => r.read(index._1, mat); mat.asArray.toVector
      case 2 => r.read(index._1, index._2, mat); mat.asArray.toVector
      case 3 => r.read(index._1, index._2, index._3, mat); mat.asArray.toVector
      case _ => throw new RuntimeException("Bad number of dimensions")
    }
  }
}