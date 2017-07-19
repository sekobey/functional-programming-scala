case class Pos(row: Int, col: Int) {
  /** The position obtained by changing the `row` coordinate by `d` */
  def deltaRow(d: Int): Pos = copy(row = row + d)

  /** The position obtained by changing the `col` coordinate by `d` */
  def deltaCol(d: Int): Pos = copy(col = col + d)
}

def terrainFunction(levelVector: Vector[Vector[Char]]): Pos => Boolean = {

  def isValidPos(pos: Pos): Boolean = {
    if (levelVector(pos.row)(pos.col) != '-')
      true
    else
      false
  }

  isValidPos
}

def findChar(c: Char, levelVector: Vector[Vector[Char]]): Pos = {
// Alternative way
  // val b = (for {
//    row <- 0 until levelVector.length
//    col <- 0 until levelVector(row).length
//    if (levelVector(row)(col) == 'T')
//  } yield new Pos(row, col))
//  b(0)

  val posX = levelVector.indexWhere(_.indexOf(c) > -1)
  val posY = levelVector(posX).indexOf(c)
  new Pos(posX, posY)
}

type Terrain = Pos => Boolean

val levelVector = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'), Vector('-', '-', '-'))
lazy val terrain: Terrain = terrainFunction(levelVector)
lazy val startPos: Pos = findChar('S', levelVector)
lazy val goal: Pos = findChar('T', levelVector)

sealed abstract class Move
case object Left  extends Move
case object Right extends Move
case object Up    extends Move
case object Down  extends Move

case class Block(b1: Pos, b2: Pos) {

  // checks the requirement mentioned above
  require(b1.row <= b2.row && b1.col <= b2.col, "Invalid block position: b1=" + b1 + ", b2=" + b2)

  /**
    * Returns a block where the `row` coordinates of `b1` and `b2` are
    * changed by `d1` and `d2`, respectively.
    */
  def deltaRow(d1: Int, d2: Int) = Block(b1.deltaRow(d1), b2.deltaRow(d2))

  /**
    * Returns a block where the `col` coordinates of `b1` and `b2` are
    * changed by `d1` and `d2`, respectively.
    */
  def deltaCol(d1: Int, d2: Int) = Block(b1.deltaCol(d1), b2.deltaCol(d2))


  /** The block obtained by moving left */
  def left = if (isStanding)             deltaCol(-2, -1)
  else if (b1.row == b2.row)  deltaCol(-1, -2)
  else                        deltaCol(-1, -1)

  /** The block obtained by moving right */
  def right = if (isStanding)            deltaCol(1, 2)
  else if (b1.row == b2.row) deltaCol(2, 1)
  else                       deltaCol(1, 1)

  /** The block obtained by moving up */
  def up = if (isStanding)               deltaRow(-2, -1)
  else if (b1.row == b2.row)    deltaRow(-1, -1)
  else                          deltaRow(-1, -2)

  /** The block obtained by moving down */
  def down = if (isStanding)             deltaRow(1, 2)
  else if (b1.row == b2.row)  deltaRow(1, 1)
  else                        deltaRow(2, 1)


  /**
    * Returns the list of blocks that can be obtained by moving
    * the current block, together with the corresponding move.
    */
  def neighbors: List[(Block, Move)] = (this.left, Left) :: (this.up, Up) :: (this.right, Right) :: (this.down, Down) :: Nil

  /**
    * Returns the list of positions reachable from the current block
    * which are inside the terrain.
    */
  def legalNeighbors: List[(Block, Move)] = {
    val list = List[(Block, Move)]()
    (this.left, this.up, this.right, this.down) match {
      case (l, _, _, _) if l.isLegal => (l, Left) :: list
      case (_, u, _, _) if u.isLegal => (u, Up) :: list
      case (_, _, r, _) if r.isLegal => (r, Right) :: list
      case (_, _, _, d) if d.isLegal => (d, Down) :: list
    }
  }

  /**
    * Returns `true` if the block is standing.
    */
  def isStanding: Boolean = b1.row == b2.row && b1.col == b2.col

  /**
    * Returns `true` if the block is entirely inside the terrain.
    */
  def isLegal: Boolean = terrain(b1) && terrain(b2)
}

terrainFunction(levelVector)(new Pos(3,1))

findChar('T', levelVector)

val b = new Block(new Pos(0,0), new Pos(1,0))
b.neighbors
b.legalNeighbors
