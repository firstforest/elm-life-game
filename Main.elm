import Dict as D
import Mouse
import Debug

size = 20

type Cell = { isAlive:Bool, isSelected:Bool }
type Cells = D.Dict (Float, Float) Cell
type Input = {x:Int, y:Int, delta:Float, isDown:Bool}

xs = concat (map (repeat size) [0..(size - 1)])
ys = concat (repeat size [0..(size - 1)])
positionList = zip xs ys

initialCells : Cells
initialCells = D.fromList (map (\p -> (p, Cell False False)) positionList)

insertCell : [(Float, Float)] -> Cells -> Cells
insertCell ps cs =
  let
    i (x, y) ccs = D.insert (x, y) (Cell True False) ccs
  in
    foldl i cs ps

testCells = insertCell [(4,4), (4,5), (4,6), (4,7),(4,8)] initialCells

falseCell = Cell False False

getAroundCells : (Float, Float) -> Cells -> [Cell]
getAroundCells (x, y) cs =
  [ D.getOrElse falseCell (x-1,y-1) cs
  , D.getOrElse falseCell (x-1,y) cs
  , D.getOrElse falseCell (x-1,y+1) cs
  , D.getOrElse falseCell (x,y-1) cs
  , D.getOrElse falseCell (x,y+1) cs
  , D.getOrElse falseCell (x+1,y-1) cs
  , D.getOrElse falseCell (x+1,y) cs
  , D.getOrElse falseCell (x+1,y+1) cs
  ]

stepCell : Cells -> (Float, Float) -> Cells -> Cells
stepCell base (x, y) cs =
  let
    aroundCells = getAroundCells (x, y) base
    aliveNums = length (filter (\c -> c.isAlive) aroundCells)
    b = (2 == aliveNums && .isAlive (D.getOrElse falseCell (x, y) cs)) || aliveNums == 3
  in
    D.insert (x, y) (Cell b False) cs

stepCells : Input -> Cells -> Cells
stepCells {x, y, delta, isDown} cells =
  let
    nextCells =
      if (floor (Debug.watch "delta" delta)) % 30 == 0
      then foldl (stepCell cells) cells (D.keys cells)
      else D.map (\c -> {c | isSelected <- False }) cells
    pos = (toFloat (x // cellsize), toFloat (y // cellsize))
    selectedCell = D.getOrElse falseCell pos nextCells
  in
    D.insert pos {selectedCell | isSelected <- True, isAlive <- selectedCell.isAlive || isDown} nextCells

cellsize = 10
offset = (size*cellsize/2) - 5

cellColor cell =
  if cell.isSelected 
  then red
  else 
   if cell.isAlive then black else gray

createCell : ((Float, Float),Cell) -> Form
createCell ((x, y),cell) =
  let
    fillSize = cellsize - 2
  in
    rect fillSize fillSize |>
    filled (cellColor cell) |>
    move (-offset + (x * cellsize), offset - (y * cellsize))

display : Cells -> Element
display cells =
  map createCell (D.toList cells) |> collage (cellsize * size) (cellsize * size)

input = Input <~ Mouse.x ~ Mouse.y ~ foldp (+) 0 (sampleOn (fps 30) (constant 1)) ~ Mouse.isDown

main = lift display <| foldp stepCells testCells input