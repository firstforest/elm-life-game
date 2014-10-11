import Dict as D

size = 30

type Cells = D.Dict (Float, Float) Bool

xs = concat (map (repeat size) [0..(size - 1)])
ys = concat (repeat size [0..(size - 1)])
positionList = zip xs ys

initialCells : Cells
initialCells = D.fromList (map (\p -> (p, False)) positionList)

insertCell : [(Float, Float)] -> Cells -> Cells
insertCell ps cs =
  let
    i (x, y) ccs = D.insert (x, y) True ccs
  in
    foldl i cs ps

testCells = insertCell [(4,4), (4,5), (4,6), (4,7),(4,8)] initialCells

getAroundCells : (Float, Float) -> Cells -> [Bool]
getAroundCells (x, y) cs =
  [ D.getOrElse False (x-1,y-1) cs
  , D.getOrElse False (x-1,y) cs
  , D.getOrElse False (x-1,y+1) cs
  , D.getOrElse False (x,y-1) cs
  , D.getOrElse False (x,y+1) cs
  , D.getOrElse False (x+1,y-1) cs
  , D.getOrElse False (x+1,y) cs
  , D.getOrElse False (x+1,y+1) cs
  ]

stepCell : Cells -> (Float, Float) -> Cells -> Cells
stepCell base (x, y) cs =
  let
    aroundCells = getAroundCells (x, y) base
    aliveNums = length (filter identity aroundCells)
    b = (2 == aliveNums && D.getOrElse False (x, y) cs) || aliveNums == 3
  in
    D.insert (x, y) b cs

stepCells : a -> Cells -> Cells
stepCells _ cells =
  foldl (stepCell cells) cells (D.keys cells)

cellsize = 20
offset = (size*cellsize/2) - 5

createCell : ((Float, Float),Bool) -> Form
createCell ((x, y),b) =
  let
    fillSize = cellsize - 2
  in
    rect fillSize fillSize |>
    filled (if b then black else gray) |>
    move (-offset + (x * cellsize), offset - (y * cellsize))

display : Cells -> Element
display cells =
  map createCell (D.toList cells) |> collage (cellsize * size) (cellsize * size)

main = lift display <| foldp stepCells testCells (every second)