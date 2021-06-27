--module Onitama where
module Onitama where

import Data.Maybe (fromJust, listToMaybe)
import Data.List (elemIndex)

-- Los jugadores posibles, Rojo y Azul
data OnitamaPlayer = RedPlayer | BluePlayer deriving (Eq, Show, Enum, Bounded, Read)
--Los posibles tipos de piezas en el juego
data OnitamaPiece = Master OnitamaPlayer (Int, Int) | Apprentice OnitamaPlayer (Int, Int) | Empty (Int, Int)  deriving (Eq, Show, Read)
--Las posibles cartas que puede dar el juego
data OnitamaCard = Tiger | Dragon | Rabbit | Monkey | Crab | Elephant
                | Mantis | Crane | Frog | Boar | Goose | Horse | Rooster | Ox | Eel | Cobra deriving (Eq, Show, Enum, Read)

data OnitamaAction = OnitamaAction OnitamaPiece OnitamaCard (Int, Int) deriving (Eq, Show, Read)


data GameResult p = Winner p | Loser p | Draw deriving (Eq, Show)
data OnitamaBoard = Matrix [[OnitamaPiece]] deriving (Eq, Show)
--data OnitamaBoard = Matrix [Piece, Piece, Piece, Piece, Piece] deriving (Eq, Show)
--data Piece = Position [OnitamaPiece, OnitamaPiece, OnitamaPiece, OnitamaPiece, OnitamaPiece] deriving (Eq, Show)

--Definimos la estructura del juego con las cartas de ambos jugadores, la carta que sobra(la 5ta), un tablero inicial, un jugador y donde almacenar un resultado final
--Primero tenemos el tablero, luego un jugador, luego sus cartas y por ultimo el resultado
data OnitamaGame = OnitamaGame OnitamaBoard OnitamaPlayer ([OnitamaCard], [OnitamaCard]) OnitamaCard [GameResult OnitamaPlayer] deriving (Show)

--beginning :: [OnitamaCard] -> OnitamaGame
--El estado inicial del juego de Onitama, repartimos las cartas y elegimos quien comienza jugando
--Suponemos qu eel mazo no viene siempre de la misma manera
beginning :: [OnitamaCard] -> OnitamaGame
beginning mazo = OnitamaGame (getInitialOnitamaBoard) (getInitialPlayer carta1) ([carta2, carta3], [carta4, carta5]) carta1  []
    where [carta1, carta2, carta3, carta4, carta5] = take 5 mazo


--Armamos el tablero en su estado inicial
{-getInitialOnitamaBoard :: OnitamaBoard
getInitialOnitamaBoard = Matrix [Position [Apprentice RedPlayer (1,1), Apprentice RedPlayer (1,2), Master RedPlayer (1,3), Apprentice RedPlayer (1,4), Apprentice RedPlayer(1,5)],
                                 Position [Empty (2,1), Empty (2,2), Empty (2,3), Empty (2,4), Empty (2,5)],
                                 Position [Empty (3,1), Empty (3,2), Empty (3,3), Empty (3,4), Empty (3,5)],
                                 Position [Empty (4,1), Empty (4,2), Empty (4,3), Empty (4,4), Empty (4,5)], 
                                 Position [Apprentice BluePlayer (5,1), Apprentice BluePlayer (5,2), Master BluePlayer (5,3), Apprentice BluePlayer (5,4), Apprentice BluePlayer (5,5)]] -}

getInitialOnitamaBoard :: OnitamaBoard
getInitialOnitamaBoard = Matrix [[Apprentice RedPlayer (1,1), Apprentice RedPlayer (1,2), Master RedPlayer (1,3), Apprentice RedPlayer (1,4), Apprentice RedPlayer(1,5)],
                                 [Empty (2,1), Empty (2,2), Empty (2,3), Empty (2,4), Empty (2,5)],
                                 [Empty (3,1), Empty (3,2), Empty (3,3), Empty (3,4), Empty (3,5)],
                                 [Empty (4,1), Empty (4,2), Empty (4,3), Empty (4,4), Empty (4,5)], 
                                 [Apprentice BluePlayer (5,1), Apprentice BluePlayer (5,2), Master BluePlayer (5,3), Apprentice BluePlayer (5,4), Apprentice BluePlayer (5,5)]]
                                  
--Dada la 5ta carta, es decir la que queda sobre la mesa para intercambiar obtenemos el color para determinar quien arranca
getInitialPlayer :: OnitamaCard -> OnitamaPlayer
getInitialPlayer r
    |r == Tiger = BluePlayer
    |r == Rabbit = BluePlayer
    |r == Crab = BluePlayer
    |r == Goose = BluePlayer
    |r == Monkey = BluePlayer
    |r == Ox = BluePlayer
    |r == Crane = BluePlayer
    |r == Eel = BluePlayer
    |otherwise = RedPlayer

--Obtenemos la posicion de la pieza, sin importar si es aprendis o si es maestro.
getPositionPice :: OnitamaPiece -> (Int, Int)
getPositionPice (Master _ j) = j
getPositionPice (Apprentice _ j) = j
getPositionPice (Empty j) = j

deck :: [OnitamaCard]
deck = [Tiger, Dragon, Rabbit, Monkey, Crab, Elephant, Mantis, Crane, Frog, Boar, Goose, Horse, Rooster, Ox, Eel, Cobra]

--Esta función determina a cuál jugador le toca mover, dado un estado de juego.
activePlayer :: OnitamaGame -> Maybe OnitamaPlayer
activePlayer (OnitamaGame _ player _ _ ganador) = if (not (null ganador)) then Nothing else (Just player)
--Cambio la firma revisar y ver como re implementar
--activePlayer :: OnitamaGame -> OnitamaPlayer
--activePlayer (OnitamaGame _ j _ _ []) = j

--La lista debe incluir una y solo una tupla para cada jugador. Si el jugador está activo, la lista asociada debe incluir todos sus posibles
--movimientos para el estado de juego dado. Sino la lista debe estar vacía.
actions :: OnitamaGame -> [(OnitamaPlayer, [OnitamaAction])]
actions (OnitamaGame board player (redcard, bluecard) _   []) = if (player == RedPlayer) 
    then [(RedPlayer, (createActionList board player redcard)), (BluePlayer, [])] 
    else (if (player == BluePlayer) then [(RedPlayer, []), (BluePlayer, (createActionList board player bluecard) )]
    else [])

createActionList :: OnitamaBoard -> OnitamaPlayer -> [OnitamaCard] -> [OnitamaAction]
createActionList board player card  = (foldr1 (++) [map (\x -> OnitamaAction playerPices cards x) (recoverMoves board playerPices cards)| playerPices <- (piecePlayer board player), cards <- card])


colourPiece :: OnitamaPiece -> OnitamaPlayer
colourPiece (Master col _) = col
colourPiece (Apprentice col _) = col
colourPiece (Empty _) = error "Pieza vacia, no posee color"

validPiece :: OnitamaPiece -> Bool
validPiece (Master _ _) = True
validPiece (Apprentice _ _) = True
validPiece (Empty _) = False

listValidPice :: [[OnitamaPiece]] -> [OnitamaPiece]
listValidPice board = filter (validPiece) (boardToUnicList board)

boardToUnicList :: [[OnitamaPiece]] -> [OnitamaPiece]
boardToUnicList x = foldr1 (++) x

piecePlayer ::OnitamaBoard -> OnitamaPlayer -> [OnitamaPiece]
piecePlayer board player = filter (\x -> colourPiece x == player)  (listValidPice (getBoard board))

recoverMoves :: OnitamaBoard ->  OnitamaPiece -> OnitamaCard ->  [(Int, Int)]
recoverMoves board piece card = if ((getPiceInPos board (getPositionPice piece)) == piece)
    then (filter (\cordenada -> validMove board piece cordenada) listMovesValid)
    else error "No se encontro la pieza"
    where listMovesValid = (checkPosInBoard (couldMoveTo (colourPiece piece) card (getPositionPice piece)))

        --validamos si la pieza puede ir a la posicion dada controlamos que no haya una pieza nuestra(del color del jugador)
validMove :: OnitamaBoard -> OnitamaPiece -> (Int, Int) ->  Bool
validMove board pieza pos  
    |(validPiece (pieceInBoard)) && ((colourPiece pieza) == (colourPiece (pieceInBoard))) = False
    |((validPiece (pieceInBoard)) == False) = True
    |otherwise = True
    where pieceInBoard = (getPiceInPos board pos)


checkPosInBoard :: [(Int, Int)] -> [(Int, Int)]
checkPosInBoard list = filter (\(x,y) -> x>0 && x<6 && y>0 && y<6) list
        
--Esta funcion representara todos los posibles movimientos de ambos players
--Debemos tener en cuenta que los jugadlres "Avanzan o retroceden" dependiendo de la carta que elijan
--y la decision que tomen de forma opuesta, es decir que los rojos estan en la parte de "arriba" y los azules de "abajo"
--Por eso los movimientos son "opuestos"
--              Color Player    OnitamaCard     Pos actaul de la pieza    Lista de posibles destinos
couldMoveTo :: OnitamaPlayer -> OnitamaCard -> (Int, Int) -> [(Int, Int)]
couldMoveTo RedPlayer Tiger (x,y) = [(x+2, y), (x-1 , y)]
couldMoveTo BluePlayer Tiger (x,y) = [(x-2, y), (x+1, y)]
couldMoveTo RedPlayer Crab (x,y) = [(x, y-2), (x, y+2), (x+1,y)]
couldMoveTo BluePlayer Crab (x,y) = [(x, y-2), (x, y+2), (x-1, y)]
couldMoveTo RedPlayer Monkey (x,y) = [(x-1, y+1), (x-1, y-1), (x+1, y+1), (x+1, y-1)]
couldMoveTo BluePlayer Monkey (x,y) = [(x+1, y-1), (x+1, y+1), (x-1, y-1), (x-1, y+1)]
couldMoveTo RedPlayer Crane (x,y) = [(x-1, y-1), (x+1, y), (x-1, y+1)]
couldMoveTo BluePlayer Crane (x,y) = [(x+1, y-1), (x-1, y), (x+1, y+1)]
couldMoveTo RedPlayer Dragon (x,y) = [(x+1, y-2), (x+1, y+2), (x-1, y-1), (x- 1, y+1)]
couldMoveTo BluePlayer Dragon (x,y) = [(x-1, y-2), (x-1, y+2), (x+1, y+1), (x+1, y-1)]
couldMoveTo RedPlayer Elephant (x,y) = [(x, y+1), (x, y-1), (x+1, y+1), (x+1, y-1)]
couldMoveTo BluePlayer Elephant (x,y) = [(x, y+1), (x, y-1), (x-1, y+1), (x-1, y-1)]
couldMoveTo RedPlayer Mantis (x,y) = [(x+1, y+1), (x-1, y), (x+1, y-1)]
couldMoveTo BluePlayer Mantis (x,y) = [(x-1, y-1), (x-1, y), (x-1, y+1)]
couldMoveTo RedPlayer Boar (x,y) = [(x+1, y), (x, y+1), (x, y-1)]
couldMoveTo BluePlayer Boar (x,y) = [(x-1, y), (x, y-1), (x, y+1)]
couldMoveTo RedPlayer Frog (x,y) = [(x-1, y+1), (x, y-2), (x+1, y-1)]
couldMoveTo BluePlayer Frog (x,y) = [(x+1, y+1), (x, y-2), (x-1, y-1)]
couldMoveTo RedPlayer Goose (x,y) = [(x, y+1), (x, y-1), (x+1, y-1), (x-1, y+1)]
couldMoveTo BluePlayer Goose (x,y) = [(x, y-1), (x, y+1), (x-1, y-1), (x+1, y+1)]
couldMoveTo RedPlayer Horse (x,y) = [(x-1, y), (x+1, y), (x, y-1)]
couldMoveTo BluePlayer Horse (x,y) = [(x-1, y), (x+1, y), (x, y-1)]
couldMoveTo RedPlayer Eel (x,y) = [(x+1, y-1), (x-1, y-1), (x, y+1)]
couldMoveTo BluePlayer Eel (x,y) = [(x+1, y-1), (x-1, y-1), (x, y+1)]
couldMoveTo RedPlayer Rabbit (x,y) = [(x+1, y-1), (x-1, y+1), (x, y+2)]
couldMoveTo BluePlayer Rabbit (x,y) = [(x+1, y-1), (x-1, y+1), (x, y+2)]
couldMoveTo RedPlayer Rooster (x,y) = [(x, y-1), (x, y+1), (x+1, y+1), (x-1, y-1)]
couldMoveTo BluePlayer Rooster (x,y) = [(x, y-1), (x, y+1), (x-1, y+1), (x+1, y-1)]
couldMoveTo RedPlayer Ox (x,y) = [(x-1, y), (x+1, y), (x,y+1)]
couldMoveTo BluePlayer Ox (x,y) = [(x-1, y), (x+1, y), (x,y+1)]
couldMoveTo RedPlayer Cobra (x,y) = [(x, y-1), (x+1, y+1), (x-1, y-1)]
couldMoveTo BluePlayer Cobra (x,y) = [(x, y-1), (x-1, y+1), (x+1, y+1)]                    

--obtenemos lo que hay en una cordenada especifica
--obtenemos la primer lista (coordenada de las x) y luego obtenemos el elemento de esa litsa(coordenada de la y) restamos uno por que parte de 0 el !!
getPiceInPos :: OnitamaBoard -> (Int, Int) -> OnitamaPiece
getPiceInPos board (x,y) = (((getBoard board) !! (x-1)) !! (y-1))

--Obtenemos la lista de lista que forma el tablero
getBoard :: OnitamaBoard -> [[OnitamaPiece]]
getBoard (Matrix board) = board

--nextBoard :: OnitamaBoard ->  OnitamaAction -> OnitamaBoard
--nextBoard (Matrix [[a]])

next :: OnitamaGame -> OnitamaPlayer -> OnitamaAction -> OnitamaGame
next (OnitamaGame board player (redcard,bluecard) fifthcard gmr) activeplayer (OnitamaAction piece card pos)
    | gmr /= [] = (OnitamaGame board player (redcard,bluecard) fifthcard gmr)
    | player /= activeplayer = error "Wrong player"
    | player == BluePlayer && elem (OnitamaAction piece card pos) (snd blue) = actionWithPiece (OnitamaGame board player (redcard,bluecard) fifthcard gmr) (OnitamaAction piece card pos)
    | player == RedPlayer && elem (OnitamaAction piece card pos) (snd red) = actionWithPiece (OnitamaGame board player (redcard,bluecard) fifthcard gmr) (OnitamaAction piece card pos)
    | otherwise = error "No valid action"
    where [red, blue] = actions (OnitamaGame board player (redcard,bluecard) fifthcard gmr)

-- verificamos si hay pieza rival o empty
checkDest :: OnitamaBoard -> (Int, Int) ->  OnitamaPiece
checkDest board pos = (getPiceInPos board pos)

-- dada la pieza en esa posicion vemos que accion ejecutar
actionWithPiece :: OnitamaGame -> OnitamaAction -> OnitamaGame
actionWithPiece game@(OnitamaGame board player (redcard,bluecard) fifthcard gmr) (OnitamaAction piece card pos)
    | player == BluePlayer && pos == (1,3) = (OnitamaGame board player (redcard,bluecard) fifthcard [Winner BluePlayer, Loser RedPlayer]) -- gano por llegar al santuario
    | player == RedPlayer && pos == (5,3) = (OnitamaGame board player (redcard,bluecard) fifthcard [Winner RedPlayer,Loser BluePlayer]) -- gano por llegar al santuario
    | getPiceName getPiceInPosDest == "Apprentice" = newBoardToGame game (OnitamaAction piece card pos)
    | getPiceName getPiceInPosDest == "Master" = (OnitamaGame board player (redcard,bluecard) fifthcard [Winner player,Loser (lockOponnentPlayer player)])
    | getPiceName getPiceInPosDest == "Empty" = newBoardToGame game (OnitamaAction piece card pos)
    where getPiceInPosDest = checkDest board pos

getPiceName :: OnitamaPiece -> String
getPiceName  (Master player a) = "Master"
getPiceName  (Apprentice player a) = "Apprentice"
getPiceName  (Empty a) = "Empty"

lockOponnentPlayer :: OnitamaPlayer -> OnitamaPlayer
lockOponnentPlayer p = if p == RedPlayer then BluePlayer else RedPlayer 

newBoardToGame :: OnitamaGame -> OnitamaAction -> OnitamaGame 
newBoardToGame (OnitamaGame board player (redcard,bluecard) fifthcard gmr) (OnitamaAction piece card pos) = OnitamaGame (auxNewBoard board (OnitamaAction piece card pos)) (lockOponnentPlayer player) (changeCards (redcard,bluecard) card fifthcard player) card [] 

changeCards :: ([OnitamaCard], [OnitamaCard]) -> OnitamaCard -> OnitamaCard -> OnitamaPlayer -> ([OnitamaCard], [OnitamaCard])
changeCards (x:xs,blue) card fifthcard RedPlayer = if x == card then (fifthcard:xs,blue) else (x:[fifthcard],blue)
changeCards (red,x:xs) card fifthcard BluePlayer = if x == card then (red,(fifthcard:xs)) else (red,x:[fifthcard])

--Devolvemos el nuevo tablero con la solicitud del movimiento de pieza realizado con exito
auxNewBoard :: OnitamaBoard -> OnitamaAction -> OnitamaBoard
auxNewBoard board1 (OnitamaAction piece card (x,y)) = Matrix ((take (x-1) (getBoard board)) ++ [(changePiece ((getBoard board) !! (x-1)) (changeCordPice piece (x,y)) (x,y))] ++ (drop x (getBoard board))) where board = auxNewBoard2 board1 piece


--En la posicion original d ela pieza le aplicamos un set y la dejamos como empty
auxNewBoard2 :: OnitamaBoard -> OnitamaPiece -> OnitamaBoard
auxNewBoard2 board (Master py (x,y)) = Matrix ((take (x-1) (getBoard board)) ++ [(changePiece ((getBoard board) !! (x-1)) (emptyPieceInOldCord (x,y)) (x,y))] ++ (drop x (getBoard board)))
auxNewBoard2 board (Apprentice py (x,y)) = Matrix ((take (x-1) (getBoard board)) ++ [(changePiece ((getBoard board) !! (x-1)) (emptyPieceInOldCord (x,y)) (x,y))] ++ (drop x (getBoard board)))


--Cambiamos la pieza en la corredenada exacta
changePiece :: [OnitamaPiece] -> OnitamaPiece -> (Int, Int) -> [OnitamaPiece]
changePiece board pice (x,y) = take (y-1) board ++ [pice] ++ drop y board

emptyPieceInOldCord ::  (Int,Int) -> OnitamaPiece 
emptyPieceInOldCord a = Empty a

changeCordPice :: OnitamaPiece -> (Int, Int) -> OnitamaPiece
changeCordPice (Master r j) newC = Master r newC
changeCordPice (Apprentice r j) newC = Apprentice r newC
changeCordPice (Empty j) newC = Empty newC

result :: OnitamaGame -> [GameResult OnitamaPlayer]
result (OnitamaGame _ _ _ _ finish) = finish

--Log de la accion a realizar, para poder interpretar y corroborar las acciones que vamos a realizar
{-instance Show (OnitamaAction) where
        show (OnitamaAction pice card destino) = "La OnitamaPiece " ++ show pice ++
                                          " Se encuentra en las cordenadas " ++ show stay ++ 
                                          " solicita moverse hacia " ++ show destino ++ 
                                          " mediante la OnitamaCard " ++ show card
                                          where stay = getPositionPice pice -}

showAction :: OnitamaAction -> String
showAction action = show action
