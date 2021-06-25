data OnitamaPlayer = RedPlayer | BluePlayer deriving (Eq, Show, Enum)

data OnitamaPiece = Master OnitamaPlayer | Apprentice OnitamaPlayer| Empty 
                    deriving (Eq, Show, Enum, Ord)

data OnitamaCard = Tiger | Dragon | Rabbit | Monkey | Crab | Elephant
                | Mantis | Crane | Frog | Boar | Goose | Horse | Rooster | Ox | Eel | Cobra 
                deriving (Eq, Show, Ord, Enum)

data OnitamaAction = Move OnitamaCard (Int, Int) (Int, Int) deriving (Eq, Show)

data GameResult p = Winner p | Loser p | Draw deriving (Eq, Show)

data OnitamaBoard = Matrix (Piece, Piece, Piece, Piece, Piece) deriving (Eq, Show)

data Piece = Position (OnitamaPiece, OnitamaPiece, OnitamaPiece, OnitamaPiece, OnitamaPiece) deriving (Eq, Show)

data OnitamaGame = 

beginning :: [OnitamaCard] -> OnitamaGame

activePlayer :: OnitamaGame -> OnitamaPlayer

actions :: OnitamaGame -> [(OnitamaPlayer, [OnitamaAction])]

next :: OnitamaGame -> OnitamaPlayer -> OnitamaAction -> OnitamaGame

result :: OnitamaGame -> [GameResult OnitamaPlayer]

score :: OnitamaGame -> [(OnitamaPlayer, Double)]

showGame :: OnitamaGame -> String

