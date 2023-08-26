module TicTacToe.Initial where



data Axis = One | Two | Three 
    deriving stock (Eq, Ord)

data Position = Position {col :: Axis, row::Axis} 
    deriving stock (Eq, Ord)

data Result player = AlreadyTaken { by :: player} 
            | NextTurn { _of :: player } 
            | GameEnded { winner :: player }
            deriving stock (Eq, Ord)


data TicTacToe player a = GetCell Position (Maybe player -> TicTacToe player a)
                        | PutCell player Position (() -> TicTacToe player a)
                        | NextPlayer player (player -> TicTacToe player a)
                        | Done a
                        deriving stock Functor


instance Applicative (TicTacToe player) where
    pure = Done
    f <*> a = do
       a' <- a
       f' <- f
       return $ f' a'


instance Monad (TicTacToe player) where
    (Done x) >>= f = f x
    (GetCell p g) >>= f = GetCell p (\v -> g v >>= f)
    (PutCell pl pos g) >>= f = PutCell pl pos (\v -> g v >>= f)
    (NextPlayer p g) >>= f = NextPlayer p (\v -> g v >>= f)


getCell :: Position -> TicTacToe player (Maybe player)
getCell p = GetCell p return


putCell :: player -> Position -> TicTacToe player ()
putCell pl po = PutCell pl po return


nextPlayer :: player -> TicTacToe player player
nextPlayer p = NextPlayer p return


take_ :: Eq player => player -> Position -> TicTacToe player (Result player)
take_ player pos = do
    i <- getCell pos
    case i of 
        Just p -> return $ AlreadyTaken p
        Nothing -> do 
            putCell player pos
            won <- hasWon player
            if won 
                then return (GameEnded player) 
                else do
                 np <- nextPlayer player
                 return $ NextTurn np


winningLines :: [[Position]]
winningLines = [ --Horizontal
                 [(Position One One), (Position Two One), (Position Three One)]
               , [(Position One Two), (Position Two Two), (Position Three Two)]
               , [(Position One Three), (Position Two Three), (Position Three Three)]

                --vertical
               , [(Position One One), (Position One Two), (Position One Three)]
               , [(Position Two One), (Position Two Two), (Position Two Three)]
               , [(Position Three One), (Position Three Two), (Position Three Three)]
                --diagonal
               , [(Position One One), (Position Two Two), (Position Three Three)]
               , [(Position Three One), (Position Two Two), (Position Three One)]
               ]

hasWon :: Eq player => player -> TicTacToe player Bool
hasWon player = any id <$> traverse (isOccupiedBy player) winningLines 


isOccupiedBy :: Eq player => player -> [Position] -> TicTacToe player Bool
isOccupiedBy p [x, y, z] = do
    r <- runMaybeT do
        x' <- MaybeT $ getCell x
        y' <- MaybeT $ getCell y
        z' <- MaybeT $ getCell z
        return $ x' == p && x' == y' && z' == y'
    return . isJust $ r
isOccupiedBy _ _ = return False