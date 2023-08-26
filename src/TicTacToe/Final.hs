module TicTacToe.Final where 

-- when ever I'm making a typeclass dependent on m, I'm making it impure ?

class GamePlay m player where
    getCell :: Position -> m (Maybe player)
    putCell :: player -> Position -> m ()
    nextPlayer :: player -> m player

-- instance type Board = Map Position Player

data Axis = One | Two | Three 
    deriving stock (Eq, Ord)

data Position = Position {col :: Axis, row::Axis} 
    deriving stock (Eq, Ord)

-- data Player = O | X 
--     deriving stock (Eq, Ord)

data Result player = AlreadyTaken { by :: player} 
            | NextTurn { _of :: player } 
            | GameEnded { winner :: player }
            deriving stock (Eq, Ord)

take_ :: forall m player .(GamePlay m player, Eq player, Monad m) => player -> Position -> m (Result player)
take_ player pos = do
    i <- getCell @m @player pos
    case i of 
        Just p -> return $ AlreadyTaken p
        Nothing -> do 
            putCell @m @player player pos
            won <- hasWon @m player
            if won 
                then return (GameEnded player) 
                else do
                 np <- nextPlayer @m @player player
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

hasWon :: forall m player. (GamePlay m player, Monad m, Eq player) => player -> m Bool
hasWon player = any id <$> traverse (isOccupiedBy @m @player player) winningLines 


isOccupiedBy :: forall m player. 
                (GamePlay m player
                , Eq player
                , Monad m
                ) => 
                player -> [Position] -> m Bool
isOccupiedBy p [x, y, z] = do
    r <- runMaybeT do
        x' <- MaybeT $ getCell @m @player x
        y' <- MaybeT $ getCell @m @player y
        z' <- MaybeT $ getCell @m @player z
        return $ x' == p && x' == y' && z' == y'
    return . isJust $ r
isOccupiedBy _ _ = return False