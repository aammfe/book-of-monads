module TicTacToe.Final where 

-- when ever I'm making a typeclass dependent on m, I'm making it impure ?

class MonadState board m => IsBoard m player board where
    get_ :: Position -> m (Maybe player)
    put_ :: player -> Position -> m () 

-- instance type Board = Map Position Player

data Axis = One | Two | Three 
    deriving stock (Eq, Ord)

data Position = Position {col :: Axis, row::Axis} 
    deriving stock (Eq, Ord)

class Eq player => IsPlayer m player where
    next :: player -> m player

-- data Player = O | X 
--     deriving stock (Eq, Ord)

data Result player baord = AlreadyTaken { by :: player} 
            | NextTurn { _of :: player } 
            | GameEnded { winner :: player }
            deriving stock (Eq, Ord)

take_ :: forall m board player .(IsBoard m player board, IsPlayer m player) => player -> Position -> m (Result player board)
take_ player pos = do
    i <- get_ pos
    case i of 
        Just p -> return $ AlreadyTaken p
        Nothing -> do 
            put_ player pos
            won <- hasWon @m @board player
            if won 
                then return (GameEnded player) 
                else do
                 np <- next player
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

hasWon :: forall m board player. 
    ( IsBoard m player board
    , IsPlayer m player
    ) => player -> m Bool
hasWon player = any id <$> traverse (isOccupiedBy @m @board @player player) winningLines 


isOccupiedBy :: forall m board player. 
                (IsBoard m player board
                , Eq player
                ) => 
                player -> [Position] -> m Bool
isOccupiedBy p [x, y, z] = do
    r <- runMaybeT do
        x' <- MaybeT $ get_ @m @player @board x
        y' <- MaybeT $ get_ @m @player @board y
        z' <- MaybeT $ get_ @m @player @board z
        return $ x' == p && x' == y' && z' == y'
    return . isJust $ r
isOccupiedBy _ _ = return False