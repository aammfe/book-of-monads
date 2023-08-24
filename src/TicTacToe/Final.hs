module TicTacToe.Final where 

-- when ever I'm making a typeclass dependent on m, I'm making it impure ?

class IsBoard m player board where
    get_ :: board -> Position -> m (Maybe player)
    put_ :: board -> player -> Position -> m board 

-- instance type Board = Map Position Player

data Axis = One | Two | Three 
    deriving stock (Eq, Ord)

data Position = Position {col :: Axis, row::Axis} 
    deriving stock (Eq, Ord)

class Eq player => IsPlayer m player where
    next :: player -> m player

-- data Player = O | X 
--     deriving stock (Eq, Ord)

data Result player baord = AlreadyTaken { by :: player, board::baord } 
            | NextTurn { _of :: player, board::baord  } 
            | GameEnded { winner :: player, board::baord  }
            deriving stock (Eq, Ord)

take_ :: forall m board player .(IsBoard m player board, IsPlayer m player, Monad m) => board -> player -> Position -> m (Result player board)
take_ board player pos = do
    i <- get_ board pos
    case i of 
        Just p -> return $ AlreadyTaken p board
        Nothing -> do 
            board' <- put_ board player pos
            won <- hasWon @m board' player
            if won 
                then return (GameEnded player board) 
                else do
                 np <- next player
                 return $ NextTurn np board'


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
    , Applicative m
    , Monad m
    , IsPlayer m player
    ) => board -> player -> m Bool
hasWon b player = any id <$> traverse (isOccupiedBy b player) winningLines


isOccupiedBy :: forall m board player. 
                (IsBoard m player board 
                , Monad m
                , Eq player
                ) => 
                board -> player -> [Position] -> m Bool
isOccupiedBy b p [x, y, z] = do
    r <- runMaybeT do
        x' <- MaybeT $ get_ @m @player b x
        y' <- MaybeT $ get_ @m @player b y
        z' <- MaybeT $ get_ @m @player b z
        return $ x' == p && x' == y' && z' == y'
    return . isJust $ r
isOccupiedBy _ _ _ = return False