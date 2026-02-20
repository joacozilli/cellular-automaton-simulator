{-# LANGUAGE FlexibleInstances #-}
module Types where


import           Data.Word (Word32)
import qualified Data.Vector as Vector
import qualified Data.Vector.Storable as SVector
import qualified Data.Map.Strict as Map

-- rgba format of a color packed in a 32 bits word with order of bytes [r,g,b,a]
type RGBA = Word32

type Name = String
type State = Name

-- literal coordinate in grid
newtype Coord = Coord (Int, Int) deriving Show

instance Num Coord where
   (Coord (x1, y1)) + (Coord (x2, y2)) = Coord (x1 + x2, y1 + y2)
   (-) = undefined
   (*) = undefined
   abs = undefined
   signum = undefined
   fromInteger = undefined

-- neighborhood as it is defined by automata
type Neighborhood = Vector.Vector Coord

type States = Map.Map State RGBA

-- cellular automata with its name, states, neighborhood, transition rule and default state
data Automata = CA Name States Neighborhood Rule State  deriving Show

data Rule = State (Exp State) | If (Exp Bool) Rule Rule | Let Name (Exp Int) Rule

deriving instance Show Rule

data Exp a where
    -- state expressions
    Self :: Exp State
    Neighbor :: Int -> Exp State
    Lit :: String -> Exp State

    -- int expressions
    Var :: String -> Exp Int
    Const :: Int -> Exp Int
    Neighbors :: Exp State -> Exp Int
    Opp :: Exp Int -> Exp Int
    
    -- boolean expressions
    BTrue :: Exp Bool
    BFalse :: Exp Bool
    And :: Exp Bool -> Exp Bool -> Exp Bool
    Or :: Exp Bool -> Exp Bool -> Exp Bool
    Not :: Exp Bool -> Exp Bool
    Lt :: Exp Int -> Exp Int -> Exp Bool
    Le :: Exp Int -> Exp Int -> Exp Bool
    Gt :: Exp Int -> Exp Int -> Exp Bool
    Ge :: Exp Int -> Exp Int -> Exp Bool
    EqInt :: Exp Int -> Exp Int -> Exp Bool
    NeqInt :: Exp Int -> Exp Int -> Exp Bool
    EqState :: Exp State -> Exp State -> Exp Bool
    NeqState :: Exp State -> Exp State -> Exp Bool
    In :: Exp State -> [Exp State] -> Exp Bool

deriving instance Show (Exp a)

type Vars = Map.Map Name Int

data Env = Env {cell :: Int,
                envConf :: Conf,
                envNeighbors :: LitNeighbors,
                envVars :: Vars,
                envFrontier :: Frontier,
                envDefaultColor :: RGBA
                }     

-- configuration of an instant
type Conf = (SVector.Vector RGBA, -- unidimensional representation of grid
                            Int,  -- number of rows
                            Int)  -- number of columns

-- literal neighborhood of each cell
type LitNeighbors = Vector.Vector (Vector.Vector Int)

-- type of frontier in simulation
data Frontier = Default  -- neighbors outside grid range are considered of default color
              | Toroidal -- grid is considered a toroid

-- world type for play
data World = World {transition :: Env -> RGBA,        -- converted transition rule
                    conf :: Conf,                     -- actual configuration
                    neighbors :: LitNeighbors,        -- literal neighborhood of each cell
                    colors :: [RGBA],                 -- list of all colors
                    defaultColor :: RGBA,             -- default color
                    frontier :: Frontier,             -- type of frontier in simulation
                    paused :: Bool,                   -- flag for paused state
                    initial :: Bool,                  -- flag for being the initial world
                    instant:: Int,                    -- current instant
                    drawScale :: Float,               -- draw scale for drawing
                    translation :: (Float,Float),     -- final picture translation
                    par :: Bool                       -- flag for parallel global transition
                    }

data Error = UndefState Name
           | NeighborOutOfRange Int
           | UndefVar Name
           | SameColor [State]

deriving instance Eq Error