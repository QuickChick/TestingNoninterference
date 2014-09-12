{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FunctionalDependencies, 
  FlexibleInstances #-}
module Memory where

import Labels
import Data.Map (Map)
import qualified Data.Map as Map

-- Memory Frames:
-- * A list of data
-- * A stamp denoting the context in which they were created
-- * A label protecting the data
data Frame a = Frame Label Label [a]
             deriving (Eq, Show, Read)

-- Blocks (Pointers to frames)
-- * A label that is the stamp of the target frame
-- * An index into the frame list at the *stamp* level
data Block = Block Int Label
           deriving (Eq, Ord, Show, Read) -- Ord For Map

stamp :: Block -> Label
stamp (Block _ l) = l

putStamp :: Label -> Block -> Block
putStamp l (Block n _) = Block n l

class (Eq m, Show m, Read m) => MemC m a | m -> a where
    next :: m -> Label -> Int 
    getFrame :: m -> Block -> Maybe (Frame a)
    empty :: m
    updFrame :: m -> Block -> Frame a -> Maybe m
    allocate :: m -> Label -> Frame a -> (Block, m)

    getBlocksAtLevel :: m -> Label -> [Block]
    getBlocksBelow   :: Label -> m -> [Block]
    mapMemory :: (Frame a -> Frame a) -> m -> m

-- Memory:
-- * Contents: A map from Blocks to Frames
-- * Sizes   : A map from stamps to the length of the list of 
--             frames at that level
data Mem a = Mem { contents :: Map Block (Frame a)
                 , sizes    :: Map Label Int}
            deriving (Eq, Show, Read)

-- Maps as memories 
instance (Eq a, Show a, Read a) => MemC (Mem a) a where
    next Mem{..} l = Map.findWithDefault 0 l sizes
    getFrame Mem{..} block = Map.lookup block contents
    empty = Mem Map.empty Map.empty
    updFrame m@Mem{..} b f = do 
      _ <- getFrame m b
      return $ Mem (Map.insert b f contents) sizes
    allocate m@Mem{..} l f = 
        (b, Mem (Map.insert b f contents)
              (Map.insertWith (+) l 1 sizes))
        where b = Block (next m l) l
    
    getBlocksAtLevel m l = map (flip Block l) [0..(next m l- 1)]

    getBlocksBelow l m = concatMap (getBlocksAtLevel m) (labelsBelow l)

    mapMemory f m@Mem{..} = m{contents = Map.map f contents}


