module Labels where

-- Four point Lattice. Should be enough.
data Label = L | M1 | M2 | H
  deriving (Eq, Read, Show, Ord) -- Ord IS NOT flowsTo. Used for Map

bot :: Label
bot = L

flowsTo :: Label -> Label -> Bool
flowsTo L  L  = True
flowsTo L  M1 = True
flowsTo L  M2 = True
flowsTo L  H  = True
flowsTo M1 M1 = True
flowsTo M1 H  = True
flowsTo M2 M2 = True
flowsTo M2 H  = True
flowsTo H  H  = True
flowsTo _  _ = False

-- Aliases
isLow :: Label -> Label -> Bool
isLow = flowsTo

isHigh :: Label -> Label -> Bool
isHigh = (not .) . isLow
               
flows :: Label -> Label -> Int
flows l1 l2 = if flowsTo l1 l2 then 1 else 0

lub :: Label -> Label -> Label
lub _ H = H
lub H _ = H
lub L l = l
lub l L = l
lub M1 M2 = H
lub M2 M1 = H
lub x  _x' = x -- x == _x'

meet :: Label -> Label -> Label
meet L  _  = L
meet _  L  = L
meet H  l  = l
meet l  H  = l
meet M1 M2 = L
meet M2 M1 = L
meet x  _x' = x -- x == _x'

-- All Smaller Labels
labelsBelow :: Label -> [Label]
labelsBelow L  = [L]
labelsBelow M1 = [L, M1]
labelsBelow M2 = [L, M2]
labelsBelow H  = [L, M1, M2, H]


