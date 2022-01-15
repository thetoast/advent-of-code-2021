module Day18 where

import Prelude

import Control.Monad.State (StateT, evalStateT, get, gets, lift, modify_, put)
import Data.Array (concat, dropEnd, last, length, snoc, takeEnd, uncons)
import Data.Either (Either(..), note)
import Data.Foldable (foldl, maximum)
import Data.Int (fromString, floor, ceil, toNumber)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), splitAt)
import Data.String as String
import Data.String.CodeUnits (charAt)
import Data.String.Regex (search)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)

type ID
  = Int

data SFNum
  = SFNum ID Int
  | SFNumPair ID SFNum SFNum

instance eqSFNum :: Eq SFNum where
  eq (SFNum _ i1) (SFNum _ i2) = i1 == i2
  eq (SFNumPair _ n11 n12) (SFNumPair _ n21 n22) = n11 == n21 && n12 == n22
  eq _ _ = false

instance showSFNum :: Show SFNum where
  show (SFNum _ i) = show i
  show (SFNumPair _ n1 n2) = "[" <> show n1 <> "," <> show n2 <> "]"

showWithIds :: SFNum -> String
showWithIds (SFNum id i) = show id <> ":" <> show i

showWithIds (SFNumPair id n1 n2) = "[" <> show id <> ": " <> showWithIds n1 <> "," <> showWithIds n2 <> "]"

type ParserState
  = { input :: String, id :: Int }

type Parse
  = StateT ParserState (Either String)

sLeft :: forall a. String -> Parse a
sLeft = lift <<< Left

sNote :: forall a. String -> Maybe a -> Parse a
sNote s = note s >>> lift

setInput :: String -> Parse Unit
setInput input = modify_ \s -> s { input = input }

nextId :: Parse Int
nextId = do
  { id } <- get
  _ <- modify_ \s -> s { id = id + 1 }
  pure id

checkChar :: String -> Parse Unit
checkChar s = do
  { before, after } <- gets _.input <#> splitAt 1
  if before == s then do
    setInput after
  else
    lift $ Left $ "checkChar fail: expected=" <> s <> " actual=" <> before

parsePair :: Parse SFNum
parsePair = do
  id <- nextId
  _ <- checkChar "["
  first <- parseNum
  _ <- checkChar ","
  second <- parseNum
  _ <- checkChar "]"
  pure $ SFNumPair id first second

parseNum :: Parse SFNum
parseNum =
  gets _.input <#> charAt 0
    >>= case _ of
        Just '[' -> parsePair
        Just _ -> parseRegular
        Nothing -> sLeft "could not parse empty string"

parseRegular :: Parse SFNum
parseRegular = do
  { input, id } <- get
  i <- search (unsafeRegex "[,\\]]" noFlags) input # note ("could not find index of ',' or ']' in " <> input) # lift
  { before, after } <- pure $ splitAt i input
  num <- sNote ("could not parse regular from" <> before) $ fromString before <#> SFNum id
  put { input: after, id: id + 1 } $> num

makeState :: String -> ParserState
makeState input = { input, id: 1 }

parseInput :: String -> Either String SFNum
parseInput = makeState >>> evalStateT parseNum

fixIds :: SFNum -> SFNum
fixIds num = setId num 0
  where
  setId (SFNum _ n) id = SFNum id n

  setId (SFNumPair _ n1 n2) id = SFNumPair id (setId n1 (id + 1)) (setId n2 (id + 2))

addSFNums :: SFNum -> SFNum -> SFNum
addSFNums n1 n2 = SFNumPair 0 n1 n2 # fixIds # reduce

isSame :: SFNum -> SFNum -> Boolean
isSame n1 n2
  | (SFNum id1 _) <- n1, (SFNum id2 _) <- n2 = id1 == id2
  | (SFNumPair id1 _ _) <- n1, (SFNumPair id2 _ _) <- n2 = id1 == id2
  | otherwise = false

findExplode :: SFNum -> Maybe (Array SFNum)
findExplode num = checkNode num [ num ]
  where
  checkNode (SFNum _ _) _ = Nothing

  checkNode (SFNumPair _ n1 n2) hist = case length hist of
    5 -> Just hist
    _ -> case checkNode n1 $ snoc hist n1 of
      Just hist' -> Just hist'
      Nothing -> checkNode n2 $ snoc hist n2

leftMostRegular :: SFNum -> Maybe (Array SFNum)
leftMostRegular num = checkNode num [ num ]
  where
  checkNode (SFNumPair _ n@(SFNum _ _) _) hist = Just $ snoc hist n

  checkNode (SFNumPair _ n@(SFNumPair _ _ _) _) hist = checkNode n $ snoc hist n

  checkNode _ _ = Nothing

rightMostRegular :: SFNum -> Maybe (Array SFNum)
rightMostRegular num = checkNode num [ num ]
  where
  checkNode (SFNumPair _ _ n@(SFNum _ _)) hist = Just $ snoc hist n

  checkNode (SFNumPair _ _ n@(SFNumPair _ _ _)) hist = checkNode n $ snoc hist n

  checkNode _ _ = Nothing

isLeafLeft :: Array SFNum -> Boolean
isLeafLeft hist = case takeEnd 2 hist of
  [ (SFNumPair _ l _), child ] -> isSame child l
  _ -> false

isLeafRight :: Array SFNum -> Boolean
isLeafRight hist = case takeEnd 2 hist of
  [ (SFNumPair _ _ r), child ] -> isSame child r
  _ -> false

closestRightToLeaf :: Array SFNum -> Maybe (Array SFNum)
closestRightToLeaf hist
  | isLeafLeft hist = case takeEnd 2 hist of
    [ (SFNumPair _ _ n@(SFNum _ _)), _ ] -> Just $ snoc (dropEnd 1 hist) n
    [ (SFNumPair _ _ n@(SFNumPair _ _ _)), _ ] -> leftMostRegular n <#> \a -> concat [ (dropEnd 1 hist), a ]
    _ -> Nothing
  | [] <- hist = Nothing
  | otherwise = closestRightToLeaf $ dropEnd 1 hist

closestLeftToLeaf :: Array SFNum -> Maybe (Array SFNum)
closestLeftToLeaf hist
  | isLeafRight hist = case takeEnd 2 hist of
    [ (SFNumPair _ n@(SFNum _ _) _), _ ] -> Just $ snoc (dropEnd 1 hist) n
    [ (SFNumPair _ n@(SFNumPair _ _ _) _), _ ] -> rightMostRegular n <#> \a -> concat [ (dropEnd 1 hist), a ]
    _ -> Nothing
  | [] <- hist = Nothing
  | otherwise = closestLeftToLeaf $ dropEnd 1 hist

explode :: SFNum -> Maybe SFNum
explode = findExplode >=> explodePath

replaceLeaf :: Array SFNum -> SFNum -> Maybe SFNum
replaceLeaf hist newLeaf = do
  parentHist <- pure $ dropEnd 1 hist
  case last parentHist of
    Just (SFNumPair _ l r) -> do
      newParent <- pure $ if isLeafLeft hist then (SFNumPair 0 newLeaf r) else (SFNumPair 0 l newLeaf)
      replaceLeaf parentHist newParent
    _ -> Just newLeaf

addToLeaf :: Int -> Array SFNum -> Maybe SFNum
addToLeaf i hist = do
  case last hist of
    Just (SFNum _ n) -> do
      newLeaf <- pure (SFNum 0 (n + i))
      replaceLeaf hist newLeaf
    _ -> Nothing

idOf :: SFNum -> Int
idOf (SFNum id _) = id

idOf (SFNumPair id _ _) = id

choose :: SFNum -> SFNum -> Maybe SFNum
choose n1 n2 =
  if idOf n1 == 0 && idOf n2 == 0 then
    Nothing
  else if idOf n1 == 0 then
    Just n1
  else if idOf n2 == 0 then
    Just n2
  else if n1 == n2 then
    Just n1
  else
    Nothing

merge :: SFNum -> SFNum -> Maybe SFNum
merge (SFNumPair _ l1 l2) (SFNumPair _ r1 r2) = do
  newLeft <- case choose l1 r1 of
    Nothing -> merge l1 r1
    j -> j
  newRight <- case choose l2 r2 of
    Nothing -> merge l2 r2
    j -> j
  Just $ (SFNumPair 0 newLeft newRight)

merge _ _ = Nothing

explodePath :: Array SFNum -> Maybe SFNum
explodePath path = case last path of
  Just (SFNumPair _ (SFNum _ l) (SFNum _ r)) -> do
    newMiddle <- replaceLeaf path (SFNum 0 0)
    mergeLeft <- case closestLeftToLeaf path >>= addToLeaf l of
      Just left -> merge left newMiddle
      Nothing -> Just newMiddle
    final <- case closestRightToLeaf path >>= addToLeaf r of
      Just right -> merge right mergeLeft
      Nothing -> Just mergeLeft
    pure $ fixIds final
  _ -> Nothing

findSplit :: SFNum -> Maybe (Array SFNum)
findSplit num = checkNode num [ num ]
  where
  checkNode (SFNum _ i) hist = if i >= 10 then Just hist else Nothing

  checkNode (SFNumPair _ n1 n2) hist = case checkNode n1 $ snoc hist n1 of
    Just hist' -> Just hist'
    Nothing -> checkNode n2 $ snoc hist n2

splitPath :: Array SFNum -> Maybe SFNum
splitPath path = case last path of
  Just (SFNum _ i) ->
    replaceLeaf path
      ( toNumber i
          # \n ->
              (SFNumPair 0 (SFNum 0 $ floor (n / 2.0)) (SFNum 0 $ ceil (n / 2.0)))
      )
      <#> fixIds
  _ -> Nothing

split :: SFNum -> Maybe SFNum
split = findSplit >=> splitPath

reduce :: SFNum -> SFNum
reduce num = case explode num of
  Just exploded -> reduce exploded
  Nothing -> case split num of
    Just split' -> reduce split'
    Nothing -> num

magnitude :: SFNum -> Int
magnitude (SFNumPair _ n1 n2) = (3 * magnitude n1) + (2 * magnitude n2)

magnitude (SFNum _ i) = i

permutations :: Array SFNum -> Array SFNum
permutations arr = do
  a1 <- arr
  a2 <- arr
  if a1 == a2 then [] else [addSFNums a1 a2]

solve1 :: String -> Either String Int
solve1 inputs = do
  numbers <- String.split (Pattern "\n") inputs # traverse parseInput
  { head, tail } <- note "could not get first item" $ uncons numbers
  finalNum <- pure $ foldl addSFNums head tail
  pure $ magnitude finalNum

solve2 :: String -> Either String Int
solve2 inputs = do
  numbers <- String.split (Pattern "\n") inputs # traverse parseInput
  perms <- pure $ permutations numbers
  note "Could not find max" $ maximum $ magnitude <$> perms
