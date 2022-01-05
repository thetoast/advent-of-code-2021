module Day16 where

import Prelude

import Control.Monad.State (State, StateT, evalStateT, gets, lift, modify_)
import Data.Array (replicate)
import Data.BigInt (BigInt, fromBase, toBase, fromInt)
import Data.Either (Either(..), note)
import Data.Foldable (foldl, intercalate, maximum, minimum, product, sum)
import Data.Int (binary, fromStringAs)
import Data.List (List(..), index, snoc)
import Data.Maybe (Maybe(..))
import Data.String (splitAt)
import Data.String as String
import Data.Traversable (traverse)

type Packet
  = { version :: PacketVersion
    , type :: PacketType
    , data :: PacketData
    }

type PacketVersion
  = Int

data PacketType
  = Literal
  | Op Int

instance showPacketType :: Show PacketType where
  show Literal = "Literal"
  show (Op i) = "Op: " <> show i

data PacketData
  = LiteralValue BigInt
  | OpPackets (List Packet)

instance showPacketData :: Show PacketData where
  show (LiteralValue i) = show i
  show (OpPackets packets) = "[" <> show packets <> "]"

data PacketLength
  = Bits Int
  | Packets Int

instance showPacketLength :: Show PacketLength where
  show (Bits i) = show i <> " bits"
  show (Packets i) = show i <> " packets"

type ParserState
  = State StateData

type ParserState2
  = StateT StateData (Either String)

type StateData
  = { input :: String
    , version :: Maybe Int
    , type :: Maybe PacketType
    , length :: Maybe PacketLength
    , data :: Maybe PacketData
    }

parseVersion :: ParserState2 PacketVersion
parseVersion = do
  { before, after } <- splitAt 3 <$> gets _.input
  version <- lift $ note "could not parse version" $ fromStringAs binary before
  _ <- modify_ \s -> s { input = after, version = Just version }
  pure version

parseType :: ParserState2 PacketType
parseType = do
  { before, after } <- splitAt 3 <$> gets _.input
  packetType <-
    lift
      $ case fromStringAs binary before of
          Just 4 -> Right Literal
          Just n -> Right (Op n)
          Nothing -> Left "could not parse type"
  _ <- modify_ \s -> s { input = after, type = Just packetType }
  pure packetType

parseBitLength :: ParserState2 PacketLength
parseBitLength = do
  { before, after } <- splitAt 15 <$> gets _.input
  length <-
    lift
      $ case fromStringAs binary before of
          Just n -> Right $ Bits n
          Nothing -> Left "could not parse bit length"
  _ <- modify_ \s -> s { input = after, length = Just length }
  pure length

parsePacketLength :: ParserState2 PacketLength
parsePacketLength = do
  { before, after } <- splitAt 11 <$> gets _.input
  length <-
    lift
      $ case fromStringAs binary before of
          Just n -> Right $ Packets n
          Nothing -> Left "could not parse packet length"
  _ <- modify_ \s -> s { input = after, length = Just length }
  pure length

parseLength :: ParserState2 PacketLength
parseLength = do
  { before, after } <- splitAt 1 <$> gets _.input
  _ <- modify_ \s -> s { input = after }
  case fromStringAs binary before of
    Just 0 -> parseBitLength
    Just 1 -> parsePacketLength
    Just _ -> lift $ Left "unsupported length type"
    Nothing -> lift $ Left "could not parse length type"

checkLastChunk :: ParserState2 Boolean
checkLastChunk = do
  { before, after } <- splitAt 1 <$> gets _.input
  _ <- modify_ \s -> s { input = after }
  case fromStringAs binary before of
    Just 0 -> pure true
    Just 1 -> pure false
    Just _ -> lift $ Left "unsupported last chunk bit"
    Nothing -> lift $ Left "could not parse last chunk bit"

parseChunks :: String -> ParserState2 String
parseChunks str = do
  isLast <- checkLastChunk
  { before, after } <- splitAt 4 <$> gets _.input
  _ <- modify_ \s -> s { input = after }
  newStr <- pure $ str <> before
  if isLast then pure newStr else parseChunks newStr

parseLiteral :: ParserState2 PacketData
parseLiteral = do
  literalStr <- parseChunks ""
  packetData <- case fromBase 2 literalStr of
    Just n -> pure $ LiteralValue n
    Nothing -> lift $ Left $ "could not parse literal string: " <> literalStr
  _ <- modify_ \s -> s { data = Just packetData }
  pure packetData

parseOpBits :: Int -> List Packet -> ParserState2 (List Packet)
parseOpBits n packets = do
  preLen <- String.length <$> gets _.input
  packet <- parsePacket
  numRead <- sub preLen <$> String.length <$> gets _.input
  newPackets <- pure $ snoc packets packet
  if numRead == n then pure newPackets else parseOpBits (n - numRead) newPackets

parseOpPackets :: Int -> List Packet -> ParserState2 (List Packet)
parseOpPackets n packets = do
  packet <- parsePacket
  newPackets <- pure $ snoc packets packet
  if n - 1 == 0 then pure newPackets else parseOpPackets (n - 1) newPackets

parseOpData :: ParserState2 PacketData
parseOpData = do
  len <- parseLength
  packets <-
    OpPackets
      <$> case len of
          Bits n -> parseOpBits n Nil
          Packets n -> parseOpPackets n Nil
  _ <- modify_ \s -> s { data = Just packets }
  pure packets

parseData :: PacketType -> ParserState2 PacketData
parseData = case _ of
  Literal -> parseLiteral
  Op _ -> parseOpData

parsePacket :: ParserState2 Packet
parsePacket = do
  packetVersion <- parseVersion
  packetType <- parseType
  packetData <- parseData packetType
  pure { version: packetVersion, type: packetType, data: packetData }

makeState :: String -> StateData
makeState input = { input, version: Nothing, type: Nothing, length: Nothing, data: Nothing }

padZeros :: String -> String
padZeros input = String.length input # sub 4 # flip mod 4 # flip replicate "0" # intercalate "" # flip (<>) input

parseInput :: String -> Either String Packet
parseInput input = do
  binStr <- note "could not parse input" $ fromBase 16 input <#> toBase 2 <#> padZeros
  evalStateT parsePacket (makeState binStr)

sumVersion :: Int -> Packet -> Int
sumVersion sum { type: Literal, version } = sum + version

sumVersion sum { type: Op _, version, data: OpPackets packets } = foldl sumVersion (sum + version) packets

sumVersion sum _ = sum

solve1 :: String -> Either String Int
solve1 input = do
  packet <- parseInput input
  pure $ sumVersion 0 packet

binOp :: (BigInt -> BigInt -> Boolean) -> List Packet -> Either String BigInt
binOp op packets = do
  l <- note "could not get binOp[0]" $ index packets 0 
  r <- note "could not get binOp[1]" $ index packets 1 
  lVal <- packetValue l
  rVal <- packetValue r
  pure if (op lVal rVal) then fromInt 1 else fromInt 0

packetValue :: Packet -> Either String BigInt
packetValue packet = case packet.data of
  LiteralValue n -> Right n
  OpPackets packets -> case packet.type of
    Op 0 -> sum <$> traverse packetValue packets
    Op 1 -> product <$> traverse packetValue packets
    Op 2 -> note "could not find min value" =<< minimum <$> traverse packetValue packets
    Op 3 -> note "could not find max value" =<< maximum <$> traverse packetValue packets
    Op 5 -> binOp (>) packets
    Op 6 -> binOp (<) packets
    Op 7 -> binOp (==) packets
    Op n -> Left $ "bad operation: " <> show n
    Literal -> Left $ "corrupt packet; op with literal data"

solve2 :: String -> Either String BigInt
solve2 input = do
  packet <- parseInput input
  packetValue packet
