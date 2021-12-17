module AoC.Days.Day16 where

import AoC.Lib.Parser
import AoC.Prelude hiding (many, oneOf, some)

parse :: String -> Maybe Packet
parse = parseMaybe rootP . h2b . head . lines

solveA :: Packet -> Int
solveA = checkSum

solveB :: Packet -> Int
solveB = eval

checkSum :: Packet -> Int
checkSum (PacketLit header _) = versionId header
checkSum (PacketOp header Op {subs}) = versionId header + sum (checkSum <$> subs)

eval :: Packet -> Int
eval (PacketLit _ (Lit i)) = i
eval (PacketOp Header {typeId} Op {subs})
  | typeId == 0 && length subs == 1 = eval (head subs)
  | typeId == 0 = sum $ eval <$> subs
  | typeId == 1 && length subs == 1 = eval (head subs)
  | typeId == 1 = product $ eval <$> subs
  | typeId == 2 = minimum $ eval <$> subs
  | typeId == 3 = maximum $ eval <$> subs
  | typeId == 5 && length subs == 2 = if eval (head subs) > eval (subs !! 1) then 1 else 0
  | typeId == 6 && length subs == 2 = if eval (head subs) < eval (subs !! 1) then 1 else 0
  | typeId == 7 && length subs == 2 = if eval (head subs) == eval (subs !! 1) then 1 else 0
  | otherwise = error "invalid Op"

data Packet
  = PacketLit Header Lit
  | PacketOp Header Op
  deriving stock (Show)

data Header = Header
  { versionId :: Int,
    typeId :: Int
  }
  deriving stock (Show)

newtype Lit = Lit Int
  deriving stock (Show)

data Op = Op
  { byLength :: Bool,
    subLength :: Maybe Int,
    subCount :: Maybe Int,
    subs :: [Packet]
  }
  deriving stock (Show)

rootP :: Parser Packet
rootP = packetP <* many bit0P

packetP :: Parser Packet
packetP = do
  header <- headerP
  if typeId header == 4
    then PacketLit header <$> litP
    else PacketOp header <$> opP

headerP :: Parser Header
headerP = do
  versionId <- b2d <$> count 3 bitP
  typeId <- b2d <$> count 3 bitP
  pure $ Header versionId typeId

litP :: Parser Lit
litP = Lit . b2d <$> litChunk
  where
    litChunk :: Parser String
    litChunk = do
      prefix <- count 1 bitP
      if b2d prefix == 0
        then count 4 bitP
        else do
          next <- count 4 bitP
          rest <- litChunk
          pure $ next <> rest

opP :: Parser Op
opP = do
  byLength <- (== 0) . b2d <$> count 1 bitP
  subSize <- b2d <$> count (if byLength then 15 else 11) bitP
  subs <-
    if byLength
      then do
        subsS <- count subSize bitP
        case runParser (some packetP) "" subsS of
          Right v -> pure v
          Left e -> error (show e)
      else count subSize packetP
  pure
    Op
      { byLength = byLength,
        subLength = if byLength then Just subSize else Nothing,
        subCount = if byLength then Nothing else Just subSize,
        subs = subs
      }

bitP, bit0P, bit1P :: Parser Char
bitP = oneOf ['0', '1']
bit0P = char '0'
bit1P = char '1'

b2d :: String -> Int
b2d = foldl' (\acc x -> 2 * acc + c2i x) 0
  where
    c2i :: Char -> Int
    c2i = \case
      '0' -> 0
      '1' -> 1
      _ -> error "Not bin"

h2b :: String -> String
h2b = concatMap $ \case
  '0' -> "0000"
  '1' -> "0001"
  '2' -> "0010"
  '3' -> "0011"
  '4' -> "0100"
  '5' -> "0101"
  '6' -> "0110"
  '7' -> "0111"
  '8' -> "1000"
  '9' -> "1001"
  'A' -> "1010"
  'B' -> "1011"
  'C' -> "1100"
  'D' -> "1101"
  'E' -> "1110"
  'F' -> "1111"
  _ -> error "Not hex"
