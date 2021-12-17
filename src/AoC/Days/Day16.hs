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
checkSum (PLit Header {versionId} _) = versionId
checkSum (POp Header {versionId} (Op subs)) = versionId + sum (checkSum <$> subs)

eval :: Packet -> Int
eval (PLit _ (Lit i)) = i
eval (POp Header {typeId} (Op subs))
  | typeId == 0 = sum $ eval <$> subs
  | typeId == 1 = product $ eval <$> subs
  | typeId == 2 = minimum $ eval <$> subs
  | typeId == 3 = maximum $ eval <$> subs
  | typeId == 5 && length subs == 2 = if eval (head subs) > eval (subs !! 1) then 1 else 0
  | typeId == 6 && length subs == 2 = if eval (head subs) < eval (subs !! 1) then 1 else 0
  | typeId == 7 && length subs == 2 = if eval (head subs) == eval (subs !! 1) then 1 else 0
  | otherwise = error "invalid Op"

data Packet
  = PLit Header Lit
  | POp Header Op
  deriving stock (Show)

data Header = Header
  { versionId :: Int,
    typeId :: Int
  }
  deriving stock (Show)

newtype Lit = Lit Int
  deriving stock (Show)

newtype Op = Op [Packet]
  deriving stock (Show)

rootP :: Parser Packet
rootP = packetP <* many bit0

packetP :: Parser Packet
packetP = do
  header <- headerP
  if typeId header == 4
    then PLit header <$> litP
    else POp header <$> opP

headerP :: Parser Header
headerP = do
  versionId <- b2d <$> bits 3
  typeId <- b2d <$> bits 3
  pure $ Header versionId typeId

litP :: Parser Lit
litP = Lit . b2d <$> litChunk
  where
    litChunk :: Parser String
    litChunk = do
      prefix <- b2d <$> bits 1
      if prefix == 0
        then bits 4
        else do
          next <- bits 4
          rest <- litChunk
          pure $ next <> rest

opP :: Parser Op
opP = do
  byLength <- (== 0) . b2d <$> bits 1
  subSize <- b2d <$> bits (if byLength then 15 else 11)
  subs <-
    if byLength
      then do
        subsS <- bits subSize
        case runParser (some packetP) "" subsS of
          Right v -> pure v
          Left e -> error (show e)
      else count subSize packetP
  pure $ Op subs

bits :: Int -> Parser String
bits n = count n (bit0 <|> bit1)

bit0, bit1 :: Parser Char
bit0 = char '0'
bit1 = char '1'

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
