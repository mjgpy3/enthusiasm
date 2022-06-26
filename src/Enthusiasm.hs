{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Enthusiasm
    ( Reg(..)
    , seti
    , addi
    , addr
    , jmpn
    , jmpa
    , modr
    , says
    , sayr
    , halt
    , (>>)
    , execute
    , a
    , b
    , c
    , d
    , e
    , f
    , g
    , h
    ) where

import Prelude hiding ((>>))

import Control.Monad.IO.Class
import Data.Text (Text, unpack)
import qualified Data.Map.Strict as M
import qualified Data.Sequence.NonEmpty as NES
import qualified Data.Vector as V
import Data.Foldable (toList)

data Reg
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  deriving (Show, Ord, Eq)

data Val
  = RVal Reg
  | NVal Int
  deriving (Show)

instance Num Val where
  fromInteger = NVal . fromInteger

a = RVal A
b = RVal B
c = RVal C
d = RVal D
e = RVal E
f = RVal F
g = RVal G
h = RVal H

data Inst
  = Addr Val Val Reg
  | Jmpn Int Reg
  | Jmpa Int
  | Modr Val Val Reg
  | Says Text
  | Sayr Reg
  | Halt
  deriving Show

newtype Code = Code { uncode :: NES.NESeq Inst }
  deriving Show

(>>) :: Code -> Code -> Code
Code left >> Code right =
  Code $ left NES.>< right

one inst a = Code $ NES.singleton $ inst a
two inst a b = Code $ NES.singleton $ inst a b
three inst a b c = Code $ NES.singleton $ inst a b c

seti i o = Code $ NES.singleton $ Addr 0 (NVal i) o
addi i o = Code $ NES.singleton $ Addr i (RVal o) o
addr = three Addr
jmpn = two Jmpn
jmpa = one Jmpa
modr = three Modr
says = one Says
sayr = one Sayr
halt = Code $ NES.singleton Halt

data ProgState
  = ProgState {
    ip :: Int
  , regs :: M.Map Reg Int
  , code :: V.Vector Inst
  }

mkInitial = ProgState 0 M.empty

execute :: MonadIO m => Code -> m ()
execute = tryExecuteAll . mkInitial . V.fromList . toList . uncode

tryExecuteAll ps@(ProgState {..}) =
  case code V.!? ip of
    Just Halt -> pure ()
    Nothing -> pure ()
    Just inst -> do
      s <- newState ps inst
      tryExecuteAll s

newState ps@(ProgState {..}) = \case
  Addr a b o -> set o (get a + get b)
  Jmpn off t -> jumpIf (getR t /= 0) off
  Jmpa addr -> pure $ ps { ip = addr }
  Modr n d o -> set o (get n `mod` get d)
  Says t -> do
    liftIO $ putStr $ unpack t
    next ps
  Sayr r -> do
    liftIO $ putStr $ show $ getR r
    next ps
  Halt -> pure ps

  where
    getR = flip (M.findWithDefault 0) regs

    get (RVal r) = getR r
    get (NVal n) = n

    set r v = next $ ps { regs = M.insert r v regs }

    mut r f = set r $ f $ getR r

    next s = pure $ s { ip = ip + 1 }

    jumpIf c off =
      if c
      then pure $ ps { ip = ip + off }
      else next ps
