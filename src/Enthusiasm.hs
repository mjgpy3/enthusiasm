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
    ) where

import Prelude hiding ((>>))

import Control.Monad.IO.Class
import Data.Text (Text, unpack)
import qualified Data.Map.Strict as M

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

data Inst
  = Seti Int Reg
  | Addi Int Reg
  | Addr Reg Reg Reg
  | Jmpn Int Reg
  | Jmpa Int
  | Modr Reg Reg Reg
  | Says Text
  | Sayr Reg
  | Halt
  deriving Show

newtype Code = Code { uncode :: M.Map Int Inst }
  deriving Show

(>>) :: Code -> Code -> Code
Code left >> Code right =
  case M.lookupMax left of
    Just (n, _) -> Code $ M.union left $ M.mapKeys (+ (n + 1)) right
    Nothing -> error "This should be impossible, famous last words, haha hee"

one inst a = Code $ M.singleton 0 $ inst a
two inst a b = Code $ M.singleton 0 $ inst a b
three inst a b c = Code $ M.singleton 0 $ inst a b c

seti = two Seti
addi = two Addi
addr = three Addr
jmpn = two Jmpn
jmpa = one Jmpa
modr = three Modr
says = one Says
sayr = one Sayr
halt = Code $ M.singleton 0 Halt

data ProgState
  = ProgState {
    ip :: Int
  , regs :: M.Map Reg Int
  , code :: Code
  }

mkInitial = ProgState 0 M.empty

execute :: MonadIO m => Code -> m ()
execute = tryExecuteAll . mkInitial

tryExecuteAll ps@(ProgState {..}) =
  case M.lookup ip $ uncode code of
    Just Halt -> pure ()
    Nothing -> pure ()
    Just inst -> do
      s <- newState ps inst
      tryExecuteAll s

newState ps@(ProgState {..}) = \case
  Seti i o -> set o i
  Addi i o -> mut o (+ i)
  Addr a b o -> set o (get a + get b)
  Jmpn off t -> jumpIf (get t /= 0) off
  Jmpa addr -> pure $ ps { ip = addr }
  Modr n d o -> set o (get n `mod` get d)
  Says t -> do
    liftIO $ putStr $ unpack t
    next ps
  Sayr r -> do
    liftIO $ putStr $ show $ get r
    next ps
  Halt -> pure ps

  where
    get = flip (M.findWithDefault 0) regs

    set r v = next $ ps { regs = M.insert r v regs }

    mut r f = set r $ f $ get r

    next s = pure $ s { ip = ip + 1 }

    jumpIf c off =
      if c
      then pure $ ps { ip = ip + off }
      else next ps
