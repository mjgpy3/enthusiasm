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
    ) where

import Prelude hiding ((>>))

import Data.Text (Text)
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
  deriving Show

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
