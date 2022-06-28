{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Enthusiasm

someFunc :: IO ()
someFunc =
  execute $ Enthusiasm.do
    let loop = jmpa 0
    addi 1 C

    addr c (-101) F
    jmpn 2 F
    halt

    let
      sayDivides message divisor = Enthusiasm.do
        modr c divisor F
        jmpn 3 F
        says $ message <> "\n"
        loop

    "FizzBuzz" `sayDivides` 15
    "Fizz" `sayDivides` 3
    "Buzz" `sayDivides` 5

    sayr C
    says "\n"
    loop
