{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Enthusiasm

someFunc :: IO ()
someFunc =
  execute $ Enthusiasm.do
    seti (-101) H

    let loop = jmpa 1
    addi 1 C

    addr c h F
    jmpn 2 F
    halt

    let
      sayDivides message divisor = Enthusiasm.do
        seti divisor D
        modr c d F
        jmpn 3 F
        says $ message <> "\n"
        loop

    "FizzBuzz" `sayDivides` 15
    "Fizz" `sayDivides` 3
    "Buzz" `sayDivides` 5

    sayr C
    says "\n"
    loop
