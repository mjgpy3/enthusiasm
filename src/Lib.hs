{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Enthusiasm

someFunc :: IO ()
someFunc =
  print $ Enthusiasm.do
    seti (-101) H

    let loop = jmpa 1
    addi 1 C

    addr C H F
    jmpn 2 F
    halt

    let
      sayDivides message divisor = Enthusiasm.do
        seti divisor D
        modr C D F
        jmpn 3 F
        says $ message <> "\n"
        loop

    "FizzBuzz" `sayDivides` 15
    "Fizz" `sayDivides` 3
    "Buzz" `sayDivides` 5

    sayr C
    says "\n"
    loop
