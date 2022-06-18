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

    addi 1 C

    addr C H F
    jmpn 2 F
    halt

    seti 15 D
    modr C D F
    jmpn 3 F
    says "FizzBuzz\n"
    jmpa 1

    seti 3 D
    modr C D F
    jmpn 3 F
    says "Fizz\n"
    jmpa 1

    seti 5 D
    modr C D F
    jmpn 3 F
    says "Buzz\n"
    jmpa 1

    sayr C
    says "\n"
    jmpa 1
