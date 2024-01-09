module Main where

import Test.HUnit
import Tarefa1TestSpec
import Tarefa2TestSpec
import Tarefa3TestSpec
import Tarefa4TestSpec


main :: IO ()
main = runTestTTAndExit $ test [testesTarefa1, testesTarefa2, testesTarefa3, testesTarefa4]

