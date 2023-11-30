module Main where

import Test.HUnit
import Tarefa1TestSpec

test1 = [test_hitboxPersonagem,test_colisoesParede, test_safeGet, test_colisoesPersonagens, test_overlap]

main :: IO ()
main = runTestTTAndExit $ test [test1]
