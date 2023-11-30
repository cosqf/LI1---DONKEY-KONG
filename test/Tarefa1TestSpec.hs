module Tarefa1TestSpec where
import LI12324
import Tarefa1
import Test.HUnit

test_hitboxPersonagem = test [
    "Teste 1" ~: ((1.0,1.0),(2.0,2.0)) ~=? hitboxPersonagem Personagem { velocidade = (0, 0), tipo = Jogador, posicao = (1.5, 1.5), direcao = Norte, tamanho = (1, 1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False, 0) },
    "Teste 2" ~: ((1.5,2.5),(2.5,3.5)) ~=? hitboxPersonagem Personagem { velocidade = (0, 0), tipo = Fantasma, posicao = (2.0, 3.0), direcao = Norte, tamanho = (1, 1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False, 0) }
    ]

test_colisoesParede = test [
    "Teste 1" ~: ~=? colisoesParede
    ,"Teste 2" ~: ~=? colisoesParede
    ,"Teste 3" ~: ~=? colisoesParede
    ]

test_safeGet = test [
    "Teste 1" ~: ~=? safeGet
    ,"Teste 2" ~: ~=? safeGet
    ,"Teste 3" ~: ~=? safeGet
    ]


test_colisoesPersonagens = test [
    "Teste 1" ~: ~=? colisoesPersonagens
    ,"Teste 2" ~: ~=? colisoesPersonagens
    ,"Teste 3" ~: ~=? colisoesPersonagens
    ]


test_overlap = test [
    "Teste 1" ~: ~=? overlap
    ,"Teste 2" ~: ~=? overlap
    ,"Teste 3" ~: ~=? overlap
    ]

