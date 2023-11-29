{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Ivo Filipe Mendes Vieira <a103999@alunos.uminho.pt>
              Filipa Cosquete Santos <a106837@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324
import Tarefa1 
import Tarefa2

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta = undefined


{-
1. Um inimigo perde 1 (uma) vida se estiver dentro da hitbox de dano de
um jogador armado. Por jogador armado entende-se um jogador cuja
componente aplicaDano esteja activa e com tempo restante. Note                fantasmahit, hitboxMartelo
que a hitbox de dano não é a mesma hitbox do jogador, mas antes uma       
hitbox com as dimensões do jogador posicionada exactamente à frente
do jogador, cf. figura 8.

2. Um inimigo morre quando as suas vidas chegam a 0 (zero.) Nessa               fantasmamorto
altura, deixa de ser representado no mapa. Note que um inimigo
(enquanto Personagem) não é removido da lista de inimigos de um
Jogo mesmo quando morre.

3. Efeito de gravidade: qualquer personagem que não esteja sobre uma
plataforma deverá “cair”. A velocidade da queda será dada no código
pela variável gravidade.

4. O jogador deverá perder 1 (uma) vida se for atingido por um inimigo.           jogadorhit
O inimigo não perde vidas nesta situação.

5. Ao recolher um coleccionável, este deverá desaparecer do mapa. No
caso do martelo, este deve armar o jogador durante 10 (dez) segundos
a contar do momento da sua recolha. No caso de uma moeda, esta            tiracole, apanhacole
deve aumentar a pontuação do jogador.

6. Um alçapão deverá desaparecer se o jogador o pisar. Por outro lado,
não sofrerá qualquer alteração se for um inimigo a pisá-lo.

7. Colisões: personagens não podem sair do mapa nem atravessar blo-
cos de plataforma. Mais ainda, deve também assumir que a hitbox
da estrela ou de um objecto coleccionável tem tamanho 1 × 1, i.e.
estrela/martelo/moeda ocupam um bloco da matriz na totalidade.
-}


fantasmahit :: [Hitbox] -> Hitbox -> Personagem          --recebe lista de inimigos e a hitbox do martelo
fantasmahit hitfant h 
    |overlap h hitfant = fantasma {vida= v-1}        --se a hitbox tocar nas hitboxes do fantasma perde uma vida
    |otherwise = fantasmahit hitfant h
        where 
            hitfant = map hitboxPersonagem fantasmas
            fantasmas = [fantasma]       --n sei bem se está bem configurado
            v= vida fantasma


hitboxMartelo :: Personagem -> Hitbox
hitboxMartelo mario@(Personagem {posicao = (x, y), direcao = d, aplicaDano = (True, t)}) = --n sei se é eficiente ter a hitboxmartelo separada do fantasmahit?
    let (w, h) = tamanho mario      --copiei o formato da tarefa 1
    in
        case d of
            Este -> ((x-w/2+1, y-h/2), (x+w/2+1, y+h/2)) --forma hitbox à direita do mario
            Oeste -> ((x-w/2-1, y-h/2), (x+w/2-1, y+h/2)) --forma hitbox à esquerda do mario


fantasmamorto :: [Personagem] -> [Personagem] -- suposto usar na lista q contem os inimigos no mapa apenas
fantasmamorto (x:xs)= if tipo x == Fantasma && vida x == 0  --verifica se é fantasma e se tem 0 vidas
                    then fantasmamorto xs --se sim, tira da lista
                    else x: fantasmamorto xs --se n, mantem

fantasmamortopontos ::[Personagem] -> Personagem-> Personagem  -- recebe pontos por matar um fantasma, n está especificado q temos q fzr isto mas ?
fantasmamortopontos (x:xs) mario@(Personagem{pontos = p})
    |tipo x == Fantasma && vida x == 0 = mario {pontos= p+500} -- mais 500 pontos 


jogadorhit ::[Personagem] -> Personagem -> Personagem --verifica se a colisao de personagens com o mario acontece
jogadorhit (x:xs) mario@(Personagem{vida = v})
    |colisoesPersonagens x mario = mario {vida= v-1} --se sim tira uma vida
    |otherwise = jogadorhit xs mario


gameover :: Personagem -> Bool --recebe vida e dá true ou false
gameover Personagem {vida=v} = v == 0 --e verifica se é 0. se sim, n tem mais vida


tiracole :: [(Colecionavel, Posicao)] -> Personagem -> [(Colecionavel, Posicao)]
tiracole [] mario = []
tiracole ((c,x):t) mario
    |x == posicao mario = tiracole t mario --remove do mapa os colecionaveis
    |otherwise= (c,x):t

apanhacole:: [(Colecionavel, Posicao)] -> Personagem -> Personagem
apanhacole [] mario = mario
apanhacole (((Moeda,x):t)) mario@(Personagem {pontos = p}) --verifica se a posicao da moeda é igual à do mario
    |x == posicao mario = mario {pontos= p + 800} --se sim recebe 800 pontos (verificar dps se é msm essa a quantidade de pontos recebida no jogo)
    |otherwise = apanhacole t mario -- se n continua a verificar 
apanhacole (((Martelo,x):t)) mario@(Personagem {aplicaDano=(b,d)})
    |x == posicao mario = mario{aplicaDano =(True,10)} --se a posicao do mario for igual à do martelo, o mario recebe a condição de dar dano
    |otherwise = apanhacole t mario --atenção o tempo n está definido ainda, só na tarefa 4




