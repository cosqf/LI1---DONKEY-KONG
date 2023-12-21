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
import Data.List (elemIndices)

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta semente tempo Jogo {mapa= m, inimigos= i, colecionaveis = c, jogador= j } =
    let
        iniatualizado = flip fantasmahit j . multf (flip colisao m) . multf (queda m (0,10))  $ i
        marioatualizado = fantasmamortopontos i . jogadorhit i . flip removeMartelo  tempo. queda m (0,10) . flip colisao m . apanhacole c . flip removeMartelo tempo $ j
        colecatualizado = tiracole c j
        mapaatualizado = removeAlcapao j m
    in
        Jogo
        {
            mapa = mapaatualizado,
            inimigos = iniatualizado,
            colecionaveis = colecatualizado,
            jogador = marioatualizado
        }

fantasmahit :: [Personagem] -> Personagem -> [Personagem]          --recebe lista de inimigos e mario
fantasmahit (f@(Personagem {tipo=Fantasma, vida= v}):fs) h
    |overlap (hitboxMartelo h) (hitboxPersonagem f) = f {vida= v-1} : fantasmahit fs h      --se a hitbox tocar nas hitboxes do fantasma perde uma vida
    |otherwise= fantasmahit fs h


hitboxMartelo :: Personagem -> Hitbox
hitboxMartelo mario@(Personagem {posicao = (x, y), direcao = d, aplicaDano = (True, t)}) = --n sei se é eficiente ter a hitboxmartelo separada do fantasmahit?
    let (w, h) = tamanho mario      --copiei o formato da tarefa 1
    in
        case d of
            Este -> ((x-w/2+1, y-h/2), (x+w/2+1, y+h/2)) --forma hitbox à direita do mario
            Oeste -> ((x-w/2-1, y-h/2), (x+w/2-1, y+h/2)) --forma hitbox à esquerda do mario

{-
fantasmamorto :: [Personagem] -> [Personagem] -- suposto usar na lista q contem os inimigos no mapa apenas
fantasmamorto (x:xs)= if tipo x == Fantasma && vida x == 0  --verifica se é fantasma e se tem 0 vidas
                    then fantasmamorto xs --se sim, tira da lista
                    else x: fantasmamorto xs --se n, mantem
-}
fantasmamortopontos ::[Personagem] -> Personagem-> Personagem  -- recebe pontos por matar um fantasma, n está especificado q temos q fzr isto mas ?
fantasmamortopontos (f@(Personagem {tipo=Fantasma, vida= v}):fs) mario@(Personagem{pontos = p})
    |overlap (hitboxMartelo mario) (hitboxPersonagem f)= mario {pontos= p+500} -- mais 500 pontos
    |otherwise= fantasmamortopontos fs mario


jogadorhit ::[Personagem] -> Personagem -> Personagem --verifica se a colisao de personagens com o mario acontece
jogadorhit (x@(Personagem {vida=vf}):xs) mario@(Personagem{vida = v})
    |vf == 0 = jogadorhit xs mario
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
    |otherwise = apanhacole t mario --atenção o tempo n está definido ainda

removeMartelo :: Personagem -> Tempo -> Personagem --recebe o tempo e verifica se o tempo do martelo está a zero
removeMartelo mario@(Personagem {aplicaDano = (False,0)}) t = mario
removeMartelo mario@(Personagem {aplicaDano = (True , m)}) t
    |t - m == t = mario {aplicaDano = (False, 0)}
    |otherwise= mario {aplicaDano = (True, m-1)}

queda :: Mapa -> Velocidade -> Personagem-> Personagem         -- velocidade fica igual à gravidade e a direção a sul
queda mapa gravidade p
    |blocodirecao p Sul mapa == Vazio = p {velocidade= gravidade, direcao = Sul}
    |otherwise = p

removeAlcapao :: Personagem -> Mapa -> Mapa
removeAlcapao mario@(Personagem {posicao= (x,y)}) mapa@(Mapa a1 a2 l)
    |blocodirecao mario Sul mapa == Alcapao = Mapa a1 a2 (troca (x,y+1) Vazio mapa)
    |elem (x,y-2) (posb mapa Alcapao) = Mapa a1 a2 (troca (x,y+2) Vazio mapa)
    where
        troca :: Posicao -> Bloco -> Mapa -> [[Bloco]]
        troca (x,1) b (Mapa a1 a2 (h:t)) = auxTroca (x,1) b h : t
        troca (x,y) b (Mapa a1 a2 l) = head l : troca (x,y-1) b (Mapa a1 a2 (tail l))
            
        auxTroca ::  Posicao -> Bloco -> [Bloco] -> [Bloco]
        auxTroca (1,1) b h = b:tail h
        auxTroca (x,1) b (h:t) = h: auxTroca (x-1,1) b t

colisao :: Personagem -> Mapa -> Personagem
colisao p m
    |colisoesParede m p =  p{velocidade=(0,0)}
    |otherwise = p



{-
colisao :: [Personagem] -> Mapa -> [Personagem]
colisao (p@(Personagem {posicao = (x,y)}) :ps) mapa@(Mapa _ _ l)
    |velocidade p /=(0,0) && direcao p == Este && blocodirecao p Este mapa == Plataforma || length (head l) == floor x = p {velocidade= (0,0)} : colisao ps mapa
    |velocidade p /=(0,0) && direcao p == Oeste && blocodirecao p Oeste mapa == Plataforma || x== 0 = p {velocidade= (0,0)} : colisao ps mapa
    |otherwise = colisao ps mapa
-}


-- funções uteis 

multf :: (a -> a) -> [a] -> [a]   -- esta função aplica outras funções a listas de elementos ao inves de apenas um
multf _ []     = []
multf f (x:xs) = f x : multf f xs


blocodirecao :: Personagem -> Direcao -> Mapa -> Bloco     --indica o bloco posicionado no lado norte/sul/etc do personagem
blocodirecao (Personagem {posicao= (x,y)}) Norte mapa = blocopos (x,y-1) mapa
blocodirecao (Personagem {posicao= (x,y)}) Sul mapa = blocopos (x,y+1) mapa
blocodirecao (Personagem {posicao= (x,y)}) Oeste mapa = blocopos (x-1,y) mapa
blocodirecao (Personagem {posicao= (x,y)}) Este mapa = blocopos (x+1,y) mapa

blocopos :: Posicao -> Mapa -> Bloco    -- indica o bloco na coordenada (x,y)
blocopos (x, y) (Mapa _ _ mapa)
    |y<0 || y>fromIntegral (length mapa)=Vazio      -- se estiver fora do mapa dá vazio
    |x<0 || x>fromIntegral (length (head mapa))=Vazio
    |x<1 && y<1 = (mapa !! floor y) !! floor x
    |x<1 = (mapa !! floor (y-1)) !! floor x
    |y<1 = (mapa !! floor y) !! floor (x-1)
    | otherwise = (mapa !! floor (y-1)) !! floor (x-1)

posb :: Mapa -> Bloco -> [Posicao] --calcula todas as posicoes de um bloco
posb (Mapa _ _ l) b = aux (map (elemIndices b) l) 1.0
  where
    aux :: [[Int]] -> Double -> [Posicao]
    aux [] _ = []
    aux ([]:c) x = aux c (x + 1)
    aux ((a:b):c) x = (x, fromIntegral a) : aux (b:c) x



{-
1. Um inimigo perde 1 (uma) vida se estiver dentro da hitbox de dano de
um jogador armado. Por jogador armado entende-se um jogador cuja
componente aplicaDano esteja activa e com tempo restante. Note                fantasmahit, hitboxMartelo
que a hitbox de dano não é a mesma hitbox do jogador, mas antes uma       
hitbox com as dimensões do jogador posicionada exactamente à frente
do jogador, cf. figura 8.

2. Um inimigo morre quando as suas vidas chegam a 0 (zero.) Nessa         fantasmamorto (!!!!)
altura, deixa de ser representado no mapa. Note que um inimigo
(enquanto Personagem) não é removido da lista de inimigos de um
Jogo mesmo quando morre.

3. Efeito de gravidade: qualquer personagem que não esteja sobre uma               queda
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
da estrela ou de um objecto coleccionável tem tamanho 1 × 1, i.e.                colisao
estrela/martelo/moeda ocupam um bloco da matriz na totalidade.
-}