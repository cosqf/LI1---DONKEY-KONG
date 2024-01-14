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
import Funcoes

{-|atualiza o estado do jogo, de acordo com o tempo especificado, atualiza a posição dos personagens,dos colecionaveis, tal como os pontos de vida dos personagens-}
movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta semente tempo Jogo {mapa= m@(Mapa (posi,_) _ blocos), inimigos= i, colecionaveis = c, jogador= j } =
    let
        iniatualizado = map (velocidades . (fantEscCheck m) . (flip colisao m) . (queda m (0,10)) ) . flip fantasmahit j . map dkparado $ i
        marioatualizado = marioEscCheck m . velocidades . fantasmamortopontos i . jogadorhit i posi . removeMartelo . queda m (0,10) . apanhacole c . flip colisao m $ j
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


-- | Certifica-se que o Donkey Kong está parado.
dkparado :: Personagem -> Personagem
dkparado p@(Personagem {tipo= MacacoMalvado}) = p {velocidade = (0,0)}
dkparado p = p

{-|verifica se um fantasma colide com um martelo, se a hitbox do martelo estiver sobreposta à hitboxdo fantasma, a vida do fantasma é atualizada e a lista é atualizada, caso não aconteça a lista fica igual-}
fantasmahit :: [Personagem] -> Personagem -> [Personagem]          -- |recebe lista de inimigos e mario
fantasmahit [] _ = []
fantasmahit (f@(Personagem {tipo=Fantasma, vida= v}):fs) h
    |overlap' (hitboxMartelo h) (hitboxPersonagem f) = f {vida= v-1} : fantasmahit fs h      -- |se a hitbox tocar nas hitboxes do fantasma perde uma vida
    |otherwise= f: fantasmahit fs h
fantasmahit p ps = p

{-|calcula a hitbox do martelo do jogador,Se o personagem estiver virado para a direita, a função retorna um retângulo que se estende uma unidade para a direita do personagem.
Se o personagem estiver virado para a esquerda, a função retorna um retângulo que se estende uma unidade para a esquerda do personagem-}
hitboxMartelo :: Personagem -> Maybe Hitbox
hitboxMartelo mario@(Personagem {posicao = (x, y), direcao = d,tamanho= (xt,yt),emEscada=False, aplicaDano = (True, t)}) =
    let (w, h) = tamanho mario      --copiei o formato da tarefa 1
    in
        case d of
            Este -> Just (((x-w/2)+xt, y-h/2), ((x+w/2)+xt, y+h/2)) -- |forma hitbox à direita do mario
            Oeste -> Just (((x-w/2)-xt, y-h/2), ((x+w/2)-xt, y+h/2)) -- |forma hitbox à esquerda do mario
            _ ->  Nothing
hitboxMartelo _ = Nothing

fantasmamortopontos ::[Personagem] -> Personagem-> Personagem
fantasmamortopontos [] m = m
fantasmamortopontos (f@(Personagem {tipo=Fantasma, vida= v}):fs) mario@(Personagem{pontos = p})
    |overlap' (hitboxMartelo mario) (hitboxPersonagem f)= mario {pontos= p+500} -- | mais 500 pontos
    |otherwise= fantasmamortopontos fs mario
fantasmamortopontos _ p = p

{-|verifica se um personagem colide com o jogador,se um inimigo colidir com o jogador, o jogador perde uma vida-}

jogadorhit ::[Personagem]-> Posicao -> Personagem -> Personagem -- |verifica se a colisao de personagens com o mario acontece
jogadorhit [] _ m = m
jogadorhit (x@(Personagem {vida=vf}):xs) posi mario@(Personagem{vida = v})
    |vf == 0 = jogadorhit xs posi mario
    |colisoesPersonagens x mario = mario {vida= v-1, posicao = posi} -- |se sim tira uma vida
    |otherwise = jogadorhit xs posi mario


{-
jogadorhit :: Double -> [Personagem] -> Personagem -> Personagem -- |verifica se a colisao de personagens com o mario acontece
jogadorhit _ [] m = m
jogadorhit cool (x@(Personagem {vida=vf}):xs) mario@(Personagem{vida = v})
    |vf <= 0 = jogadorhit cool xs mario
    |cool== 0 && colisoesPersonagens x mario = mario {vida= v-1} -- |se sim tira uma vida
    |otherwise = jogadorhit cool xs mario

cooldownVida :: Tempo -> Double -> [Personagem] -> Personagem -> Double
cooldownVida _ t [] _ = t
cooldownVida temp t l@(x@(Personagem {vida = vf}) : xs) mario@(Personagem {vida = v})
    | vf <= 0 = t
    | colisoesPersonagens x mario = cooldownVida temp (temp+1) l mario  
    | temp <= t = t - (1.0 / 50.0)
    | otherwise = t
-}
{-|remove colecionáveis do mapa se o jogador colidir com eles-}
tiracole :: [(Colecionavel, Posicao)] -> Personagem -> [(Colecionavel, Posicao)]
tiracole [] mario = []
tiracole ((c,x):t) mario
    |colisoesHitB x (posicao mario) = tiracole t mario -- |remove do mapa os colecionaveis
    |otherwise= (c,x) : tiracole t mario

{-|coleta coleccionável e atribui recompensas-}
apanhacole:: [(Colecionavel, Posicao)] -> Personagem -> Personagem
apanhacole [] mario = mario
apanhacole (((Moeda,x):t)) mario@(Personagem {pontos = p}) -- |verifica se a posicao da moeda é igual à do mario
    |colisoesHitB x (posicao mario) = mario {pontos= p + 500} -- |se sim recebe 500 pontos 
    |otherwise = apanhacole t mario -- |se n continua a verificar 
apanhacole (((Martelo,x):t)) mario@(Personagem {aplicaDano=(b,d)})
    |colisoesHitB x (posicao mario) = mario{aplicaDano =(True,10)} -- |se a posicao do mario for igual à do martelo, o mario recebe a condição de dar dano
    |otherwise = apanhacole t mario

{-|remove o martelo do jogador se o tempo de duração do martelo acabar-}
removeMartelo :: Personagem -> Personagem 
removeMartelo mario@(Personagem {aplicaDano = (False,0)}) = mario
removeMartelo mario@(Personagem {aplicaDano = (True , m)})
    |m <= 0 = mario {aplicaDano = (False, 0)}
    |otherwise= mario{aplicaDano = (True , m-(1/25))} -- framerate de 60s: 1/60


-- | atualiza a velocidade e a direção do jogador para que ele caia
queda :: Mapa -> Velocidade -> Personagem-> Personagem         -- | velocidade fica igual à gravidade e a direção a sul
queda mapa gravidade p@(Personagem {posicao= (x,y), tamanho = (tx,ty)})
    |fst (velocidade p) == 50 = p
    |blocopos (x,y+ty/2) mapa == Vazio = p {velocidade= gravidade, direcao = Sul, emEscada= False}
    |otherwise = p

{-|remove um alçapão do mapa se o jogador estiver sobre ele e esse bloco é preenchido com um bloco vazio-}
removeAlcapao :: Personagem -> Mapa -> Mapa
removeAlcapao mario@(Personagem {posicao= (x,y), velocidade= (vx,vy)}) mapa@(Mapa a1 a2 l)
    |blocodirecao mario Sul mapa == Alcapao = Mapa a1 a2 (troca (fromIntegral (ceiling x),fromIntegral (ceiling y+1)) Vazio mapa)
    |vy/=0 && elem (x,y+2) (posb mapa Alcapao) = Mapa a1 a2 (troca (x,y+2) Vazio mapa)
    |otherwise = mapa
        where
            troca :: Posicao -> Bloco -> Mapa -> [[Bloco]]
            troca _ _ (Mapa _ _ []) = []
            troca (x,1) b (Mapa a1 a2 (h:t)) = auxTroca (x,1) b h : t
            troca (x,y) b (Mapa a1 a2 l) = head l : troca (x,y-1) b (Mapa a1 a2 (tail l))

            auxTroca ::  Posicao -> Bloco -> [Bloco] -> [Bloco]
            auxTroca _ _ [] = []
            auxTroca (1,1) b h = b:tail h
            auxTroca (x,1) b (h:t) = h: auxTroca (x-1,1) b t

-- |verifica se um personagem colide com uma parede no mapa
colisao :: Personagem -> Mapa -> Personagem
colisao p@(Personagem {posicao= (x,y), direcao= d}) m@(Mapa _ _ blocos)
    |emEscada p = p
    |blocopos (x+0.5,y) m == Plataforma && d == Este = p{velocidade=(0,0)}
    |blocopos (x-0.5,y) m == Plataforma && d == Oeste = p{velocidade=(0,0)}
    |blocodirecao p Sul m == Plataforma && snd (velocidade p)>0 = p{velocidade=(0,0)}
    |x<=0.3 && d == Oeste= p{velocidade = (0,0)}
    |x>= mapWidth -0.3 && d == Este = p{velocidade = (0,0)}
    |otherwise = p
        where
            mapWidth= fromIntegral $ length (head blocos)

{-
colisao :: Personagem -> Mapa -> Personagem
colisao p@(Personagem {posicao= (x,y), direcao= d}) m
    |emEscada p = p
    |colisoesParede m p{posicao = (x+0.5,y)} && d == Este
    || colisoesParede m p{posicao = (x-0.5,y)} && d == Oeste 
    ||blocodirecao p Sul m == Plataforma && snd (velocidade p)>0
    ||x>= realToFrac comp && d == Este|| x<=0 && d== Oeste||y<=0 && d == Norte || y>= realToFrac alt && d == Sul  =  p{velocidade=(0,0)}
    |otherwise = p
        where
            (comp , alt) = tamanhoCompMapa m 
-}

-- |atualiza a posição de um personagem de acordo com sua velocidade
velocidades :: Personagem -> Personagem -- | relacionar a velocidade com a posicao
velocidades p@Personagem{velocidade=(0,0), posicao=(x,y)} = p
velocidades p@Personagem{velocidade=(vx, 0), posicao=(x,y), tipo = Jogador} =
    p { posicao = (x + (vx / 100), y)}
velocidades p@Personagem{velocidade=(0,vy), posicao=(x,y), tipo = Jogador} =
    p { posicao = (x, y + (vy / 100))}
velocidades p@Personagem{velocidade=(vx,vy), posicao=(x,y), tipo = Jogador, direcao= dir}
    |dir == Este =p { posicao = (x + (vx / 30), y + (vy / 50)), velocidade = (0,10), direcao = dir}
    |otherwise = p { posicao = (x + (vx / 30), y + (vy / 50)), velocidade = (0,10), direcao = dir}
velocidades p@Personagem{velocidade=(vx, 0), posicao=(x,y)} =
    p { posicao = (x + (vx / 200), y)}
velocidades p@Personagem{velocidade=(0,vy), posicao=(x,y)} =
    p { posicao = (x, y + (vy / 200))}
velocidades p@Personagem{velocidade=(vx,vy), posicao=(x,y)} =
    p { posicao = (x + (vx / 200), y + (vy / 200))}


marioEscCheck :: Mapa -> Personagem -> Personagem
marioEscCheck mapa p@(Personagem {posicao= (x,y), velocidade = (vx,vy), emEscada = esc, tamanho= (tx,ty)})
    |esc && vy>0 && blocopos (x,y+ty/2+1) mapa == Vazio = p {velocidade = (0,0)}
    |otherwise = p

fantEscCheck :: Mapa -> Personagem -> Personagem
fantEscCheck mapa p@(Personagem {posicao= (x,y), velocidade = (vx,vy), emEscada = esc, tamanho= (tx,ty)})
    |esc && (blocopos (x,y) mapa == Escada || blocopos (x,y) mapa == Plataforma ) && vy<=0 = p {velocidade = (0,vy), emEscada = True}
    |esc && blocopos (x,y+ty/2) mapa /= Plataforma = p {velocidade = (0,vy)} 
    |esc && blocopos (x,y+ty/2) mapa == Vazio = p {emEscada= False, velocidade = (vx,0)}
    |esc = p {velocidade = (0,vy)}
    |vy/=0 && blocopos (x,y+2) mapa == Escada = p {emEscada = True, velocidade = (vx,0)}
    |otherwise = p


-- | Arredonda a posição do y dos personagens
roundPosicao ::  Mapa -> Personagem -> Personagem
roundPosicao mapa p@Personagem { posicao = (x, y) }
    | not (emEscada p) && blocopos (x,y+0.5) mapa /= Vazio = p { posicao = (x, yr) }
    | otherwise = p
    where
        roundCoordinate :: Double -> Double
        roundCoordinate c =
            if abs (c - fromIntegral (round c)) <= 0.3
                then fromIntegral (round c)
                else fromIntegral (round (c / 0.5)) * 0.5
        yr = roundCoordinate y


overlap' :: Maybe Hitbox -> Hitbox -> Bool -- | mesmo que overlap mas aceita Maybe Hitbox
overlap' Nothing _ = False
overlap' (Just ((x1, y1), (x2, y2))) ((x3, y3), (x4, y4)) = x1 <= x4 && x2 >= x3 && y1 <= y4 && y2 >= y3

-- | função auxiliar que verifica se duas hitboxes estão a colidir
colisoesHitB :: Posicao -> Posicao -> Bool
colisoesHitB (x,y) (z,w)=
  let hitboxP1 = ((x - 1/2, y - 1/2), (x + 1/2, y + 1/2))
      hitboxP2 = ((z - 1/2, w - 1/2), (z + 1/2, w + 1/2))
  in
    overlap hitboxP1 hitboxP2


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

