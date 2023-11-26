{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Ivo Filipe Mendes Vieira <a103999@alunos.uminho.pt>
              Filipa Cosquete Santos <a106837@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324

valida :: Jogo -> Bool
valida = undefined

{-
1. O mapa tem “chão”, i.e. uma plataforma que impede que o jogador
ou outro personagem caia fora do mapa.

2. Todos os inimigos têm a propriedade ressalta a True, enquanto que 
o jogador a tem a False.

3. A posição inicial de um jogador não pode colidir com a posição inicial
de um outro personagem. Note que as posições iniciais de inimigos
podem colidir entre estes.

4. Número mı́nimo de inimigos: 2 (dois.)

5. Inimigos Fantasma têm exactamente 1 (uma) vida

6. Escadas não podem começar/terminar em alçapões, e pelo menos uma
das suas extremidades tem que ser do tipo Plataforma.

7. Alçapões não podem ser menos largos que o jogador.

8. Não podem existir personagens nem coleccionáveis “dentro” de plata-
formas ou alçapões, i.e. o bloco (na matriz do mapa) correspendente
à posição de um personagem ou objecto tem que ser Vazio.
-}

--exemplo
--mario = Personagem (5,5) Jogador (0,0) Este (1,1) False False 3 500 (False,0)

fantasma = Personagem {tipo= Fantasma, tamanho= (1,1), ressalta = True, vida= 1, aplicaDano = (False,0)} -- tamanho dos fantasmas = tamanho mario = (1,1)?
dk = Personagem {tipo= MacacoMalvado,tamanho= (2,2), ressalta = True, vida = 8, aplicaDano = (False,0)} --vida 8 pq há 8 alçapões, tamanho (2,2)?
mario = Personagem {tipo = Jogador,tamanho= (1,1), ressalta = False, vida = 3}

