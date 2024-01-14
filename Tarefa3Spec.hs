module Tarefa3Spec (testesTarefa3) where

import Tarefa1
import Tarefa3
import Test.HUnit
import DataStruct
import Maps
import Main

blocos1 :: [[Bloco]]
blocos1 = [ [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Alcapao, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]

mapaGrande :: Mapa
mapaGrande = Mapa ((8.5, 6.5), Leste) (5, 1.5) blocos1

pl1 = Personagem (0.0,0.0) Jogador (8.5,7) Oeste (0.8,0.8) False False 10 0 (True, 10.0)

en1 = Personagem (0.0,0.0) Fantasma (8,7) Lest (0.8,0.8) False True 10 0 (False, 0.0)
en2 = Personagem (0.0,0.0) Fantasma (8.7,7) Leste (0.8,0.8) False True 10 0 (False, 0.0)

c1 = (Martelo, (5,1))

j1 = Jogo mapaGrande [en1,en2] [c1] pl1

teste1A = "T1A: Inimigo 1 perde vida." ~: True ~=? (vida . head . inimigos $ movimenta 100 1.0 j1) < 10
teste1B = "T1B: Jogador perde vida." ~: True ~=? (vida . jogador $ movimenta 100 1.0 j1) < 10
teste1C = "T1C: Inimigo 2 não perde vida." ~: True ~=? (vida . last . inimigos $ movimenta 100 1.0 j1) == 10

pl2 = Personagem (0.0,0.0) Jogador (5.2,1) Oeste (0.8,0.8) False False 10 0 (False, 0.0)

j3 = Jogo mapaGrande [] [c1] pl2

j4 = Jogo mapaGrande [] [] (pl2 {aplicaDano = (True, 10.0)})

teste2A = "T2A: Jogador apanha martelo e a flag fica True." ~: True ~=? (fst . aplicaDano . jogador $ movimenta 100 1.0 j3)
teste2B = "T2B: Jogador apanha martelo e o tempo restante é maior que zero." ~: True ~=? (snd . aplicaDano . jogador $ movimenta 100 1.0 j3) > 0

pl3 = Personagem (0.0,0.0) Jogador (3.5,4) Oeste (0.8,0.8) True False 10 0 (False, 0.0)

j5 = Jogo mapaGrande [] [] pl3

teste3 = "T3: Jogador não cai quando esta na escada." ~: j5 ~=? movimenta 100 1.0 j5

pl4 = Personagem (-1.0,0.0) Jogador (0.5,10.5) Oeste (1,1) False False 10 0 (False, 0.0)

j6 = Jogo mapaGrande [] [] pl4

teste4 = "T4: Jogador não atravessa o limite do mapa." ~: False ~=? (fst . posicao . jogador $ movimenta 100 1.0 j6) < 0.0

pl5 = Personagem (0.0,0.0) Jogador (5,7.6) Oeste (1,1) False False 10 0 (False, 0.0)
en3 = Personagem (0.0,0.0) Fantasma (2.5,7.6) Este (1,1) False True 10 0 (False, 0.0)

j7 = Jogo mapaGrande [en3] [] pl5

blocos2 :: [[Bloco]]
blocos2 = [ [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Alcapao, Plataforma, Plataforma, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]

gameMap2 :: Mapa
gameMap2 = Mapa ((8.5, 6.5), Este) (5, 1.5) blocos2

teste5 = "T5: Alcapao e removido por jogador mas nao pelo inimigo." ~: gameMap2 ~=? mapa (movimenta 100 1.0 j7)

pl6 = Personagem (0.0,0.0) Jogador (5,1) Oeste (1,1) False False 10 0 (False, 0.0)
c2 = (Moeda, (5,1))

j8 = Jogo mapaGrande [] [c2] pl6

teste6 = "T6: Jogador apanha uma moeda" ~: True ~=? (pontos . jogador $ movimenta 100 1.0 j8) > (pontos . jogador $ j8)

testesTarefa3 = test [teste1A, teste1B, teste1C, teste2A, teste2B, teste3, teste4, teste5, teste6]
