{-|
Module : Tarefa1
Description : Modulo Haskell Gloss Tarefa1
Copyright : Hugo Soares (a107293)
            Henrique Brito (a107378)
Deteção de colisões.
O objectivo desta tarefa é implementar o par de funções
colisoesParede :: Mapa -> Personagem -> Bool
colisoesPersonagens :: Personagem -> Personagem -> Bool
-}
module Tarefa1 where

import DataStruct
import Maps

-- | Verifica colisões com paredes na direção especificada.
colisoesParede :: Mapa -> Personagem -> Direcao -> Bool
colisoesParede (Mapa _ _ mapa) Personagem {posicao = (x,y)} dir =
    case dir of 
        Oeste -> colisaoPlataforma (round ((x-8)/16),round (y/16))
        Leste -> colisaoPlataforma (round ((x+8)/16),round (y/16))
        Norte -> colisaoPlataforma (round (x/16),round ((y+16)/16))
        Sul   -> colisaoPlataforma (round (x/16),round ((y-20)/16))
    where 
        colisaoPlataforma coord = 
            case obterValor mapa coord of
                Plataforma -> True 
                Alcapao -> True
                Vazio -> False
                Escada -> False

-- | Obtém o valor em uma posição específica no mapa do jogo.
obterValor :: [[Bloco]]-> (Int,Int) -> Bloco
obterValor [] _ = Vazio
obterValor (h:t) (x,y)        |y' == 0       = obterColuna h (x,y)
                              |otherwise     = obterValor t (x,y'-1)
        where y' = abs y

-- | Obtém o valor em uma coluna específica no mapa do jogo.
obterColuna :: [Bloco] -> (Int,Int) -> Bloco    
obterColuna (h:t) (x,y) |x==0       = h
                        |otherwise  = obterColuna t (x-1,y)


colisoesPersonagens :: Personagem -> [Personagem] -> Bool
colisoesPersonagens _ [] = False
colisoesPersonagens mario inimigos@(ini:resto) = colisoesPersonagem mario ini || colisoesPersonagens mario resto

-- | Verifica colisões entre dois personagens.
colisoesPersonagem :: Personagem -> Personagem -> Bool
colisoesPersonagem (Personagem {tamanho = (a1,b1), posicao = (x1,y1)}) Personagem {tamanho = (a2,b2), posicao = (x2,y2)} =  
   not (x1 + (a1/2) < x2 - (a2/2)|| x2 + (a2/2) < x1 - (a1/2) || y1 + (b1/2) < y2 - (b2/2)|| y2 + (b2/2) < y1 - (a1/2)) 




