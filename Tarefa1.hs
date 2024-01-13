module Tarefa1 where
import DataStruct
import Maps

colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede (Mapa _ _ mapa) Personagem {posicao = (x,y), direcao = dir} =
    case dir of 
        Oeste -> colisaoPlataforma (round ((x-16)/16),round (y/16))
        Leste -> colisaoPlataforma (round ((x+16)/16),round (y/16))
        _ ->  False
    where 
        colisaoPlataforma coord = 
            case obterValor mapa coord of
                Plataforma -> True 
                Alcapao -> True
                Vazio -> False
                Escada -> False

colisoesChao :: Mapa -> Personagem -> Bool
colisoesChao (Mapa _ _ mapa) Personagem {posicao = (x,y)} =
         colisaoPlataforma (round (x/16),round ((y+16)/16)) || colisaoPlataforma (round (x/16),round ((y-16)/16))
    where 
        colisaoPlataforma coord = 
            case obterValor mapa coord of
                Plataforma -> True 
                Alcapao -> True
                Vazio -> False
                Escada -> False

obterValor :: [[Bloco]]-> (Int,Int) -> Bloco
obterValor (h:t) (x,y)        | y' == 0      = obterColuna h (x,y)
                              | otherwise    = obterValor t (x,y'-1)
        where y' = abs y

obterColuna :: [Bloco] -> (Int,Int) -> Bloco
obterColuna (h:t) (x,y) |x == 0     = h
                        |otherwise  = obterColuna t (x-1 , y)

colisoesPersonagem :: Personagem -> Personagem -> Bool
colisoesPersonagem Personagem {tamanho = (a1,b1), posicao = (x1,y1)} Personagem {tamanho = (a2,b2), posicao = (x2,y2)} = 
        not (x1 + (a1/2) < x2 - (a2/2)|| x2 + (a2/2) < x1 - (a1/2) || y1 + (b1/2) < y2 - (b2/2)|| y2 + (b2/2) < y1 - (a1/2))




