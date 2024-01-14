module Tarefa1 where
import DataStruct
import Maps

-- | Verifica colisões com paredes na direção especificada.
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

-- | Verifica colisões com o chão.
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

-- | Obtém o valor em uma posição específica no mapa do jogo.
obterValor :: [[Bloco]]-> (Int,Int) -> Bloco
obterValor [] _ = Vazio
obterValor (h:t) (x,y)        |y' == 0       = obterColuna h (x,y)
                              |otherwise     = obterValor t (x,y'-1)
        where y' = abs y

-- | Obtém o valor em uma coluna específica no mapa do jogo.
obterColuna :: [Bloco] -> (Int,Int) -> Bloco    
obterColuna (h:t) (x,y) |x==0       = h
                        |otherwise  = obterColuna t ((x-1),y)

-- | Verifica colisões entre dois personagens.
colisoesPersonagem :: [Personagem] -> Personagem -> Bool
colisoesPersonagem [] _ = True
colisoesPersonagem (Personagem {tamanho = (a1,b1), posicao = (x1,y1)}:t) mario@(Personagem {tamanho = (a2,b2), posicao = (x2,y2)}) = 
        not (x1 + (a1/2) < x2 - (a2/2)|| x2 + (a2/2) < x1 - (a1/2) || y1 + (b1/2) < y2 - (b2/2)|| y2 + (b2/2) < y1 - (a1/2)) && colisoesPersonagem t mario





