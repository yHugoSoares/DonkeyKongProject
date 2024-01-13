module DataStruct where

--Datas do Jogo
data Mapa = Mapa (Posicao, Direcao) Posicao [[Bloco]]
data Bloco = Escada | Plataforma | Alcapao | Vazio deriving (Show,Eq,Read)
data Direcao = Norte | Sul | Leste | Oeste deriving (Show,Eq,Ord)
type Posicao = (Float,Float)            --par ordenado que define (coluna,linha)
data Personagem = Personagem{   
velocidade :: Velocidade                --Um vetor de velocidade normal
, tipo :: Entidade                      --Indica se é um inimigo (e qual) ou o jogador
, posicao :: Posicao                    --Coordenadas atuais
, direcao :: Direcao                    --Para qual posiçao (N,S,L,O) está virado
, tamanho :: (Double, Double)           --Tamanho do personagem descrito por um rectângulo largura X altura
, emEscada :: Bool                      --Indica se o personagem se encontra a subir/descer uma escada
, ressalta :: Bool                      --Indica se o personagem deve trocar de direçao ao colidir com uma parede ou ao chegar no final de uma plataforma
, vida :: Int                           --Quantas vidas faltam
, pontos :: Int                         --Pontuaçao acumulada
, aplicaDano :: (Bool, Double)          --Indica se tem o martelo armado e por quanto tempo ainda. Quando ativo, cria uma hitbox à frente do jogador (com as dimensoes do jogador) e qualquer inimigo que encoste sofre dano
}

type Velocidade = Float
data Entidade = Jogador | MacacoMalvado | Fantasma deriving (Show,Eq,Read)
data Colecionavel = Martelo | Moeda deriving (Show,Eq,Read)

data Jogo = Jogo 
    { mapa :: Mapa
    , inimigos :: [Personagem]
    , colecionaveis :: [(Colecionavel, Posicao)]
    , jogador :: Personagem
    } 

mov :: Float
mov = 2  -- Velocidade do movimento do Mário
jump :: Float
jump = 2 -- Valor de salto do Mário


