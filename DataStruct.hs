-- | Módulo que contém as estruturas de dados principais do jogo.
module DataStruct where

-- |Datas do Jogo

-- | Tipo de dado que representa o mapa do jogo.
data Mapa = Mapa (Posicao, Direcao) Posicao [[Bloco]]

-- | Tipo de dado que representa diferentes tipos de blocos no jogo.
data Bloco = Escada | Plataforma | Alcapao | Vazio deriving (Show,Eq,Read)

-- | Tipo de dado que representa direções cardeais.
data Direcao = Norte | Sul | Leste | Oeste deriving (Show,Eq,Ord)


-- | Tipo de dado que representa opções de menu.
data Opcao = Jogar | Sair deriving (Show,Eq,Ord)

-- | Tipo de dado que representa os estados do menu do jogo.
data Menu = Opcoes Opcao | ModoJogo | VenceuJogo deriving (Show,Eq,Ord)

-- | Alias de tipo representando uma posição 2D (coluna, linha).
type Posicao = (Float,Float)            --par ordenado que define (coluna,linha)

-- | Tipo de dado que representa personagens do jogo.
data Personagem = Personagem{   
velocidade :: Velocidade                --Um vetor de velocidade normal
, tipo :: Entidade                      --Indica se é um inimigo (e qual) ou o jogador
, posicao :: Posicao                    --Coordenadas atuais
, direcao :: Direcao                    --Para qual posiçao (N,S,L,O) está virado
, tamanho :: (Float, Float)             --Tamanho do personagem descrito por um rectângulo largura X altura
, emEscada :: Bool                      --Indica se o personagem se encontra a subir/descer uma escada
, ressalta :: Bool                      --Indica se o personagem deve trocar de direçao ao colidir com uma parede ou ao chegar no final de uma plataforma
, vida :: Int                           --Quantas vidas faltam
, pontos :: Int                         --Pontuaçao acumulada
, aplicaDano :: (Bool, Double)          --Indica se tem o martelo armado e por quanto tempo ainda. Quando ativo, cria uma hitbox à frente do jogador (com as dimensoes do jogador) e qualquer inimigo que encoste sofre dano
, velocidadeVertical :: Velocidade
}

-- | Largura dos blocos
l :: (Float,Float)                      
l = (16,16)

-- | Alias de tipo representando a velocidade.
type Velocidade = Float

-- | Tipo de dado que representa o tipo de entidade (jogador ou inimigo).   
data Entidade = Jogador | MacacoMalvado | Fantasma deriving (Show,Eq,Read)

-- | Tipo de dado que representa colecionáveis no jogo.
data Colecionavel = Martelo | Moeda deriving (Show,Eq,Read)

-- | Tipo de dado que representa o estado geral do jogo.
data Jogo = Jogo 
    { menu :: Menu
    , mapa :: Mapa
    , inimigos :: [Personagem]
    , colecionaveis :: [(Colecionavel, Posicao)]
    , jogador :: Personagem
    } 

-- | Constante representando a velocidade de movimento dos personagens.
mov :: Float
mov = 2 

-- | Constante representando a altura do salto para os personagens.
jump :: Float
jump = 2


