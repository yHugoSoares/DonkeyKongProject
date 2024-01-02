module Maps where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
--import DataStruct
--import Maps

--Datas do jogo

data Mapa = Mapa (Posicao, Direcao) Posicao [[Bloco]]
data Bloco = Escada | Plataforma | Alcapao | Vazio
data Direcao = Norte | Sul | Leste | Oeste deriving Show
type Posicao = (Double,Double)--par ordenado que define (coluna,linha)
data Personagem = Personagem{ 
velocidade :: Velocidade    --Um vetor de velocidade normal
, tipo :: Entidade          --Indica se é um inimigo (e qual) ou o jogador
, posicao :: Posicao        --Coordenadas atuais
, direcao :: Direcao        --Para qual posiçao (N,S,L,O) está virado
, tamanho :: (Double, Double)--Tamanho do personagem descrito por um rectângulo largura X altura
, emEscada :: Bool          --Indica se o personagem se encontra a subir/descer uma escada
, ressalta :: Bool          --Indica se o personagem deve trocar de direçao ao colidir com uma parede ou ao chegar no final de uma plataforma
, vida :: Int               --Quantas vidas faltam
, pontos :: Int             --Pontuaçao acumulada
, aplicaDano :: (Bool, Double)--Indica se tem o martelo armado e por quanto tempo ainda. Quando ativo, cria uma hitbox à frente do jogador (com as dimensoes do jogador) e qualquer inimigo que encoste sofre dano
}
type Velocidade = Double 
data Entidade = MacacoMalvado | Fantasma | Jogador

data Colecionavel = Martelo | Moeda

data Jogo = Jogo {
mapa :: Mapa
, inimigos :: [Personagem]
, colecionaveis :: [(Colecionavel, Posicao)]
, jogador :: Personagem
}