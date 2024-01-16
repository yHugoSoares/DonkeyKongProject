{-|
Module: Tarefa4
Description: Modulo Haskell Gloss Tarefa4
Copyright : Hugo Soares (a107293)
            Henrique Brito (a107378)
O objetivo desta tarefa é implementar a função 'atualiza'
que valida e calcula as novas direções e velocidades
dos personagens (inimigos e jogador) de acordo com as ações dadas.
-}

module Tarefa4 where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataStruct
import Maps
import Data.List
import Tarefa1

-- | Função principal que atualiza o estado do jogo com base nas ações fornecidas.
atualiza :: [Maybe Acao] -> Acao -> Jogo -> Jogo
atualiza acao1 acaop jogo = jogo

-- | Função para reagir a eventos de teclado, atualizando a lista de teclas pressionadas.
reactKey :: Event -> [Key] -> [Key]
reactKey (EventKey key Up _ _)   = delete key
reactKey (EventKey key Down _ _) = (:) key
reactKey _                      = id

-- | Aplica a lista de teclas pressionadas ao jogo.
aplicaListaKey :: [Key] -> Jogo -> Jogo
aplicaListaKey [] j          = j
aplicaListaKey (h:t) e = aplicaListaKey t (aplicaKey h e)

-- | Aplica uma tecla específica ao jogo.
aplicaKey :: Key -> Jogo -> Jogo
aplicaKey (SpecialKey KeyUp) j@(Jogo _ _ ini _ p)        = if colisoesParede (mapa j) p Norte || colisoesPersonagens p ini then j else jogada (Just Subir) j
aplicaKey (SpecialKey KeyDown) j@(Jogo _ _ ini _ p)      = if colisoesParede (mapa j) p Sul || colisoesPersonagens p ini then j else jogada (Just Descer) j
aplicaKey (SpecialKey KeyLeft) j@(Jogo _ _ ini _ p)      = if colisoesParede (mapa j) p Oeste || colisoesPersonagens p ini then j else jogada (Just AndarEsquerda) j
aplicaKey (SpecialKey KeyRight) j@(Jogo _ _ ini _ p)     = if colisoesParede (mapa j) p Leste || colisoesPersonagens p ini  then j else jogada (Just AndarDireita) j
aplicaKey _ j                                            = j

-- | Realiza a jogada com base na ação fornecida.
jogada :: Maybe Acao -- ^ A 'Jogada' a efetuar.
        -> Jogo -- ^ O 'Estado' anterior.
        -> Jogo -- ^ O 'Estado' resultante após o jogador efetuar a jogada.
jogada a (Jogo menu m ini colec jo) = Jogo menu m ini colec (playerNovo jo)
  where
    playerNovo jo = alteraJogada jo a (Jogo menu m ini colec jo)

-- | Altera a jogada com base na ação fornecida.
alteraJogada :: Personagem -> Maybe Acao -> Jogo -> Personagem
alteraJogada p@(Personagem {posicao = (x, y), velocidade = velo}) a e
  | a == Just Saltar              = p {posicao = (x, y + jump)}
  | a == Just Subir               = p {posicao = (x, y + velo)}
  | a == Just Descer              = p {posicao = (x, y - velo)}
  | a == Just AndarEsquerda       = p {posicao = (x - velo, y), direcao = Oeste}
  | a == Just AndarDireita        = p {posicao = (x + velo, y), direcao = Leste}

-- | Verifica a colisão com uma escada no mapa.
colisaoEscada :: Mapa -> Personagem -> Bool
colisaoEscada map@(Mapa a b mapa) p@(Personagem {posicao = (x, y)}) =
  case (obterValor mapa (round (x / 16), round (y + 16 / 16)), obterValor mapa (round (x / 16), round (y / 16)), obterValor mapa (round (x / 16), round (y - 16 / 16))) of
    (Escada, Escada, Escada) -> True
    (Plataforma, Escada, Escada) -> True
    (Escada, Plataforma, _) -> True
    (_, Escada, _) -> True
    (Escada, Vazio, _) -> True
    (_, _, Escada) -> True
    (_, _, _) -> False