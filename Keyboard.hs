module Keyboard where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataStruct
import Maps
import Data.List
import Tarefa1

reactKey :: Event -> [Key] -> [Key]
reactKey (EventKey key Up _ _)   = delete key
reactKey (EventKey key Down _ _) = (:) key
reactKey _                      = id

aplicaListaKey :: [Key] -> Jogo -> Jogo
aplicaListaKey [] j          = j
aplicaListaKey (h:t) e = aplicaListaKey t (aplicaKey h e)

aplicaKey :: Key -> Jogo -> Jogo
aplicaKey (SpecialKey KeyUp) j@(Jogo _ _ ini _ p)        = if colisoesParede (mapa j) p Norte || colisoesPersonagens p ini then j else jogada Norte j
aplicaKey (SpecialKey KeyDown) j@(Jogo _ _ ini _ p)      = if colisoesParede (mapa j) p Sul || colisoesPersonagens p ini then j else jogada Sul j
aplicaKey (SpecialKey KeyLeft) j@(Jogo _ _ ini _ p)      = if colisoesParede (mapa j) p Oeste || colisoesPersonagens p ini then j else jogada Oeste j
aplicaKey (SpecialKey KeyRight) j@(Jogo _ _ ini _ p)     = if colisoesParede (mapa j) p Leste || colisoesPersonagens p ini  then j else jogada Leste j
aplicaKey _ j                                            = j


jogada ::Direcao -- A 'Jogada' a efetuar.
        -> Jogo -- O 'Estado' anterior.
        -> Jogo -- O 'Estado' resultante após o jogador efetuar a jogada.      
jogada d (Jogo menu m ini colec jo) = Jogo menu m ini colec (playerNovo jo)
                                               where
                                                     playerNovo jo = alteraJogada jo d (Jogo menu m ini colec jo)

alteraJogada :: Personagem -> Direcao -> Jogo -> Personagem
alteraJogada p@(Personagem {posicao = (x,y), velocidade = velo}) d e |d == Norte     = p {posicao =(x,y+jump)}
                                                                     |d == Sul       = p {posicao =(x,y-velo)}
                                                                     |d == Oeste     = p {posicao =(x-velo,y),direcao = Oeste}
                                                                     |d == Leste     = p {posicao =(x+velo,y),direcao = Leste}