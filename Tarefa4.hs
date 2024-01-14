module Tarefa4 where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataStruct
import Maps
import Data.List
import Tarefa1


atualiza :: [Maybe Acao] -> Acao -> Jogo -> Jogo
atualiza acao1 acaop jogo = jogo

reactKey :: Event -> [Key] -> [Key]
reactKey (EventKey key Up _ _)   = delete key
reactKey (EventKey key Down _ _) = (:) key
reactKey _                      = id

aplicaListaKey :: [Key] -> Jogo -> Jogo
aplicaListaKey [] j          = j
aplicaListaKey (h:t) e = aplicaListaKey t (aplicaKey h e)

aplicaKey :: Key -> Jogo -> Jogo
aplicaKey (SpecialKey KeyUp) j@(Jogo _ _ ini _ p)        = if colisoesParede (mapa j) p Norte || colisoesPersonagens p ini then j else jogada (Just Subir) j
aplicaKey (SpecialKey KeyDown) j@(Jogo _ _ ini _ p)      = if colisoesParede (mapa j) p Sul || colisoesPersonagens p ini then j else jogada (Just Descer) j
aplicaKey (SpecialKey KeyLeft) j@(Jogo _ _ ini _ p)      = if colisoesParede (mapa j) p Oeste || colisoesPersonagens p ini then j else jogada (Just AndarEsquerda) j
aplicaKey (SpecialKey KeyRight) j@(Jogo _ _ ini _ p)     = if colisoesParede (mapa j) p Leste || colisoesPersonagens p ini  then j else jogada (Just AndarDireita) j
aplicaKey _ j                                            = j


jogada ::Maybe Acao -- A 'Jogada' a efetuar.
        -> Jogo -- O 'Estado' anterior.
        -> Jogo -- O 'Estado' resultante após o jogador efetuar a jogada.      
jogada a (Jogo menu m ini colec jo) = Jogo menu m ini colec (playerNovo jo)
                                               where
                                                     playerNovo jo = alteraJogada jo a (Jogo menu m ini colec jo)

alteraJogada :: Personagem -> Maybe Acao -> Jogo -> Personagem
alteraJogada p@(Personagem {posicao = (x,y), velocidade = velo}) a e |a == Just Saltar              = p {posicao =(x,y+jump)}
                                                                     |a == Just Subir               = p {posicao =(x,y+jump)}
                                                                     |a == Just Descer              = p {posicao =(x,y-velo)}
                                                                     |a == Just AndarEsquerda       = p {posicao =(x-velo,y),direcao = Oeste}
                                                                     |a == Just AndarDireita        = p {posicao =(x+velo,y),direcao = Leste}

