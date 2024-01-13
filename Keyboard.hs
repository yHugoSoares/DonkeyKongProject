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

aplicaListaKey :: [Key] -> Jogo -> Bool -> Jogo
aplicaListaKey [] j _          = j
aplicaListaKey (h:t) e b = aplicaListaKey t (aplicaKey h e b) b

aplicaKey :: Key -> Jogo -> Bool -> Jogo
aplicaKey k j True                     = colisao k j 
aplicaKey (SpecialKey KeyUp) j _       = jogada Norte j
aplicaKey (SpecialKey KeyDown) j _     = jogada Sul j
aplicaKey (SpecialKey KeyLeft) j _     = jogada Oeste j
aplicaKey (SpecialKey KeyRight) j _    = jogada Leste j
aplicaKey _ j _                        = j

colisao :: Key -> Jogo -> Jogo
colisao k (Jogo menu mapa ini colec jog) = Jogo menu mapa ini colec (alteraColisao k jog parede chao)
                where parede = colisoesParede mapa jog 
                      chao = colisoesChao mapa jog 
        
alteraColisao :: Key -> Personagem -> Bool -> Bool -> Personagem
alteraColisao k (Personagem velo a (x,y) dire c f d h i j g) parede chao |parede && chao && (dire == Leste || dire == Sul) = Personagem velo a (x-velo,y+velo) dire c f d h i j g
                                                                         |parede && chao && (dire == Oeste || dire == Sul) = Personagem velo a (x+velo,y+velo) dire c f d h i j g
                                                                         |parede && (dire == Oeste) = Personagem velo a (x+velo,y) dire c f d h i j g
                                                                         |parede && (dire == Leste) = Personagem velo a (x-velo,y) dire c f d h i j g
                                                                         |chao  && (dire == Sul) = Personagem velo a (x,y+velo) dire c f d h i j g
                                                                         |chao && (dire == Norte) = Personagem velo a (x,y-velo) dire c f d h i j g
                                                                         |chao && (dire == Sul) && (dire == Oeste || dire == Leste) = Personagem velo a (x,y+velo) dire c f d h i j g
                                                                         |chao && (dire == Norte) && (dire == Oeste || dire == Leste) = Personagem velo a (x,y-velo) dire c f d h i j g
                                                                         |otherwise = Personagem velo a (x,y-velo) dire c f d h i j g


jogada ::Direcao -- A 'Jogada' a efetuar.
        -> Jogo -- O 'Estado' anterior.
        -> Jogo -- O 'Estado' resultante após o jogador efetuar a jogada.      
jogada d (Jogo menu m ini colec jo) = Jogo menu m ini colec (playerNovo jo)
                                               where 
                                                     playerNovo jo = alteraJogada jo d (Jogo menu m ini colec jo)

alteraJogada :: Personagem -> Direcao -> Jogo -> Personagem
alteraJogada (Personagem velo a (x,y) dire c f n h i j g) d e |d == Norte   = Personagem velo a (x,y+jump) Norte c f n h i j g
                                                            |d == Sul       = Personagem velo a (x,y-jump) Sul c f n h i j g
                                                            |d == Oeste     = Personagem velo a (x-velo,y) Oeste c f n h i j g
                                                            |d == Leste     = Personagem velo a (x+velo,y) Leste c f n h i j g