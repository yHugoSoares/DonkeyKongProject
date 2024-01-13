-- | Módulo contendo as funções relacionadas ao teclado e interação com o usuário.
module Keyboard where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import DataStruct 
import Maps 
import Data.List
import Tarefa1

-- | Função que reage a um evento de tecla.
reactKey :: Event -> [Key] -> [Key]
reactKey (EventKey key Up _ _)   = delete key
reactKey (EventKey key Down _ _) = (:) key
reactKey _                      = id

-- | Função que aplica uma lista de teclas a um estado de jogo.
aplicaListaKey :: [Key] -> Jogo -> Bool -> Jogo
aplicaListaKey [] j _          = j
aplicaListaKey (h:t) e b = aplicaListaKey t (aplicaKey h e b) b

-- | Função que aplica uma tecla a um estado de jogo.
aplicaKey :: Key -> Jogo -> Bool -> Jogo
aplicaKey k j True                     = colisao k j 
aplicaKey (SpecialKey KeyUp) j _       = jogada Norte j
aplicaKey (SpecialKey KeyDown) j _     = jogada Sul j
aplicaKey (SpecialKey KeyLeft) j _     = jogada Oeste j
aplicaKey (SpecialKey KeyRight) j _    = jogada Leste j
aplicaKey _ j _                        = j

-- | Função que trata colisões do jogador com o ambiente.
colisao :: Key -> Jogo -> Jogo
colisao k (Jogo menu mapa ini colec jog) = Jogo menu mapa ini colec (alteraColisao k jog parede chao)
                where parede = colisoesParede mapa jog 
                      chao = colisoesChao mapa jog 
        
-- | Função que altera a colisão do jogador com o ambiente.
alteraColisao :: Key -> Personagem -> Bool -> Bool -> Personagem
alteraColisao k (Personagem velo a (x,y) dire c f g h i j) parede chao 
  | (parede == True) && (chao == True) && (dire == Leste || dire == Sul) = Personagem velo a (x-velo,y+velo) dire c f g h i j
  | (parede == True) && (chao == True) && (dire == Oeste || dire == Sul) = Personagem velo a (x+velo,y+velo) dire c f g h i j
  | (parede == True) && (dire == Oeste) = Personagem velo a (x+velo,y) dire c f g h i j
  | (parede == True) && (dire == Leste) = Personagem velo a (x-velo,y) dire c f g h i j
  | (chao == True) && (dire == Sul) = Personagem velo a (x,y+velo) dire c f g h i j
  | (chao == True) && (dire == Norte) = Personagem velo a (x,y-velo) dire c f g h i j
  | (chao == True) && (dire == Sul) && (dire == Oeste || dire == Leste) = Personagem velo a (x,y+velo) dire c f g h i j
  | (chao == True) && (dire == Norte) && (dire == Oeste || dire == Leste) = Personagem velo a (x,y-velo) dire c f g h i j
  | otherwise = Personagem velo a (x,y-velo) dire c f g h i j

-- | Função que realiza a jogada do jogador.
jogada :: Direcao -- A 'Jogada' a ser efetuada.
       -> Jogo    -- O 'Estado' anterior.
       -> Jogo    -- O 'Estado' resultante após o jogador efetuar a jogada.
jogada d (Jogo menu m ini colec jo) = Jogo menu m ini colec (playerNovo jo) 
  where 
    playerNovo jo = alteraJogada jo d (Jogo menu m ini colec jo)

-- | Função que altera a jogada do jogador.
alteraJogada :: Personagem -> Direcao -> Jogo -> Personagem
alteraJogada (Personagem velo a (x,y) dire c f g h i j) d e 
  | d == Norte = Personagem velo a (x,y+jump) Norte c f g h i j
  | d == Sul   = Personagem velo a (x,y-jump) Sul c f g h i j
  | d == Oeste = Personagem velo a (x-velo,y) Oeste c f g h i j
  | d == Leste = Personagem velo a (x+velo,y) Leste c f g h i j
