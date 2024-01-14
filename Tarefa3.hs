module Tarefa3 where

import DataStruct
    ( Jogo(Jogo, jogador),
      Personagem(Personagem, velocidadeVertical, posicao),
      Mapa,
      gravidade )
import Tarefa1


aplicaGravidade :: Jogo -> Jogo
aplicaGravidade jogo@(Jogo menu mapa mal colec jog) =
    jogo { jogador = aplicarGravidadeAoPersonagem mapa jog }

aplicarGravidadeAoPersonagem :: Mapa -> Personagem -> Personagem
aplicarGravidadeAoPersonagem mapa p@(Personagem vel tipo (x, y) dir tam emEsc res vida pontos dano velVert) =
    p { posicao = (x, y + velVert), velocidadeVertical = novaVelocidadeVert }
  where
    novaVelocidadeVert = if colisoesChao mapa p then 0 else velVert + gravidade


{-atualizaPersonagem :: Float -> Personagem -> Personagem
atualizaPersonagem dt personagem =
  personagem
    { posicao = newPos,
      velocidade = newVel,
      emEscada = emEscadaAtualizada,
      ressalta = ressaltaAtualizada
    }
  where
    emEscadaAtualizada = False  -- Placeholder, you need to implement proper logic for climbing stairs
    ressaltaAtualizada = False  -- Placeholder, you need to implement proper logic for changing direction

    -- Update velocity due to gravity
    newVelY = velocidade personagem + gravidade * dt
    newVel = newVelY

    -- Update position based on velocity
    newPos = (posX, posY + newVelY * dt)
    (posX, posY) = posicao personagem

atualizaCollectibles :: Float -> [(Colecionavel, Posicao)] -> [(Colecionavel, Posicao)]
atualizaCollectibles _ colecionaveis = colecionaveis  -- Placeholder, you need to implement the actual logic-}