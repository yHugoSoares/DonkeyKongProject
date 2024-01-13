module Physics where

import DataStruct

gravidade :: Float
gravidade = -1.0

atualizaPersonagem :: Float -> Personagem -> Personagem
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
atualizaCollectibles _ colecionaveis = colecionaveis  -- Placeholder, you need to implement the actual logic
