module Tarefa4Spec (testesTarefa4) where

import Tarefa4
import Test.HUnit
import DataStruct
import Maps
import Main

mapa01 :: Mapa
mapa01 =
  Mapa
    ((8.5, 6.5), Este)
    (5, 1.5)
    [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Alcapao, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]

inimigoParado =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Fantasma,
      posicao = (2.5, 7.6),
      direcao = Este,
      tamanho = (1, 1),
      emEscada = False,
      ressalta = True,
      vida = 1,
      pontos = 0,
      aplicaDano = (False, 0)
    }

jogadorParado =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Jogador,
      posicao = (8.5, 7),
      direcao = Oeste,
      tamanho = (0.8, 0.8),
      emEscada = False,
      ressalta = False,
      vida = 10,
      pontos = 0,
      aplicaDano = (False, 0)
    }

jogo01 :: Jogo
jogo01 =
  Jogo
    { mapa = mapa01,
      inimigos = [inimigoParado],
      colecionaveis = [],
      jogador = jogadorParado
    }

teste01 :: Test
teste01 = "T01: Quando não há nenhuma acção, o jogo permanece inalterado" ~: jogo01 ~=? atualiza [Nothing] Nothing jogo01

andarDireita01 :: Jogo
andarDireita01 = atualiza [Nothing] (Just AndarDireita) jogo01

teste02 :: Test
teste02 = TestLabel "T02" $ test [testeA, testeB]
  where
    testeA = "A: Quando a acção é AndarDireita, o vetor velocidade do jogador é positivo na componente do X" ~: True ~=? (fst . velocidade . jogador $ resultadoAndarDireita) > 0
    testeB = "B: Quando a acção é AndarDireita, a orientação do jogador é Este" ~: Este ~=? (direcao . jogador $ resultadoAndarDireita)
    resultadoAndarDireita = atualiza [Nothing] (Just AndarDireita) jogo01

teste03 :: Test
teste03 = TestLabel "T03" $ test [testeA, testeB]
  where
    testeA = "A: Quando a acção é AndarEsquerda, o vetor velocidade do jogador é negativo na componente do X" ~: True ~=? (fst . velocidade . jogador $ resultadoAndarDireita) < 0
    testeB = "B: Quando a acção é AndarEsquerda, a orientação do jogador é Oeste" ~: Oeste ~=? (direcao . jogador $ resultadoAndarDireita)
    resultadoAndarDireita = atualiza [Nothing] (Just AndarEsquerda) jogo01

teste04 :: Test
teste04 = TestLabel "T04" $ test [testeA, testeB]
  where
    testeA = "A: Quando a acção é Saltar, o vetor velocidade do jogador é negativo na componente do Y" ~: True ~=? (snd . velocidade . jogador $ resultadoSaltar) < 0
    testeB = "B: Quando a acção é Saltar, a orientação do jogador não muda" ~: (direcao . jogador $ jogo01) ~=? (direcao . jogador $ resultadoSaltar)
    resultadoSaltar = atualiza [Nothing] (Just Saltar) jogo01

jogadorEmFrenteEscada =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Jogador,
      posicao = (7.5, 7),
      direcao = Oeste,
      tamanho = (0.8, 0.8),
      emEscada = False,
      ressalta = False,
      vida = 10,
      pontos = 0,
      aplicaDano = (False, 0)
    }

jogo02 :: Jogo
jogo02 =
  Jogo
    { mapa = mapa01,
      inimigos = [inimigoParado],
      colecionaveis = [],
      jogador = jogadorEmFrenteEscada
    }

teste05 :: Test
teste05 = TestLabel "T05" $ test [testeA, testeB]
  where
    testeA = "A: Quando a acção é Subir, o vetor velocidade do jogador é negativo na componente do Y" ~: True ~=? (snd . velocidade . jogador $ resultadoSubir) < 0
    testeB = "B: Quando a acção é Subir, o jogador passa a estar em escada" ~: True ~=? (emEscada . jogador $ resultadoSubir)
    resultadoSubir = atualiza [Nothing] (Just Subir) jogo02

jogadorEmEscada =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Jogador,
      posicao = (7, 7),
      direcao = Norte,
      tamanho = (0.8, 0.8),
      emEscada = True,
      ressalta = False,
      vida = 10,
      pontos = 0,
      aplicaDano = (False, 0)
    }

teste06 :: Test
teste06 = TestLabel "T06" $ test [testeA, testeB]
  where
    testeA = "A: Quando a acção é Descer, o vetor velocidade do jogador é positivo na componente do Y" ~: True ~=? (snd . velocidade . jogador $ resultadoSubir) > 0
    testeB = "B: Quando a acção é Descer, o jogador continua em escada" ~: (emEscada jogadorEmEscada) ~=? (emEscada . jogador $ resultadoSubir)
    resultadoSubir = atualiza [Nothing] (Just Descer) jogo02

testesTarefa4 :: Test
testesTarefa4 = TestLabel "Tarefa4 (atualiza)" $ test [teste01, teste02, teste03, teste04, teste05, teste06]
