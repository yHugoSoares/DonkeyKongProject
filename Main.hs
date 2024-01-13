module Main where

import Graphics.Gloss
    ( Picture(Text, Pictures, Color, Translate, Scale),
      Display(InWindow),
      black,
      blue,
      white,
      play,
      loadBMP)
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import DataStruct
import Keyboard
import Maps
import Tarefa1
-- import Tarefa3

-- Frame rate for the game
fr :: Int
fr = 50

-- Display settings
dm :: Display
dm = FullScreen
-- dm = InWindow "Donkey Kong" (640, 640) (0, 0)

-- Type alias for the game state
type EstadoGloss = (Jogo, [Key], [Picture], Jogador, Inimigo)

-- Initial game state
estadoInicial :: Jogo
estadoInicial = Jogo (Opcoes Jogar) mapaGrande malvados [] mario

-- Function to create the initial state for Gloss, which includes a list of pictures
estadoGlossInicial :: [Picture] -> Jogador -> Inimigo -> EstadoGloss
estadoGlossInicial z skin inimigo = (estadoInicial,[], z, skin, inimigo)

-- Function to handle events in Gloss
reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
reageEventoGloss evento@(EventKey k Down _ _) (jogo@(Jogo (Opcoes _) m mal b jo) ,keys, z,skin, inimigo) = (stageMenu k jogo, reactKey evento keys, z,skin, inimigo)
                                                                                             where op = menu estadoInicial
reageEventoGloss evento (estadoInicial,keys, z,skin, inimigo) = (estadoInicial, reactKey evento keys, z,skin, inimigo)

-- Function to update the game state
atualizaEstado :: Float -> EstadoGloss -> EstadoGloss
atualizaEstado n (estadoInicial@(Jogo _ mapa _ _ mario), keys, z, skin, inimigo) = (aplicaListaKey keys estadoInicial b, keys, z, skin, inimigo)
    where b = colisoesChao mapa mario || colisoesParede mapa mario

-- atualizaEstado :: Float -> EstadoGloss -> EstadoGloss
-- atualizaEstado n (estadoInicial@(Jogo _ mapa malvados _ mario), keys, z, skin, inimigo) =
--   (aplicaListaKey keys estadoAtualizado colisao, keys, z, skin, inimigo)
--     where
--       estadoAtualizado = Jogo ModoJogo mapa malvados novoB marioAtualizado
--       novoB = atualizaCollectibles n b  -- Assuming `atualizaCollectibles` updates the collectibles based on time
--       colisao = colisoesChao mapa marioAtualizado || colisoesParede mapa marioAtualizado
--       marioAtualizado = atualizaPersonagem n mario
--       b = colecionaveis estadoInicial  -- Assuming `coletaveis` is a function that gets the current list of collectibles

-- Função que atualiza o estado do jogo com base na tecla pressionada e no estado atual do jogo
stageMenu :: Key -> Jogo -> Jogo
stageMenu k jogo = case menu jogo of
        (Opcoes op) ->
            case k of
                (SpecialKey KeyEnter) -> case op of
                                            Jogar -> jogo {menu=ModoJogo}   -- Muda para o modo de jogo normal quando a tecla enter for pressionada
                                            Sair -> error "Fim do jogo"
                (SpecialKey KeyDown) -> jogo {menu = Opcoes (mudaOP op)}  -- Move a seleção para a opção seguinte
                (SpecialKey KeyUp)   -> jogo {menu = Opcoes (mudaOP op)}  -- Move a seleção para a opção anterior
                _ -> jogo
        _ -> jogo

-- Função que muda a opção selecionada no menu
mudaOP :: Opcao -> Opcao
mudaOP op = case op of
               Jogar -> Sair
               Sair -> Jogar

-- Função que desenha o estado atual do jogo na tela usando a biblioteca Gloss
desenhaEstadoGloss :: EstadoGloss -> Picture
desenhaEstadoGloss (Jogo (Opcoes op) _ _ _ _,_,_,_,_)  = drawOptions op
desenhaEstadoGloss (estadoinicio@(Jogo ModoJogo _ _ _ _),keys, z,skin, inimigo) = desenhaMapa estadoinicio z skin inimigo

-- Função que desenha as opções do menu
drawOptions op =   case op of
    Jogar -> Pictures [Translate (-50) 10 $ Color blue $ drawOption "Jogar",
                       Translate (-50) (-70) $ Color white $ drawOption "Sair"]
    Sair ->  Pictures [Color white $ Translate (-50) 10 $ drawOption "Jogar",
                       Color blue $ Translate (-50) (-70) $ drawOption "Sair"]

-- Função que desenha uma opção do menu
drawOption option =  Scale 0.5 0.5 $ Text option

-- Função principal que inicia o jogo
main :: IO ()
main = do
    loadMAPA <- carregaImagens
    marioOeste <- loadBMP "./img/Mariooeste.bmp"
    marioLeste <- loadBMP "./img/Marioleste.bmp"
    macaco <- loadBMP "./img/macaco.bmp"
    fantasma <- loadBMP "./img/fantasma.bmp"
    play dm                             -- Inicia a janela do jogo
        black                           -- Cor do fundo da janela
        fr                              -- Framerate
        (estadoGlossInicial loadMAPA (Jogador,[marioOeste,marioLeste]) [(MacacoMalvado,macaco),(Fantasma, fantasma)])    -- Estado inicial do jogo
        desenhaEstadoGloss              -- Função que desenha o estado do jogo
        reageEventoGloss                -- Função que reage a eventos do usuário
        atualizaEstado                  -- Função que atualiza o estado do jogo a cada frame