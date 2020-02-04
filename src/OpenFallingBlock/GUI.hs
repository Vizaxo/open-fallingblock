module OpenFallingBlock.GUI where

import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL hiding (R)
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
import Control.Monad
import System.Exit
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Linear.Matrix
import Data.Word
import System.Random
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State
import Control.Lens
import Data.Array

import OpenFallingBlock.Game
import OpenFallingBlock.Pieces as Piece
import OpenFallingBlock.Input

data Blocks = Blocks VertexArrayObject NumArrayIndices BufferObject ShaderProgram

makeSquares :: ([Word32], [Vector2 Float])
makeSquares = (replicate 200 3, [Vector2 ((32*x)+16.0) ((32*y)+16.0) | x <- [0..9], y <- [0..19]])

runGUI :: IO ()
runGUI = do
  let width  = 320
      height = 640
  b <- GLFW.init
  putStrLn $ "GLFW.init: " ++ (show b)
  GLFW.defaultWindowHints
  GLFW.windowHint (GLFW.WindowHint'Resizable False)
  GLFW.windowHint (GLFW.WindowHint'Focused True)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile OpenGLProfile'Core)
  Just win <- GLFW.createWindow width height "open-fallingblock" Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  GLFW.swapInterval 1
  GLFW.setWindowCloseCallback win (Just shutdown)
  chan <- newTChanIO
  GLFW.setKeyCallback win (Just $ keyPressed chan)
  clearColor $= Color4 0.1 0.1 0.1 1.0
  let ortho = orthoMatrix 0.0 (fromIntegral width) (fromIntegral height) 0.0 (-1.0) 1.0
      (types, coords) = makeSquares
  gameTickRate <- getTimerFrequency >>= return . (`div` 2)
  initBlocks types coords >>= \case
    Left err -> putStrLn err
    Right blocks@(Blocks _ _ _ sp) -> do
      setUniform sp "uProj" (ortho :: M44 Float)
      void $ flip runStateT (Game emptyBoard Nothing 0 0) $ forever $ do
        liftIO (atomically (tryReadTChan chan)) >>= \case
          Nothing -> pure ()
          Just i  -> runInput i
        runFrame blocks
        ticksPrev <- gets (^. platformTicks)
        ticksNow <- liftIO getTimerValue
        case (ticksNow - ticksPrev > gameTickRate) of
          True  -> do gameTick
                      modify (set platformTicks ticksNow)
          False -> return ()
        liftIO $ do
          GLFW.swapBuffers win
          GLFW.pollEvents

initBlocks :: [Word32] -> [Vector2 Float] -> IO (Either String Blocks)
initBlocks types verts = do
  sp@(ShaderProgram _ _ prog) <- loadShaderFamily "res/shader"
  typeBuf <- makeBuffer ArrayBuffer types
  vao <- makeVAO $
    let
      blockPos = VertexArrayDescriptor 2 Float (fromIntegral $ sizeOf (head verts)) offset0
      blockType = VertexArrayDescriptor 1 UnsignedInt (fromIntegral $ sizeOf (head types)) offset0
      blockPosAttrib  = getAttrib sp "pos"
      blockTypeAttrib = getAttrib sp "blockType"
    in do
      posBuf <- makeBuffer ArrayBuffer verts
      vertexAttribArray blockPosAttrib $= Enabled
      vertexAttribPointer blockPosAttrib $= (ToFloat, blockPos)
      bindBuffer ArrayBuffer $= Just typeBuf
      vertexAttribArray blockTypeAttrib $= Enabled
      vertexAttribPointer blockTypeAttrib $= (KeepIntegral, blockType)
  currentProgram $= Just prog
  etex <- readTexture "res/blocks.png"
  case etex of
    Left  err -> return $ Left err
    Right tex -> do
      textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
      texture2DWrap $= (Mirrored, Clamp)
      activeTexture $= TextureUnit 0
      textureBinding Texture2D $= Just tex
      setUniform sp "uTex" (TextureUnit 0)
      return $ Right $ Blocks vao (fromIntegral $ length verts) typeBuf sp

runFrame :: (MonadIO m, MonadState Game m) => Blocks -> m ()
runFrame (Blocks vao n typesBuf sp) = do
  b <- gets (^. board)
  a <- gets (^. active)
  liftIO $ do
    let toBlock Piece.Empty = 3
        toBlock Piece.Full  = 0
        bb = case a of
          Nothing -> b
          Just p  -> lockIn p b
        blockList = [(y+x*20, toBlock block) | ((x,y),block) <- assocs bb]
    clear [ColorBuffer]
    bindBuffer ArrayBuffer $= Just typesBuf
    withMappedBuffer ArrayBuffer WriteOnly
      (\ptr -> sequence_ [pokeElemOff (ptr :: Ptr Word32) i block | (i,block) <- blockList])
      (\failure -> putStrLn $ "withMappedBuffer failure: " ++ show failure)
    bindVertexArrayObject $= Just vao
    drawArrays Points 0 n

shutdown :: Window -> IO ()
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  exitSuccess

keyPressed :: TChan Input -> Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()
keyPressed chan win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed chan win GLFW.Key'A      _ GLFW.KeyState'Pressed _ = atomically (writeTChan chan L)
keyPressed chan win GLFW.Key'E      _ GLFW.KeyState'Pressed _ = atomically (writeTChan chan R)
keyPressed chan win GLFW.Key'Comma  _ GLFW.KeyState'Pressed _ = atomically (writeTChan chan U)
keyPressed chan win GLFW.Key'O      _ GLFW.KeyState'Pressed _ = atomically (writeTChan chan D)
keyPressed chan win GLFW.Key'H      _ GLFW.KeyState'Pressed _ = atomically (writeTChan chan B)
keyPressed chan win GLFW.Key'T      _ GLFW.KeyState'Pressed _ = atomically (writeTChan chan A)
keyPressed chan _   _               _ _                     _ = return ()