module OpenFallingBlock.GUI where

import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL
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

data Blocks = Blocks VertexArrayObject NumArrayIndices BufferObject ShaderProgram

makeSquares :: ([Word32], [Vector2 Float])
makeSquares = (replicate 200 0, [Vector2 ((32*x)+16.0) ((32*y)+16.0) | x <- [0..9], y <- [0..19]])

main = do
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
    GLFW.setKeyCallback win (Just keyPressed)
    GLFW.setWindowCloseCallback win (Just shutdown)
    clearColor $= Color4 0.1 0.1 0.1 1.0
    let ortho = orthoMatrix 0.0 (fromIntegral width) (fromIntegral height) 0.0 (-1.0) 1.0
    let (types, coords) = makeSquares
    mDrawable <- initBlocks types coords
    case mDrawable of
        Left err -> putStrLn err
        Right blocks@(Blocks _ _ _ sp) -> do
            setUniform sp "uProj" (ortho :: M44 Float)
            withWindow win $ draw blocks

initBlocks :: [Word32] -> [Vector2 Float] -> IO (Either String Blocks)
initBlocks types verts = do
    sp@(ShaderProgram _ _ prog) <- loadShaderFamily "res/shader"
    typeBuf <- makeBuffer ArrayBuffer types
    vao <- makeVAO $ 
        let blockPos = VertexArrayDescriptor 2 Float (fromIntegral $ sizeOf (head verts)) offset0
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

draw (Blocks vao n typesBuf sp) = do
    bindVertexArrayObject $= Just vao
    drawArrays Points 0 n
    bindBuffer ArrayBuffer $= Just typesBuf
    withMappedBuffer ArrayBuffer WriteOnly 
        (\ptr -> do
            rs <- replicateM 200 $ randomRIO (0, 3)
            sequence_ [pokeElemOff (ptr :: Ptr Word32) i r | (i,r) <- zip [0..199] rs])
        (\failure -> putStrLn $ "withMappedBuffer failure: " ++ show failure)

withWindow :: Window -> IO () -> IO ()
withWindow win io = do
    clear [ColorBuffer]
    io
    GLFW.swapBuffers win
    forever $ do
        GLFW.waitEventsTimeout 0.2
        withWindow win io

shutdown win = do
    GLFW.destroyWindow win
    GLFW.terminate
    _ <- exitSuccess
    return ()

keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()
