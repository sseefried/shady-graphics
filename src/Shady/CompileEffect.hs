{-# LANGUAGE RankNTypes, GADTs, TypeFamilies, ScopedTypeVariables, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}
module Shady.CompileEffect(
  -- data types
  ShadyEffect(..), ShadyGeometry(..), GLSLEffect {-opaque-}, UIElem {-opaque-}, UI {-opaque-},
  Color,
  -- smart constructors for UIElem
  uiTime, uiSliderF, uiSliderI, {- monad instance -}
  -- smart constructors for ShadyEffect and ShadyGeometry record types
  shadyEffect, shadyGeometry,
  -- functions that operate on opaque data type GLSLEffect
  compileEffect,                       -- constructs   GLSLEffect
  fragmentShader, vertexShader, uniformNamesOfGLSLEffect, uiSpecOfGLSLEffect,-- deconstructs GLSLEffect
) where


-- System libraries
import Text.Printf

-- friends
import Shady.Image          (Image)
import Shady.Color          (Color, HasColor, clear, toColor)
import Shady.CompileImage   (eyePos)
import Shady.CompileSurface (wrapSurfForEffect, EyePosE, Zoom)
import Shady.ParamSurf      (SurfD, xyPlane)
import Shady.Lighting       (View, view1, basicStd)
import Shady.CompileE       (GLSL(..))
import Shady.CompileEs      (shaderProgram)
import Shady.Language.Exp   (R2, R3, R3E, pureE, patE, patT, ExpT(..), E(..),
                             FromE(..), HasType, pat)
import Shady.Misc           (EyePos)
import TypeUnary.Vec        (vec3, Vec1)
import Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (intersperse)
import Control.Monad.State.Lazy
import Data.Typeable


data ShadyEffect c = ShadyEffect {
  shadyGeometryUI  :: UI (ShadyGeometry c),
  shadyEyePos      :: EyePos,
  shadyViewGen     :: R3E -> View
} deriving (Typeable)

-- default shadyEffect
shadyEffect :: UI (ShadyGeometry c) -> ShadyEffect c
shadyEffect shadyGeomUI = ShadyEffect {
    shadyGeometryUI  = shadyGeomUI,
    shadyEyePos      = eyePos,
    shadyViewGen     = view1 }

data ShadyGeometry c = ShadyGeometry {
  shadyImage   :: Image c,
  shadySurface :: SurfD
}

shadyGeometry :: ShadyGeometry Color
shadyGeometry = ShadyGeometry {
    shadyImage       = const $ clear,
    shadySurface     = xyPlane }

data UIElem a where
  UISliderF :: String -- ^ title
            -> Float  -- ^ lower bound
            -> Float  -- ^ default
            -> Float  -- ^ upper bound
            -> Maybe Int -- ^ ticks
            -> UIElem (E (Vec1 Float))
  UISliderI :: String -- ^ title
            -> Int    -- ^ lower bound
            -> Int    -- ^ default
            -> Int    -- ^ upper bound
            -> UIElem (E (Vec1 Int))
  UITime    :: UIElem (E (Vec1 Float))

data UIElemWithUniformIndex a = UIElemWithUniformIndex Int (UIElem a)

data UI a where
  UIElem  :: (FromE a, HasType (ExpT a)) => UIElem a -> UI a
  UIBind  :: UI a -> (a -> UI b) -> UI b
  UIReturn :: a -> UI a


elemName :: forall a.String -> UIElem a -> State Int (Int, String)
elemName uniquePrefix e = do
  i <- get
  modify (+1)
  let suffix :: String
      suffix = case e of
                 UISliderF _ _ _ _ _ -> "float_slider"
                 UISliderI _ _ _ _   -> "int_slider"
                 UITime              -> "time"
  return (i, printf "%s_%d_%s" uniquePrefix i suffix)

-- Untyped version of Variables in Shady.Language.Exp
data VU = VU { uVarName :: String, uVarType :: String }

instance Show VU where
  show vu = printf "uniform %s %s" (uVarType vu) (uVarName vu)

runUIState :: String -> UI a -> State Int (a, [(VU,String)])
runUIState uniquePrefix ui = case ui of
  UIReturn a          -> return (a, [])
  UIBind (UIElem e) f -> do
    (i,name) <- elemName uniquePrefix e
    let vu = VU name (show (patT p))
        p = pat $ name
    (a, varsAndElems) <- go . f $ (fromE . patE $ p)
    return $ (a, (vu, uiElemToJSONString i e):varsAndElems)
  UIBind nextUI f -> do
    (a, varsAndElems)  <- go nextUI
    (a', varsAndElems') <- go (f a)
    return (a', varsAndElems ++ varsAndElems')
  where
    go :: UI a -> State Int (a, [(VU, String)])
    go = runUIState uniquePrefix

runUI :: String -> UI a -> (a, [(VU, String)])
runUI uniquePrefix = fst . flip runState 0 . runUIState uniquePrefix

instance Monad UI where
  return = UIReturn
  (>>=)  = UIBind

type VertexPosAttribute = R2

data GLSLEffect = GLSLEffect (GLSL ((), (R3, (R3, (R3, (R3, Zoom)))))
                               VertexPosAttribute) [VU] [String]

toShader :: [VU] -> String -> String
toShader uniforms shader = printf "%s\n%s\n%s" shaderHeaders uniformDecs shader
  where
    uniformDecs :: String
    uniformDecs = concatMap (printf "%s;\n" . show) $ uniforms
    shaderHeaders = unlines [
        "#version 120"
--      , "precision highp float;"
      , ""
      , "#define _attribute meshCoords"
      , "#define _uniform_SSSSS zoom"
      , "#define _uniform_SSSSF pan"
      , "/* aRow, bRow, and cRow are three rows of the rotation matrix */"
      , "#define _uniform_SSSF  cRow"
      , "#define _uniform_SSF   bRow"
      , "#define _uniform_SF    aRow"
      , "/* varying_F is just copy of mesh_coords"
      , "   varying_S is vertex position of mesh coordinate after transformation */"
      ]


--
-- | A selector function for extracting fragment shader from GLSLEffect
--
fragmentShader  :: GLSLEffect -> String
fragmentShader (GLSLEffect (GLSL _ fs _ _) uniforms _) = toShader uniforms fs

--
-- | A selector function for extracting vertex shader from GLSLEffect
--
vertexShader :: GLSLEffect -> String
vertexShader (GLSLEffect (GLSL vs _ _ _) uniforms _) = toShader uniforms vs

uniformNamesOfGLSLEffect :: GLSLEffect -> [String]
uniformNamesOfGLSLEffect (GLSLEffect _ uniforms _) = map uVarName uniforms

-- Produces UI Specification in JSON format
uiSpecOfGLSLEffect :: GLSLEffect -> String
uiSpecOfGLSLEffect (GLSLEffect _ _ jsonStrings) = "[" ++ (concat $ (intersperse ", " jsonStrings)) ++ "]"

--
-- | Compiles a ShadyEffect to a GLSL program.
--
-- An OpenGL (or WebGL) program that links to this GLSL program
-- should set up a 'uniform' value denoting the time value of the animation
-- and an 'attribute' denoting the vertex positions.
--
-- (For definitions of 'uniform' and 'attribute' see the GLSL spec)
--
compileEffect :: forall c. (HasColor c) => String -> ShadyEffect c -> GLSLEffect
compileEffect prefix e = GLSLEffect glsl uniforms jsons
  where
    glsl = shaderProgram $ wrapSurfForEffect eyePosE (\() -> fullSurf)
    (uniforms,jsons) = unzip uniformsAndJsons
    (geom, uniformsAndJsons) = runUI prefix $ shadyGeometryUI e
    fullSurf = (basicStd, shadyViewGen e, surface, image)
       where
         surface = shadySurface geom
         image   = toColor . shadyImage geom
    eyePosE :: EyePosE
    eyePosE = pureE (vec3 ex ey ez) where (ex,ey,ez) = shadyEyePos e

--
-- Smart constructors
--
uiTime :: UI (E (Vec1 Float))
uiTime    = UIElem UITime

--
-- | Creates a slider that produces Float values.
--
-- Conditions:
--   minVal <= defaultVal <= maxVal
--
-- If these conditions do not hold then "sensible" values are substituted.
--
uiSliderF :: String -> Float -> Float -> Float -> Maybe Int -> UI (E (Vec1 Float))
uiSliderF title minVal defaultVal maxVal mbTicks =
  UIElem (UISliderF title minVal' defaultVal' maxVal' mbTicks)
  where (minVal', defaultVal', maxVal') = sensible (minVal, defaultVal, maxVal)

--
-- | Creates a slider that produces Int values.
--
-- Conditions:
--   minVal <= defaultVal <= maxVal
--
-- If these conditions do not hold then "sensible" values are substituted.
--
uiSliderI :: String -> Int -> Int -> Int -> UI (E (Vec1 Int))
uiSliderI title minVal defaultVal maxVal =
  UIElem (UISliderI title minVal' defaultVal' maxVal')
  where
    (minVal', defaultVal', maxVal') = sensible (minVal, defaultVal, maxVal)

--
-- A helper function to clamp "bad" slider values to sensible ones.
--
-- Examples:
--  sensible (0, 2, -3)  == (0, 0, 0)
--  sensible (0, 5,  3)  == (0, 3, 3)
--  sensible (0, -5, 3)  == (0, 0, 3)
--  sensible (0, 2,  5)  == (0, 2, 5)
--  sensible (0, 2,  5)  == (0, 2, 5)
--
sensible :: (Num a, Ord a) => (a,a,a) -> (a,a,a)
sensible (minVal, defaultVal, maxVal) = (minVal, defaultVal', maxVal')
  where
    maxVal'     = minVal `max` maxVal
    defaultVal' = (minVal `max` defaultVal) `min` maxVal'

---------------------------------

instance ToJSON (UIElemWithUniformIndex a) where
  toJSON (UIElemWithUniformIndex uniformIndex uiElem) = case uiElem of
    UISliderF title minVal value maxVal mbTicks ->
      object ([ "sort" .= ("float_slider" :: String), "glslUniformIndex" .= uniformIndex,
                "title" .= title, "min" .= minVal,
                "value" .= value, "max" .= maxVal ] ++ maybe [] (\t -> ["ticks" .= t]) mbTicks)
    UISliderI title minVal value maxVal ->
      object [ "sort" .= ("int_slider" :: String), "glslUniformIndex" .= uniformIndex, "title" .= title,
               "min" .= minVal, "value" .= value, "max" .= maxVal ]
    UITime -> object [ "sort" .= ("time" :: String)]

uiElemToJSONString :: Int -> UIElem a -> String
uiElemToJSONString uniformIndex uiElem =
  BS.unpack . JSON.encode $ UIElemWithUniformIndex uniformIndex uiElem

--------------------------------------------------------

