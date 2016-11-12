{-# LANGUAGE ExistentialQuantification #-}

module ARD.World where

import ARD.Camera
import ARD.Color
import ARD.Geometric as G
import ARD.Light
import ARD.Ray
import ARD.ViewPlane

data SceneObject = forall a. GeometricObject a => SceneObject a

instance GeometricObject SceneObject where
  hit (SceneObject o) = hit o
  shadowHit (SceneObject o) = shadowHit o

data World
  = World
  { camera :: Camera
  , viewPlane :: ViewPlane
  , sceneObjects :: [SceneObject]
  , lights :: [Light]
  , backgroundColor :: Color
  }

