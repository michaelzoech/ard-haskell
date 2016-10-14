{-# LANGUAGE ExistentialQuantification #-}

module ARD.World where

import ARD.Color
import ARD.Geometric as G
import ARD.Ray
import ARD.ViewPlane

data SceneObject = forall a. GeometricObject a => SceneObject a

instance GeometricObject SceneObject where
  hit (SceneObject o) ray = hit o ray

data World
  = World
  { viewPlane :: ViewPlane
  , sceneObjects :: [SceneObject]
  , backgroundColor :: Color
  }

