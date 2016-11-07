{-# LANGUAGE ExistentialQuantification #-}

module ARD.World where

import ARD.Camera
import ARD.Color
import ARD.Geometric as G
import ARD.Ray
import ARD.ViewPlane

data SceneObject = forall a. GeometricObject a => SceneObject a

instance GeometricObject SceneObject where
  hit (SceneObject o) ray = hit o ray

data SceneCamera = forall a. Camera a => SceneCamera a

instance Camera SceneCamera where
  generateRay (SceneCamera camera) v = generateRay camera v

data World
  = World
  { camera :: SceneCamera
  , viewPlane :: ViewPlane
  , sceneObjects :: [SceneObject]
  , backgroundColor :: Color
  }

