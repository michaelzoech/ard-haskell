module ARD.World where

import ARD.Camera
import ARD.Color
import ARD.Light
import ARD.Randomize
import ARD.Ray
import ARD.Shape
import ARD.ViewPlane

data World
  = World
  { camera :: Camera
  , viewPlane :: ViewPlane
  , sceneObjects :: [Shape]
  , lights :: [Light]
  , ambientLight :: Light
  , backgroundColor :: Color
  , randomState :: Random
  }

