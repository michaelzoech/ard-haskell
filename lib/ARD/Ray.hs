module ARD.Ray where

import ARD.Vector3

data Ray
  = Ray
  { origin :: Vector3
  , direction :: Vector3
  }

