module ARD.Ray where

import ARD.Vector3

data Ray
  = Ray
  { origin :: Point3
  , direction :: Vector3
  }

