module ARD.Ray where

import ARD.Vector

data Ray
  = Ray
  { origin :: Point3
  , direction :: Vector3
  }

