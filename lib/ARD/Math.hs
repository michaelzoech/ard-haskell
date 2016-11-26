module ARD.Math where

epsilon :: Double
{-# inline epsilon #-}
epsilon = 1.0e-8

radians :: Double -> Double
{-# inline radians #-}
radians deg = deg * (pi / 180.0)

degrees :: Double -> Double
{-# inline degrees #-}
degrees rads = rads * (180.0 / pi)

