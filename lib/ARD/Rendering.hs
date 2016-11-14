module ARD.Rendering where

data RenderContext
  = RenderContext
  { pixelNumber :: !Int
  , rayNumber :: !Int
  , pixelRandom :: !Int
  }

