module PHP.Parse.Render where

import qualified Data.Text.Lazy as T

class Render t where
  render :: t -> T.Text





instance
  ( Render a
  ) => Render (Maybe a)
  where
    render x = case x of
      Nothing -> T.empty
      Just a  -> render a

instance
  ( Render a0, Render a1
  ) => Render (a0,a1)
  where
    render (a0,a1) = T.concat
      [ render a0, render a1 ]

instance
  ( Render a0, Render a1, Render a2
  ) => Render (a0,a1,a2)
  where
    render (a0,a1,a2) = T.concat
      [ render a0, render a1, render a2 ]

instance
  ( Render a0, Render a1, Render a2, Render a3
  ) => Render (a0,a1,a2,a3)
  where
    render (a0,a1,a2,a3) = T.concat
      [ render a0, render a1, render a2, render a3 ]

instance
  ( Render a0, Render a1, Render a2, Render a3, Render a4
  ) => Render (a0,a1,a2,a3,a4)
  where
    render (a0,a1,a2,a3,a4) = T.concat
      [ render a0, render a1, render a2, render a3, render a4 ]

instance
  ( Render a0, Render a1, Render a2, Render a3, Render a4
  , Render a5
  ) => Render (a0,a1,a2,a3,a4,a5)
  where
    render (a0,a1,a2,a3,a4,a5) = T.concat
      [ render a0, render a1, render a2, render a3, render a4
      , render a5 ]

instance
  ( Render a0, Render a1, Render a2, Render a3, Render a4
  , Render a5, Render a6
  ) => Render (a0,a1,a2,a3,a4,a5,a6)
  where
    render (a0,a1,a2,a3,a4,a5,a6) = T.concat
      [ render a0, render a1, render a2, render a3, render a4
      , render a5, render a6 ]

instance
  ( Render a0, Render a1, Render a2, Render a3, Render a4
  , Render a5, Render a6, Render a7
  ) => Render (a0,a1,a2,a3,a4,a5,a6,a7)
  where
    render (a0,a1,a2,a3,a4,a5,a6,a7) = T.concat
      [ render a0, render a1, render a2, render a3, render a4
      , render a5, render a6, render a7 ]

instance
  ( Render a0, Render a1, Render a2, Render a3, Render a4
  , Render a5, Render a6, Render a7, Render a8
  ) => Render (a0,a1,a2,a3,a4,a5,a6,a7,a8)
  where
    render (a0,a1,a2,a3,a4,a5,a6,a7,a8) = T.concat
      [ render a0, render a1, render a2, render a3, render a4
      , render a5, render a6, render a7, render a8 ]

instance
  ( Render a0, Render a1, Render a2, Render a3, Render a4
  , Render a5, Render a6, Render a7, Render a8, Render a9
  ) => Render (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9)
  where
    render (a0,a1,a2,a3,a4,a5,a6,a7,a8,a9) = T.concat
      [ render a0, render a1, render a2, render a3, render a4
      , render a5, render a6, render a7, render a8, render a9 ]
