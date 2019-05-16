module Data.Tuple.Extra where

tuple2
  :: a1 -> a2
  -> (a1,a2)
tuple2 a1 a2 =
  (a1,a2)

tuple3
  :: a1 -> a2 -> a3
  -> (a1,a2,a3)
tuple3 a1 a2 a3 =
  (a1,a2,a3)

curry3
  :: ((a1,a2,a3) -> b)
  -> a1 -> a2 -> a3 -> b
curry3 f a1 a2 a3 = f (a1,a2,a3)

tuple4
  :: a1 -> a2 -> a3 -> a4
  -> (a1,a2,a3,a4)
tuple4 a1 a2 a3 a4 =
  (a1,a2,a3,a4)

curry4
  :: ((a1,a2,a3,a4) -> b)
  -> a1 -> a2 -> a3 -> a4 -> b
curry4 f a1 a2 a3 a4 = f (a1,a2,a3,a4)

tuple5
  :: a1 -> a2 -> a3 -> a4 -> a5
  -> (a1,a2,a3,a4,a5)
tuple5 a1 a2 a3 a4 a5 =
  (a1,a2,a3,a4,a5)

curry5
  :: ((a1,a2,a3,a4,a5) -> b)
  -> a1 -> a2 -> a3 -> a4 -> a5 -> b
curry5 f a1 a2 a3 a4 a5 =
  f (a1,a2,a3,a4,a5)

tuple6
  :: a1 -> a2 -> a3 -> a4 -> a5 -> a6
  -> (a1,a2,a3,a4,a5,a6)
tuple6 a1 a2 a3 a4 a5 a6 =
  (a1,a2,a3,a4,a5,a6)

curry6
  :: ((a1,a2,a3,a4,a5,a6) -> b)
  -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b
curry6 f a1 a2 a3 a4 a5 a6 =
  f (a1,a2,a3,a4,a5,a6)

tuple7
  :: a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7
  -> (a1,a2,a3,a4,a5,a6,a7)
tuple7 a1 a2 a3 a4 a5 a6 a7 =
  (a1,a2,a3,a4,a5,a6,a7)

curry7
  :: ((a1,a2,a3,a4,a5,a6,a7) -> b)
  -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b
curry7 f a1 a2 a3 a4 a5 a6 a7 =
  f (a1,a2,a3,a4,a5,a6,a7)

tuple8
  :: a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8
  -> (a1,a2,a3,a4,a5,a6,a7,a8)
tuple8 a1 a2 a3 a4 a5 a6 a7 a8 =
  (a1,a2,a3,a4,a5,a6,a7,a8)

curry8
  :: ((a1,a2,a3,a4,a5,a6,a7,a8) -> b)
  -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> b
curry8 f a1 a2 a3 a4 a5 a6 a7 a8 =
  f (a1,a2,a3,a4,a5,a6,a7,a8)

tuple9
  :: a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9
  -> (a1,a2,a3,a4,a5,a6,a7,a8,a9)
tuple9 a1 a2 a3 a4 a5 a6 a7 a8 a9 =
  (a1,a2,a3,a4,a5,a6,a7,a8,a9)

curry9
  :: ((a1,a2,a3,a4,a5,a6,a7,a8,a9) -> b)
  -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> b
curry9 f a1 a2 a3 a4 a5 a6 a7 a8 a9 =
  f (a1,a2,a3,a4,a5,a6,a7,a8,a9)

tuple10
  :: a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10
  -> (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)
tuple10 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
  (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)

curry10
  :: ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10) -> b)
  -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> b
curry10 f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 =
  f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)

curry11
  :: ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11) -> b)
  -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> b
curry11 f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 =
  f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11)

curry12
  :: ((a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12) -> b)
  -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> a8 -> a9 -> a10 -> a11 -> a12 -> b
curry12 f a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 =
  f (a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)