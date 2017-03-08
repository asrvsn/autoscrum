module AirtableComputation.Scheduler.Features
  ( Features(..) 
  , f_equilateral
  , f_unop
  , f_binop
  , f_nop
  ) where

    
    
data Features = Features {
    f_completed :: Double
  , f_priority :: Double
  } deriving (Show)

instance Num Features where
  (+) = f_binop (+)
  (-) = f_binop (-)
  (*) = f_binop (*)
  abs = f_unop abs
  signum = f_unop signum
  fromInteger = f_equilateral . fromInteger

instance Fractional Features where
  fromRational = f_equilateral . fromRational
  (/) = f_binop (/)

instance Floating Features where
  pi = f_equilateral pi
  exp = f_unop exp
  log = f_unop log
  sqrt = f_unop sqrt
  sin = f_unop sin
  cos = f_unop cos
  asin = f_unop asin
  acos = f_unop acos
  atan = f_unop atan
  sinh = f_unop sinh
  cosh = f_unop cosh
  asinh = f_unop asinh
  acosh = f_unop acosh
  atanh = f_unop atanh

f_equilateral :: Double -> Features
f_equilateral d = Features {
    f_completed = d
  , f_priority = d
  }

f_unop :: (Double -> Double) -> Features -> Features
f_unop func f = Features {
    f_completed = func (f_completed f)
  , f_priority = func (f_priority f)
  }

f_binop :: (Double -> Double -> Double) -> Features -> Features -> Features
f_binop func f1 f2 = Features {
    f_completed = func (f_completed f1) (f_completed f2)
  , f_priority = func (f_priority f1) (f_priority f2)
  }

f_nop :: ([Double] -> Double) -> [Features] -> Features
f_nop func fs = Features {
    f_completed = func (map f_completed fs)
  , f_priority = func (map f_priority fs)
  }