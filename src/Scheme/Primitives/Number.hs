module Scheme.Primitives.Number ( numberp
                                , complexp
                                , realp
                                , rationalp
                                , integerp
                                ) where

import Scheme.Data

numberp (Integer  _) = Boolean True
numberp (Float    _) = Boolean True
numberp (Rational _) = Boolean True
numberp (Complex  _) = Boolean True
numberp _            = Boolean False

complexp (Complex _) = Boolean True
complexp _           = Boolean False

realp (Float _) = Boolean True
realp _         = Boolean False

rationalp (Rational _) = Boolean True
rationalp _            = Boolean False

integerp (Integer _) = Boolean True
integerp _           = Boolean False
