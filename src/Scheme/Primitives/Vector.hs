module Scheme.Primitives.Vector (vectorPrimitives) where

import Scheme.Data
import Scheme.Primitives.Common

vectorPrimitives :: [(String, [SchemeValue] -> ThrowsSchemeError SchemeValue)]
vectorPrimitives = [("vector?", unaryFunction vectorp)]

vectorp (Vector _) = Boolean True
vectorp _          = Boolean False
