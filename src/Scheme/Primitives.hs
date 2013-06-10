module Scheme.Primitives (primitiveBindings) where

import Scheme.Data
import Scheme.Primitives.Equal
import Scheme.Primitives.Boolean
import Scheme.Primitives.Char
import Scheme.Primitives.List
import Scheme.Primitives.Number
import Scheme.Primitives.String
import Scheme.Primitives.Symbol
import Scheme.Primitives.Vector
import Scheme.Primitives.Control
import Scheme.Primitives.IO
import Scheme.Environment

primitiveBindings :: IO SchemeEnvironment
primitiveBindings = nullEnvironment >>= (flip bindVars $ concat
                                         [ (map (makeNativeFunc NativeFunction) equalPrimitives)
                                         , (map (makeNativeFunc NativeFunction) booleanPrimitives)
                                         , (map (makeNativeFunc NativeFunction) charPrimitives)
                                         , (map (makeNativeFunc NativeFunction) listPrimitives)
                                         , (map (makeNativeFunc NativeFunction) numberPrimitives)
                                         , (map (makeNativeFunc NativeFunction) stringPrimitives)
                                         , (map (makeNativeFunc NativeFunction) symbolPrimitives)
                                         , (map (makeNativeFunc NativeFunction) vectorPrimitives)
                                         , (map (makeNativeFunc IONativeFunction) controlPrimitives)
                                         , (map (makeNativeFunc IONativeFunction) ioPrimitives)
                                         ])
  where
    makeNativeFunc constructor (var, fun) = (var, constructor fun)
