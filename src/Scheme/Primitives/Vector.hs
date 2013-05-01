module Scheme.Primitives.Vector (vectorp) where

import Scheme.Data

vectorp (Vector _) = Boolean True
vectorp _          = Boolean False
