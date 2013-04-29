module Scheme.Primitives.Vector where

import Scheme.Data

vectorp (Vector _) = Boolean True
vectorp _          = Boolean False
