module Scheme.Primitives.String (stringp) where

import Scheme.Data

stringp (String _) = Boolean True
stringp _          = Boolean False
