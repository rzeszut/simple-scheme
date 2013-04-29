module Scheme.Primitives.String where

import Scheme.Data

stringp (String _) = Boolean True
stringp _          = Boolean False
