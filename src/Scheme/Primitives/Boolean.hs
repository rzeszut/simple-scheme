module Scheme.Primitives.Boolean where

import Scheme.Data

not' (Boolean True)  = Boolean False
not' (Boolean False) = Boolean True
not' _               = Boolean False

booleanp (Boolean _) = Boolean True
booleanp _           = Boolean False

