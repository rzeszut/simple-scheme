module Scheme.Primitives.Char where

import Scheme.Data

charp (Char _) = Boolean True
charp _        = Boolean False
