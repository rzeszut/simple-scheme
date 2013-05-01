module Scheme.Primitives.Char (charp) where

import Scheme.Data

charp (Char _) = Boolean True
charp _        = Boolean False
