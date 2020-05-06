module Mooc.Todo where

import Control.Exception (Exception, throw)

data TODO = TODO deriving Show
instance Exception TODO
todo = throw TODO
