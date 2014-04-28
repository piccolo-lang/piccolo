module Utils.Location where

data Location = Location { locOffset      :: !Int
                         , locStartLine   :: !Int
                         , locStartColumn :: !Int
                         , locEndLine     :: !Int
                         , locEndColumn   :: !Int
                         } deriving (Eq, Show)
