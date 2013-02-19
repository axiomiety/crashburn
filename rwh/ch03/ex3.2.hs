-- file: ch03/ex3.2.hs
data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
              deriving (Show)

root = Node "parent" Nothing Nothing
leftTree = Node "parent" (Just (Node "left" Nothing Nothing)) Nothing
simpleTree = Node "parent" (Just $ Node "left child" Nothing Nothing)
                           (Just $ Node "right child" Nothing Nothing)
