module JoinList where

import           Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

instance Sized Int where
  size = Size

-- indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
-- indexJ _ Empty            = Nothing
-- indexJ i (Single j x)
--   | i == j = Just x
--   | otherwise             = Nothing
-- indexJ i (Append s l1 l2)
--   | i < s = indexJ i l1
--   | otherwise            = indexJ i l2
