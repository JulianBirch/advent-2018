module Recursion where
    
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo f g = h where h = f . fmap h . g

hyloM :: (Traversable g, Monad m) => (g b -> m b) -> (a -> m (g a)) -> a -> m b
hyloM f g = h where h = f <=< traverse h <=< g

data TD f a = TD (f a)

instance (Functor f) => Traversable (TD f a) where
    sequenceA :: Applicative f => TD g (ap a) -> ap (TD g a)

-- hylo' :: Functor f => m (f b -> b) -> m (a -> f a) -> m a -> m b

func :: (Traversable g, Monad m) => (a -> m (g a))
func = undefined

func2 :: (Traversable g, Monad m) => (g a -> m b)
func2 = undefined

{-

newtype Compose f g a = Compose (f (g a))



hyloM2 :: (Traversable g, Monad m) => (g b -> m b) -> (a -> m (g a)) -> a -> m b
hyloM2 f g = hylo f' g'
  where g' :: m (a -> g a)
  -}