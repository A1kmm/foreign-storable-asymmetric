{-# LANGUAGE ScopedTypeVariables #-}
module Foreign.Storable.Asymmetric
where
  
import Foreign.Storable
import Foreign.Ptr
import Control.Applicative

data NoData = NoData
instance Storable NoData where
  sizeOf _ = 0
  alignment _ = 1
  peek _ = return NoData
  poke _ _ = return ()

data DifferentPeekPoke a b = PeekOut b | PokeIn a
getPeek (PeekOut b) = b
getPeek (PokeIn a) = error "Attempt to call getPeek on poke input"

instance (Storable a, Storable b) => Storable (DifferentPeekPoke a b) where
  sizeOf _ = max (sizeOf (undefined :: a)) (sizeOf (undefined :: b))
  alignment _ = max (alignment (undefined :: a)) (alignment (undefined :: b))
  peek ptr = liftA PeekOut $ peek (castPtr ptr)
  poke ptr (PokeIn a) = poke (castPtr ptr) a
  poke ptr (PeekOut b) = poke (castPtr ptr) b
