{-# LANGUAGE TypeOperators #-}

data (f :+: g) e = Inl (f e) | Inr (g e)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap h (Inl x) = Inl $ fmap h x
    fmap h (Inr x) = Inr $ fmap h x


let x = Inl [1] :: ([] :+: Maybe)