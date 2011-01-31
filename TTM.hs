{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

infixr 5 :+:
data Cons x xs = x :+: xs
data Nil = Nil

data True
data False

class IsNull xs r | xs -> r
instance IsNull Nil True
instance IsNull (Cons x xs) False

class Head xs h | xs -> h
instance Head (Cons x xs) x

class Tail xs t | xs -> t
instance Tail (Cons x xs) xs

data Tape l c r = Tape l c r

class Left t l | t -> l
instance Left (Tape l c r) l

class Right t r | t -> r
instance Right (Tape l c r) r

data One  = One
data Zero = Zero

class MoveLeft' t n t' | t n -> t'
instance MoveLeft' (Tape l c r) True (Tape (Cons c l) Zero Nil)
instance (Tail r t, Head r h) =>
         MoveLeft' (Tape l c r) False (Tape (Cons c l) h t)

class MoveRight' t n t' | t n -> t'
instance MoveRight' (Tape Nil c r) True (Tape Nil Zero (Cons c r))
instance (Tail l t, Head l h) =>
         MoveRight' (Tape l c r) False (Tape t h (Cons c r))

class MoveLeft t t' | t -> t' where
  moveLeft :: t -> t'
  moveLeft = undefined

instance (IsNull r n, Right t r, MoveLeft' t n t') => MoveLeft t t'

class MoveRight t t' | t -> t' where
  moveRight :: t -> t'
  moveRight = undefined

instance (IsNull r n, Left t r, MoveRight' t n t') => MoveRight t t'

class Write a t t' | a t -> t' where
  write :: a -> t -> t'
  write = undefined

instance Write Zero (Tape l c r) (Tape l Zero r)
instance Write One  (Tape l c r) (Tape l One r)
