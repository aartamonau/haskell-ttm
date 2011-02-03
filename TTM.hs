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

class Equals x y r | x y -> r where
  equals :: x -> y -> r
  equals = undefined

instance Equals One  One  True
instance Equals One  Zero False
instance Equals Zero One  False
instance Equals Zero Zero True

data MLeft  = MLeft
data MRight = MRight
data MNoop  = MNoop

class MoveLeft' t n t' | t n -> t'
instance MoveLeft' (Tape Nil c r) True (Tape Nil Zero (Cons c r))
instance (Tail l t, Head l h) =>
         MoveLeft' (Tape l c r) False (Tape t h (Cons c r))

class MoveRight' t n t' | t n -> t'
instance MoveRight' (Tape l c r) True (Tape (Cons c l) Zero Nil)
instance (Tail r t, Head r h) =>
         MoveRight' (Tape l c r) False (Tape (Cons c l) h t)

class MoveLeft t t' | t -> t' where
  moveLeft :: t -> t'
  moveLeft = undefined

instance (IsNull r n, Right t r, MoveLeft' t n t') => MoveLeft t t'

class MoveRight t t' | t -> t' where
  moveRight :: t -> t'
  moveRight = undefined

instance (IsNull r n, Left t r, MoveRight' t n t') => MoveRight t t'

class Move m t t' | m t -> t' where
  move :: m -> t -> t'
  move = undefined

instance MoveLeft  t t' => Move MLeft  t t'
instance MoveRight t t' => Move MRight t t'
instance Move MNoop t t

class Write a t t' | a t -> t' where
  write :: a -> t -> t'
  write = undefined

instance Write Zero (Tape l c r) (Tape l Zero r)
instance Write One  (Tape l c r) (Tape l One r)

data Sz = Sz
data S0 a = S0 a
data S1 a = S1 a
data S2 a = S2 a
data S3 a = S3 a
data S4 a = S4 a
data S5 a = S5 a
data S6 a = S6 a
data S7 a = S7 a
data S8 a = S8 a
data S9 a = S9 a

class State' s
instance State' Sz
instance State' xs => State' (S0 xs)
instance State' xs => State' (S1 xs)
instance State' xs => State' (S2 xs)
instance State' xs => State' (S3 xs)
instance State' xs => State' (S4 xs)
instance State' xs => State' (S5 xs)
instance State' xs => State' (S6 xs)
instance State' xs => State' (S7 xs)
instance State' xs => State' (S8 xs)
instance State' xs => State' (S9 xs)

class State' s => State s
instance State Sz
instance State' xs => State (S1 xs)
instance State' xs => State (S2 xs)
instance State' xs => State (S3 xs)
instance State' xs => State (S4 xs)
instance State' xs => State (S5 xs)
instance State' xs => State (S6 xs)
instance State' xs => State (S7 xs)
instance State' xs => State (S8 xs)
instance State' xs => State (S9 xs)

instance Equals Sz Sz True
instance State (x xs) => Equals Sz (x xs) False
instance State (x xs) => Equals (x xs) Sz False
instance Equals xs ys r => Equals (S0 xs) (S0 ys) r
instance Equals xs ys r => Equals (S1 xs) (S1 ys) r
instance Equals xs ys r => Equals (S2 xs) (S2 ys) r
instance Equals xs ys r => Equals (S3 xs) (S3 ys) r
instance Equals xs ys r => Equals (S4 xs) (S4 ys) r
instance Equals xs ys r => Equals (S5 xs) (S5 ys) r
instance Equals xs ys r => Equals (S6 xs) (S6 ys) r
instance Equals xs ys r => Equals (S7 xs) (S7 ys) r
instance Equals xs ys r => Equals (S8 xs) (S8 ys) r
instance Equals xs ys r => Equals (S9 xs) (S9 ys) r
instance Equals (S0 xs) (S1 ys) False
instance Equals (S0 xs) (S2 ys) False
instance Equals (S0 xs) (S3 ys) False
instance Equals (S0 xs) (S4 ys) False
instance Equals (S0 xs) (S5 ys) False
instance Equals (S0 xs) (S6 ys) False
instance Equals (S0 xs) (S7 ys) False
instance Equals (S0 xs) (S8 ys) False
instance Equals (S0 xs) (S9 ys) False
instance Equals (S1 xs) (S0 ys) False
instance Equals (S1 xs) (S2 ys) False
instance Equals (S1 xs) (S3 ys) False
instance Equals (S1 xs) (S4 ys) False
instance Equals (S1 xs) (S5 ys) False
instance Equals (S1 xs) (S6 ys) False
instance Equals (S1 xs) (S7 ys) False
instance Equals (S1 xs) (S8 ys) False
instance Equals (S1 xs) (S9 ys) False
instance Equals (S2 xs) (S0 ys) False
instance Equals (S2 xs) (S1 ys) False
instance Equals (S2 xs) (S3 ys) False
instance Equals (S2 xs) (S4 ys) False
instance Equals (S2 xs) (S5 ys) False
instance Equals (S2 xs) (S6 ys) False
instance Equals (S2 xs) (S7 ys) False
instance Equals (S2 xs) (S8 ys) False
instance Equals (S2 xs) (S9 ys) False
instance Equals (S3 xs) (S0 ys) False
instance Equals (S3 xs) (S1 ys) False
instance Equals (S3 xs) (S2 ys) False
instance Equals (S3 xs) (S4 ys) False
instance Equals (S3 xs) (S5 ys) False
instance Equals (S3 xs) (S6 ys) False
instance Equals (S3 xs) (S7 ys) False
instance Equals (S3 xs) (S8 ys) False
instance Equals (S3 xs) (S9 ys) False
instance Equals (S4 xs) (S0 ys) False
instance Equals (S4 xs) (S1 ys) False
instance Equals (S4 xs) (S2 ys) False
instance Equals (S4 xs) (S3 ys) False
instance Equals (S4 xs) (S5 ys) False
instance Equals (S4 xs) (S6 ys) False
instance Equals (S4 xs) (S7 ys) False
instance Equals (S4 xs) (S8 ys) False
instance Equals (S4 xs) (S9 ys) False
instance Equals (S5 xs) (S0 ys) False
instance Equals (S5 xs) (S1 ys) False
instance Equals (S5 xs) (S2 ys) False
instance Equals (S5 xs) (S3 ys) False
instance Equals (S5 xs) (S4 ys) False
instance Equals (S5 xs) (S6 ys) False
instance Equals (S5 xs) (S7 ys) False
instance Equals (S5 xs) (S8 ys) False
instance Equals (S5 xs) (S9 ys) False
instance Equals (S6 xs) (S0 ys) False
instance Equals (S6 xs) (S1 ys) False
instance Equals (S6 xs) (S2 ys) False
instance Equals (S6 xs) (S3 ys) False
instance Equals (S6 xs) (S4 ys) False
instance Equals (S6 xs) (S5 ys) False
instance Equals (S6 xs) (S7 ys) False
instance Equals (S6 xs) (S8 ys) False
instance Equals (S6 xs) (S9 ys) False
instance Equals (S7 xs) (S0 ys) False
instance Equals (S7 xs) (S1 ys) False
instance Equals (S7 xs) (S2 ys) False
instance Equals (S7 xs) (S3 ys) False
instance Equals (S7 xs) (S4 ys) False
instance Equals (S7 xs) (S5 ys) False
instance Equals (S7 xs) (S6 ys) False
instance Equals (S7 xs) (S8 ys) False
instance Equals (S7 xs) (S9 ys) False
instance Equals (S8 xs) (S0 ys) False
instance Equals (S8 xs) (S1 ys) False
instance Equals (S8 xs) (S2 ys) False
instance Equals (S8 xs) (S3 ys) False
instance Equals (S8 xs) (S4 ys) False
instance Equals (S8 xs) (S5 ys) False
instance Equals (S8 xs) (S6 ys) False
instance Equals (S8 xs) (S7 ys) False
instance Equals (S8 xs) (S9 ys) False
instance Equals (S9 xs) (S0 ys) False
instance Equals (S9 xs) (S1 ys) False
instance Equals (S9 xs) (S2 ys) False
instance Equals (S9 xs) (S3 ys) False
instance Equals (S9 xs) (S4 ys) False
instance Equals (S9 xs) (S5 ys) False
instance Equals (S9 xs) (S6 ys) False
instance Equals (S9 xs) (S7 ys) False
instance Equals (S9 xs) (S8 ys) False


data Rule s a a' s' m = Rule s a a' s' m

class And x y r | x y -> r where
  tAnd :: x -> y -> r
  tAnd = undefined

instance And True  True  True
instance And True  False False
instance And False True  False
instance And False False False

class IsMatchingRule r s t b | r s t -> b where
  isMatchingRule :: r -> s -> t -> b
  isMatchingRule = undefined

instance (Equals rs s r1, Equals ra tc r2, And r1 r2 r) =>
          IsMatchingRule (Rule rs ra rs' ra' rm) s (Tape tl tc tr) r

class FindRule' tbl t s eq r | tbl t s eq -> r
instance FindRule' (Cons r rs) t s True r
instance FindRule rs t s res => FindRule' (Cons r rs) t s False res

class FindRule tbl t s r | tbl t s -> r where
  findRule :: tbl -> t -> s -> r
  findRule = undefined

instance FindRule Nil t s False
instance (IsMatchingRule r s t rm, FindRule' (Cons r rs) t s rm res) =>
         FindRule (Cons r rs) t s res
