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

class IsSymbol s
instance IsSymbol One
instance IsSymbol Zero

class IsSymbolList xs
instance IsSymbolList Nil
instance (IsSymbolList xs, IsSymbol x) => IsSymbolList (Cons x xs)

class IsTape t
instance (IsSymbolList l, IsSymbolList r) => IsTape (Tape l One  r)
instance (IsSymbolList l, IsSymbolList r) => IsTape (Tape l Zero r)

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

class IsMovement m
instance IsMovement MLeft
instance IsMovement MRight
instance IsMovement MNoop

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

class IsState' s
instance IsState' Sz
instance IsState' xs => IsState' (S0 xs)
instance IsState' xs => IsState' (S1 xs)
instance IsState' xs => IsState' (S2 xs)
instance IsState' xs => IsState' (S3 xs)
instance IsState' xs => IsState' (S4 xs)
instance IsState' xs => IsState' (S5 xs)
instance IsState' xs => IsState' (S6 xs)
instance IsState' xs => IsState' (S7 xs)
instance IsState' xs => IsState' (S8 xs)
instance IsState' xs => IsState' (S9 xs)

class IsState' s => IsState s
instance IsState Sz
instance IsState' xs => IsState (S1 xs)
instance IsState' xs => IsState (S2 xs)
instance IsState' xs => IsState (S3 xs)
instance IsState' xs => IsState (S4 xs)
instance IsState' xs => IsState (S5 xs)
instance IsState' xs => IsState (S6 xs)
instance IsState' xs => IsState (S7 xs)
instance IsState' xs => IsState (S8 xs)
instance IsState' xs => IsState (S9 xs)

instance Equals Sz Sz True
instance IsState' (x xs) => Equals Sz (x xs) False
instance IsState' (x xs) => Equals (x xs) Sz False
instance (IsState' xs, IsState' ys, Equals xs ys r) => Equals (S0 xs) (S0 ys) r
instance (IsState' xs, IsState' ys, Equals xs ys r) => Equals (S1 xs) (S1 ys) r
instance (IsState' xs, IsState' ys, Equals xs ys r) => Equals (S2 xs) (S2 ys) r
instance (IsState' xs, IsState' ys, Equals xs ys r) => Equals (S3 xs) (S3 ys) r
instance (IsState' xs, IsState' ys, Equals xs ys r) => Equals (S4 xs) (S4 ys) r
instance (IsState' xs, IsState' ys, Equals xs ys r) => Equals (S5 xs) (S5 ys) r
instance (IsState' xs, IsState' ys, Equals xs ys r) => Equals (S6 xs) (S6 ys) r
instance (IsState' xs, IsState' ys, Equals xs ys r) => Equals (S7 xs) (S7 ys) r
instance (IsState' xs, IsState' ys, Equals xs ys r) => Equals (S8 xs) (S8 ys) r
instance (IsState' xs, IsState' ys, Equals xs ys r) => Equals (S9 xs) (S9 ys) r
instance (IsState' xs, IsState' ys) => Equals (S0 xs) (S1 ys) False
instance (IsState' xs, IsState' ys) => Equals (S0 xs) (S2 ys) False
instance (IsState' xs, IsState' ys) => Equals (S0 xs) (S3 ys) False
instance (IsState' xs, IsState' ys) => Equals (S0 xs) (S4 ys) False
instance (IsState' xs, IsState' ys) => Equals (S0 xs) (S5 ys) False
instance (IsState' xs, IsState' ys) => Equals (S0 xs) (S6 ys) False
instance (IsState' xs, IsState' ys) => Equals (S0 xs) (S7 ys) False
instance (IsState' xs, IsState' ys) => Equals (S0 xs) (S8 ys) False
instance (IsState' xs, IsState' ys) => Equals (S0 xs) (S9 ys) False
instance (IsState' xs, IsState' ys) => Equals (S1 xs) (S0 ys) False
instance (IsState' xs, IsState' ys) => Equals (S1 xs) (S2 ys) False
instance (IsState' xs, IsState' ys) => Equals (S1 xs) (S3 ys) False
instance (IsState' xs, IsState' ys) => Equals (S1 xs) (S4 ys) False
instance (IsState' xs, IsState' ys) => Equals (S1 xs) (S5 ys) False
instance (IsState' xs, IsState' ys) => Equals (S1 xs) (S6 ys) False
instance (IsState' xs, IsState' ys) => Equals (S1 xs) (S7 ys) False
instance (IsState' xs, IsState' ys) => Equals (S1 xs) (S8 ys) False
instance (IsState' xs, IsState' ys) => Equals (S1 xs) (S9 ys) False
instance (IsState' xs, IsState' ys) => Equals (S2 xs) (S0 ys) False
instance (IsState' xs, IsState' ys) => Equals (S2 xs) (S1 ys) False
instance (IsState' xs, IsState' ys) => Equals (S2 xs) (S3 ys) False
instance (IsState' xs, IsState' ys) => Equals (S2 xs) (S4 ys) False
instance (IsState' xs, IsState' ys) => Equals (S2 xs) (S5 ys) False
instance (IsState' xs, IsState' ys) => Equals (S2 xs) (S6 ys) False
instance (IsState' xs, IsState' ys) => Equals (S2 xs) (S7 ys) False
instance (IsState' xs, IsState' ys) => Equals (S2 xs) (S8 ys) False
instance (IsState' xs, IsState' ys) => Equals (S2 xs) (S9 ys) False
instance (IsState' xs, IsState' ys) => Equals (S3 xs) (S0 ys) False
instance (IsState' xs, IsState' ys) => Equals (S3 xs) (S1 ys) False
instance (IsState' xs, IsState' ys) => Equals (S3 xs) (S2 ys) False
instance (IsState' xs, IsState' ys) => Equals (S3 xs) (S4 ys) False
instance (IsState' xs, IsState' ys) => Equals (S3 xs) (S5 ys) False
instance (IsState' xs, IsState' ys) => Equals (S3 xs) (S6 ys) False
instance (IsState' xs, IsState' ys) => Equals (S3 xs) (S7 ys) False
instance (IsState' xs, IsState' ys) => Equals (S3 xs) (S8 ys) False
instance (IsState' xs, IsState' ys) => Equals (S3 xs) (S9 ys) False
instance (IsState' xs, IsState' ys) => Equals (S4 xs) (S0 ys) False
instance (IsState' xs, IsState' ys) => Equals (S4 xs) (S1 ys) False
instance (IsState' xs, IsState' ys) => Equals (S4 xs) (S2 ys) False
instance (IsState' xs, IsState' ys) => Equals (S4 xs) (S3 ys) False
instance (IsState' xs, IsState' ys) => Equals (S4 xs) (S5 ys) False
instance (IsState' xs, IsState' ys) => Equals (S4 xs) (S6 ys) False
instance (IsState' xs, IsState' ys) => Equals (S4 xs) (S7 ys) False
instance (IsState' xs, IsState' ys) => Equals (S4 xs) (S8 ys) False
instance (IsState' xs, IsState' ys) => Equals (S4 xs) (S9 ys) False
instance (IsState' xs, IsState' ys) => Equals (S5 xs) (S0 ys) False
instance (IsState' xs, IsState' ys) => Equals (S5 xs) (S1 ys) False
instance (IsState' xs, IsState' ys) => Equals (S5 xs) (S2 ys) False
instance (IsState' xs, IsState' ys) => Equals (S5 xs) (S3 ys) False
instance (IsState' xs, IsState' ys) => Equals (S5 xs) (S4 ys) False
instance (IsState' xs, IsState' ys) => Equals (S5 xs) (S6 ys) False
instance (IsState' xs, IsState' ys) => Equals (S5 xs) (S7 ys) False
instance (IsState' xs, IsState' ys) => Equals (S5 xs) (S8 ys) False
instance (IsState' xs, IsState' ys) => Equals (S5 xs) (S9 ys) False
instance (IsState' xs, IsState' ys) => Equals (S6 xs) (S0 ys) False
instance (IsState' xs, IsState' ys) => Equals (S6 xs) (S1 ys) False
instance (IsState' xs, IsState' ys) => Equals (S6 xs) (S2 ys) False
instance (IsState' xs, IsState' ys) => Equals (S6 xs) (S3 ys) False
instance (IsState' xs, IsState' ys) => Equals (S6 xs) (S4 ys) False
instance (IsState' xs, IsState' ys) => Equals (S6 xs) (S5 ys) False
instance (IsState' xs, IsState' ys) => Equals (S6 xs) (S7 ys) False
instance (IsState' xs, IsState' ys) => Equals (S6 xs) (S8 ys) False
instance (IsState' xs, IsState' ys) => Equals (S6 xs) (S9 ys) False
instance (IsState' xs, IsState' ys) => Equals (S7 xs) (S0 ys) False
instance (IsState' xs, IsState' ys) => Equals (S7 xs) (S1 ys) False
instance (IsState' xs, IsState' ys) => Equals (S7 xs) (S2 ys) False
instance (IsState' xs, IsState' ys) => Equals (S7 xs) (S3 ys) False
instance (IsState' xs, IsState' ys) => Equals (S7 xs) (S4 ys) False
instance (IsState' xs, IsState' ys) => Equals (S7 xs) (S5 ys) False
instance (IsState' xs, IsState' ys) => Equals (S7 xs) (S6 ys) False
instance (IsState' xs, IsState' ys) => Equals (S7 xs) (S8 ys) False
instance (IsState' xs, IsState' ys) => Equals (S7 xs) (S9 ys) False
instance (IsState' xs, IsState' ys) => Equals (S8 xs) (S0 ys) False
instance (IsState' xs, IsState' ys) => Equals (S8 xs) (S1 ys) False
instance (IsState' xs, IsState' ys) => Equals (S8 xs) (S2 ys) False
instance (IsState' xs, IsState' ys) => Equals (S8 xs) (S3 ys) False
instance (IsState' xs, IsState' ys) => Equals (S8 xs) (S4 ys) False
instance (IsState' xs, IsState' ys) => Equals (S8 xs) (S5 ys) False
instance (IsState' xs, IsState' ys) => Equals (S8 xs) (S6 ys) False
instance (IsState' xs, IsState' ys) => Equals (S8 xs) (S7 ys) False
instance (IsState' xs, IsState' ys) => Equals (S8 xs) (S9 ys) False
instance (IsState' xs, IsState' ys) => Equals (S9 xs) (S0 ys) False
instance (IsState' xs, IsState' ys) => Equals (S9 xs) (S1 ys) False
instance (IsState' xs, IsState' ys) => Equals (S9 xs) (S2 ys) False
instance (IsState' xs, IsState' ys) => Equals (S9 xs) (S3 ys) False
instance (IsState' xs, IsState' ys) => Equals (S9 xs) (S4 ys) False
instance (IsState' xs, IsState' ys) => Equals (S9 xs) (S5 ys) False
instance (IsState' xs, IsState' ys) => Equals (S9 xs) (S6 ys) False
instance (IsState' xs, IsState' ys) => Equals (S9 xs) (S7 ys) False
instance (IsState' xs, IsState' ys) => Equals (S9 xs) (S8 ys) False


data Rule s a s' a' m = Rule s a s' a' m

class IsRule r
instance (IsState s, IsSymbol a, IsState s', IsSymbol a', IsMovement m) =>
         IsRule (Rule s a s' a' m)

class IsTransitionTable tbl
instance IsTransitionTable Nil
instance (IsRule r, IsTransitionTable rs) =>
         IsTransitionTable (Cons r rs)

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

class ExecRule r t s t' s' | r t s -> t' s' where
  execRule :: r -> t -> s -> (t', s')
  execRule = undefined

instance ExecRule False t s False False
instance Move m (Tape l a' r) t' =>
         ExecRule (Rule s a s' a' m) (Tape l a r) s t' s'

class ExecStep tbl t s t' s' | tbl t s -> t' s' where
  execStep :: tbl -> t -> s -> (t', s')
  execStep = undefined

instance (FindRule tbl t s r, ExecRule r t s t' s') =>
         ExecStep tbl t s t' s'

class Exec' tbl tn sn t s t' s' | tbl tn sn t s -> t' s' where

instance Exec' tbl False False t s t s
instance (ExecStep tbl (Tape tnl tnc tnr) sn tn' sn',
          Exec' table tn' sn' (Tape tnl tnc tnr) sn t' s') =>
         Exec' tbl (Tape tnl tnc tnr) sn t s t' s'

class Exec tbl t t' s' | tbl t -> t' s' where
  exec :: tbl -> t -> (t', s')
  exec = undefined

instance (IsTape t, IsTransitionTable tbl,
          ExecStep tbl t Sz t' s',
          Exec' tbl t' s' t Sz rt rs) =>
         Exec tbl t rt rs
