------------------------------------------------------------------------------
-- |
-- Module      : TTM
-- Copyright   : (C) 2011 Aliaksiej Artamonaŭ
-- License     : LGPL
--
-- Maintainer  : aliaksiej.artamonau@gmail.com
-- Stability   : unstable
-- Portability : unportable
--
-- TTM stands for Type-level Turing Machine. It's an implementation of
-- deterministic Turing machine that works whithin Haskell (at least GHC one)
-- type system. Alphabet contains only two 'zero' and 'one' symbols. Noop is
-- supported additionally to usual left and right movements. Machine state is
-- presented by decimal number (refer to
-- <http://okmij.org/ftp/Haskell/number-parameterized-types.html> for
-- details). Every state for which suitable transition rule can't be found is
-- treated as accepting state.
--
-- Almost certainly you have to increase ghc's context stack size using
-- '-fcontext-stack' flag.
--
-- Several examples can be found in @examples@ directory.
--
------------------------------------------------------------------------------


{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}

module TTM
       (

         -- * Auxilliary
         -- ** Type-level list.
         Cons   ( (:+:) )
       , Nil    ( Nil )

         -- * Machine state.
       , Sz     ( Sz )
       , S0     ( S0 )
       , S1     ( S1 )
       , S2     ( S2 )
       , S3     ( S3 )
       , S4     ( S4 )
       , S5     ( S5 )
       , S6     ( S6 )
       , S7     ( S7 )
       , S8     ( S8 )
       , S9     ( S9 )

         -- * Alphabet.
       , Zero   ( Zero )
       , One    ( One )

         -- * Movements.
       , MLeft  ( MLeft )
       , MRight ( MRight )
       , MNoop  ( MNoop )

         -- * Tape.
       , Tape   ( Tape )

         -- * Transition rule.
       , Rule   ( Rule )

         -- * Execution.
       , exec
       )
       where


-- | Terminates decimal number.
data Sz = Sz
-- | Type-level 0.
data S0 a = S0 a
-- | Type-level 1.
data S1 a = S1 a
-- | Type-level 2.
data S2 a = S2 a
-- | Type-level 3.
data S3 a = S3 a
-- | Type-level 4.
data S4 a = S4 a
-- | Type-level 5.
data S5 a = S5 a
-- | Type-level 6.
data S6 a = S6 a
-- | Type-level 7.
data S7 a = S7 a
-- | Type-level 8.
data S8 a = S8 a
-- | Type-level 9.
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

infixr 5 :+:
-- | Constructs type-level list from its head and tail.
data Cons x xs = x :+: xs

-- | Empty type-level list.
data Nil = Nil

-- | Constructs a tape from a list of cells to the left of the machine's head,
-- one cell under the head and a list of cells to the right of the head.
data Tape l c r = Tape l c r

-- | One.
data One  = One
-- | Zero.
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

-- | Move to the left.
data MLeft  = MLeft
-- | Move to the right.
data MRight = MRight
-- | Do nothing.
data MNoop  = MNoop

class IsMovement m
instance IsMovement MLeft
instance IsMovement MRight
instance IsMovement MNoop

-- | Defines a rule that is triggered when machine's state is @s@ and current
-- symbol equals to @a@. As a result of execution of the rule machine's state
-- is changed to @s'@, @a@ symbol on the tape is replaced by @a'@ and
-- machine's is moved according to @m@.
data Rule s a s' a' m = Rule s a s' a' m

class IsRule r
instance (IsState s, IsSymbol a, IsState s', IsSymbol a', IsMovement m) =>
         IsRule (Rule s a s' a' m)

class IsTransitionTable tbl
instance IsTransitionTable Nil
instance (IsRule r, IsTransitionTable rs) =>
         IsTransitionTable (Cons r rs)

data True
data False

type family Equals s s'

type instance Equals One  One  = True
type instance Equals Zero Zero = True
type instance Equals Zero One  = False
type instance Equals One  Zero = False

type instance Equals Sz      Sz    = True
type instance Equals (x xs)  Sz    = False
type instance Equals Sz     (x xs) = False
type instance Equals (S0 a) (S0 b) = Equals a b
type instance Equals (S1 a) (S1 b) = Equals a b
type instance Equals (S2 a) (S2 b) = Equals a b
type instance Equals (S3 a) (S3 b) = Equals a b
type instance Equals (S4 a) (S4 b) = Equals a b
type instance Equals (S5 a) (S5 b) = Equals a b
type instance Equals (S6 a) (S6 b) = Equals a b
type instance Equals (S7 a) (S7 b) = Equals a b
type instance Equals (S8 a) (S8 b) = Equals a b
type instance Equals (S9 a) (S9 b) = Equals a b
type instance Equals (S0 a) (S1 b) = False
type instance Equals (S0 a) (S2 b) = False
type instance Equals (S0 a) (S3 b) = False
type instance Equals (S0 a) (S4 b) = False
type instance Equals (S0 a) (S5 b) = False
type instance Equals (S0 a) (S6 b) = False
type instance Equals (S0 a) (S7 b) = False
type instance Equals (S0 a) (S8 b) = False
type instance Equals (S0 a) (S9 b) = False
type instance Equals (S1 a) (S0 b) = False
type instance Equals (S1 a) (S2 b) = False
type instance Equals (S1 a) (S3 b) = False
type instance Equals (S1 a) (S4 b) = False
type instance Equals (S1 a) (S5 b) = False
type instance Equals (S1 a) (S6 b) = False
type instance Equals (S1 a) (S7 b) = False
type instance Equals (S1 a) (S8 b) = False
type instance Equals (S1 a) (S9 b) = False
type instance Equals (S2 a) (S0 b) = False
type instance Equals (S2 a) (S1 b) = False
type instance Equals (S2 a) (S3 b) = False
type instance Equals (S2 a) (S4 b) = False
type instance Equals (S2 a) (S5 b) = False
type instance Equals (S2 a) (S6 b) = False
type instance Equals (S2 a) (S7 b) = False
type instance Equals (S2 a) (S8 b) = False
type instance Equals (S2 a) (S9 b) = False
type instance Equals (S3 a) (S0 b) = False
type instance Equals (S3 a) (S1 b) = False
type instance Equals (S3 a) (S2 b) = False
type instance Equals (S3 a) (S4 b) = False
type instance Equals (S3 a) (S5 b) = False
type instance Equals (S3 a) (S6 b) = False
type instance Equals (S3 a) (S7 b) = False
type instance Equals (S3 a) (S8 b) = False
type instance Equals (S3 a) (S9 b) = False
type instance Equals (S4 a) (S0 b) = False
type instance Equals (S4 a) (S1 b) = False
type instance Equals (S4 a) (S2 b) = False
type instance Equals (S4 a) (S3 b) = False
type instance Equals (S4 a) (S5 b) = False
type instance Equals (S4 a) (S6 b) = False
type instance Equals (S4 a) (S7 b) = False
type instance Equals (S4 a) (S8 b) = False
type instance Equals (S4 a) (S9 b) = False
type instance Equals (S5 a) (S0 b) = False
type instance Equals (S5 a) (S1 b) = False
type instance Equals (S5 a) (S2 b) = False
type instance Equals (S5 a) (S3 b) = False
type instance Equals (S5 a) (S4 b) = False
type instance Equals (S5 a) (S6 b) = False
type instance Equals (S5 a) (S7 b) = False
type instance Equals (S5 a) (S8 b) = False
type instance Equals (S5 a) (S9 b) = False
type instance Equals (S6 a) (S0 b) = False
type instance Equals (S6 a) (S1 b) = False
type instance Equals (S6 a) (S2 b) = False
type instance Equals (S6 a) (S3 b) = False
type instance Equals (S6 a) (S4 b) = False
type instance Equals (S6 a) (S5 b) = False
type instance Equals (S6 a) (S7 b) = False
type instance Equals (S6 a) (S8 b) = False
type instance Equals (S6 a) (S9 b) = False
type instance Equals (S7 a) (S0 b) = False
type instance Equals (S7 a) (S1 b) = False
type instance Equals (S7 a) (S2 b) = False
type instance Equals (S7 a) (S3 b) = False
type instance Equals (S7 a) (S4 b) = False
type instance Equals (S7 a) (S5 b) = False
type instance Equals (S7 a) (S6 b) = False
type instance Equals (S7 a) (S8 b) = False
type instance Equals (S7 a) (S9 b) = False
type instance Equals (S8 a) (S0 b) = False
type instance Equals (S8 a) (S1 b) = False
type instance Equals (S8 a) (S2 b) = False
type instance Equals (S8 a) (S3 b) = False
type instance Equals (S8 a) (S4 b) = False
type instance Equals (S8 a) (S5 b) = False
type instance Equals (S8 a) (S6 b) = False
type instance Equals (S8 a) (S7 b) = False
type instance Equals (S8 a) (S9 b) = False
type instance Equals (S9 a) (S0 b) = False
type instance Equals (S9 a) (S1 b) = False
type instance Equals (S9 a) (S2 b) = False
type instance Equals (S9 a) (S3 b) = False
type instance Equals (S9 a) (S4 b) = False
type instance Equals (S9 a) (S5 b) = False
type instance Equals (S9 a) (S6 b) = False
type instance Equals (S9 a) (S7 b) = False
type instance Equals (S9 a) (S8 b) = False

type family Simplify xs
type instance Simplify (Cons Zero Nil)         = Nil
type instance Simplify (Cons Zero (Cons x xs)) = Cons Zero (Cons x xs)
type instance Simplify (Cons One xs)           = Cons One xs

type family MoveLeft t
type instance MoveLeft (Tape Nil c r) =
  Tape Nil Zero (Simplify (Cons c r))
type instance MoveLeft (Tape (Cons l ls) c r) =
  Tape ls l (Simplify (Cons c r))

type family MoveRight t
type instance MoveRight (Tape l c Nil) =
  Tape (Simplify (Cons c l)) Zero Nil
type instance MoveRight (Tape l c (Cons r rs)) =
  Tape (Simplify (Cons c l)) r rs

type family Move d t
type instance Move MNoop  t = t
type instance Move MLeft  t = MoveLeft t
type instance Move MRight t = MoveRight t

type family Write s t
type instance Write One  (Tape l c r) = Tape l One  r
type instance Write Zero (Tape l c r) = Tape l Zero r

type family And x y
type instance And False False = False
type instance And False True  = False
type instance And True  False = False
type instance And True  True  = True

type family IsMatchingRule r s t
type instance IsMatchingRule (Rule rs ra rs' ra' rm) s (Tape tl tc tr) =
  And (Equals rs s) (Equals ra tc)

type family FindRule' p tbl t s
type instance FindRule' True  (Cons r rs) t s = r
type instance FindRule' False (Cons r rs) t s = FindRule rs t s

type family FindRule tbl t s
type instance FindRule Nil t s = False
type instance FindRule (Cons r rs) t s =
  FindRule' (IsMatchingRule r s t) (Cons r rs) t s

type family ExecRule r t
type instance ExecRule False t = False
type instance ExecRule (Rule s a s' a' m) t = (Move m (Write a' t), s')

type family Exec' tbl r t s
type instance Exec' tbl False t s = (t, s)
type instance Exec' tbl (Rule s a s' a' m) t s =
  Exec tbl (ExecRule (Rule s a s' a' m) t)

type family Exec tbl ts
type instance Exec tbl (t, s) = Exec' tbl (FindRule tbl t s) t s

-- | Executes a transition table @tbl@ using @t@ as starting tape. Transition
-- table is a type-level list of transition rules. Machine starts the execution
-- in 'Sz' state. Transformed tape and a final state are returned.
--
-- Example of ghci session (using @Add@ example from @examples@ directory):
--
--  >>> :m + TTM Add
--  >>> :t exec add (Tape Nil Zero (One :+: One :+: Zero :+: One :+: One :+: One :+: Nil))
--  exec add (Tape Nil Zero (One :+: One :+: Zero :+: One :+: One :+: One :+: Nil))
--   :: (Tape
--         (Cons One (Cons One (Cons One (Cons One (Cons One Nil)))))
--         Zero
--         Nil,
--       S1 (S0 Sz))
--
exec :: (Exec tbl (t, Sz) ~ r, IsTape t, IsTransitionTable tbl) => tbl -> t -> r
exec = undefined
