------------------------------------------------------------------------------
-- |
-- Module      : Even
-- Copyright   : (C) 2011 Aliaksiej Artamonaŭ
-- License     : LGPL
--
-- Maintainer  : aliaksiej.artamonau@gmail.com
-- Stability   : unstable
-- Portability : unportable
--
-- Example for TTM package. Decides whether unary number to the right of
-- starting position of machine's head is even or odd. If it's odd the then
-- single one symbol is left on the type. If it's even then no one symbols
-- are left on the tape.
------------------------------------------------------------------------------


module Even
       (
         Even.even
       )
       where

import TTM

-- | Transition table for a turing machine which decides whether supplied
-- unary number is even or odd.
even =
  -- stop on first non-zero item to the right
  Rule     Sz  Zero     Sz  Zero MRight :+:
  Rule     Sz   One (S1 Sz)  One  MNoop :+:

  -- remove the number keeping the oddity of eliminated part;
  -- S1 Sz — erased part is even
  -- S2 Sz — erased part is odd
  Rule (S1 Sz)  One (S2 Sz) Zero MRight :+:
  Rule (S2 Sz)  One (S1 Sz) Zero MRight :+:

  -- write back the result
  Rule (S1 Sz) Zero (S9 Sz) Zero MNoop  :+:
  Rule (S2 Sz) Zero (S9 Sz)  One MNoop  :+:

  Nil
