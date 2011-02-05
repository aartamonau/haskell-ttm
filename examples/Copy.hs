------------------------------------------------------------------------------
-- |
-- Module      : Copy
-- Copyright   : (C) 2011 Aliaksiej Artamonaŭ
-- License     : LGPL
--
-- Maintainer  : aliaksiej.artamonau@gmail.com
-- Stability   : unstable
-- Portability : unportable
--
-- Example for TTM package. Replicates an unary number to the right of the
-- head of machine and positions the head between two replicas.
------------------------------------------------------------------------------


module Copy
       (
         copy
       )
       where

import TTM

-- | Transition table for a turing machine that replicates supplied unary
-- number.
copy =
  -- stop on the next item after the first non-zero
  Rule          Sz  Zero          Sz  Zero MRight :+:
  Rule          Sz   One      (S1 Sz)  One MRight :+:

  -- if it's last unit then move it to both copies and stop
  Rule      (S1 Sz) Zero      (S2 Sz)  One  MLeft :+:
  Rule      (S2 Sz)  One      (S3 Sz) Zero  MLeft :+:
  Rule      (S3 Sz) Zero (S9 $ S9 Sz)  One MRight :+:

  -- add one unit to the left replica
  Rule      (S1 Sz)  One      (S4 Sz)  One  MLeft :+:
  Rule      (S4 Sz)  One      (S5 Sz) Zero  MLeft :+:
  Rule      (S5 Sz) Zero      (S6 Sz)  One MRight :+:
  Rule      (S6 Sz) Zero      (S7 Sz) Zero MRight :+:

  -- -- go to the end of the number that is being copied
  Rule      (S7 Sz)  One      (S7 Sz)  One MRight :+:
  Rule      (S7 Sz) Zero      (S8 Sz) Zero MRight :+:

  -- -- add a unit to the right replica
  Rule      (S8 Sz) Zero      (S9 Sz)  One  MLeft :+:
  Rule      (S8 Sz)  One      (S8 Sz)  One MRight :+:

  -- go back to the beginning of the rest of the number being copied;
  -- then go one step to the right to get in the same state as in (S1 Sz);
  Rule      (S9 Sz) Zero (S1 $ S0 Sz) Zero  MLeft :+:
  Rule      (S9 Sz)  One      (S9 Sz)  One  MLeft :+:
  Rule (S1 $ S0 Sz)  One (S1 $ S0 Sz)  One  MLeft :+:
  Rule (S1 $ S0 Sz) Zero (S1 $ S1 Sz) Zero MRight :+:
  Rule (S1 $ S1 Sz)  One      (S1 Sz)  One MRight :+:

  Nil
