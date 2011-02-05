module Even
       (
         evenTransitions
       )
       where

import TTM

-- | Transition table for a turing machine which decides whether supplied
-- unary number is even or odd. If it's odd then the machine terminates
-- leaving single One symbol on the tape. If it's even then the machine
-- terminates leaving the tape empty.
evenTransitions =
  -- stop on first non-zero item to the right
  Rule     Sz  Zero     Sz  Zero MRight :+:
  Rule     Sz   One (S1 Sz)  One  MNoop :+:

  Rule (S1 Sz)  One (S2 Sz) Zero MRight :+:
  Rule (S2 Sz)  One (S1 Sz) Zero MRight :+:
  Rule (S1 Sz) Zero (S9 Sz) Zero MNoop  :+:
  Rule (S2 Sz) Zero (S9 Sz)  One MNoop  :+:

  Nil
