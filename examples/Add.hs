module Add
       (
         add
       )
       where

import TTM

-- | Transition table for a turing machine that adds two unary numbers.
add =
  -- stop on first non-zero item to the right
  Rule     Sz  Zero          Sz  Zero MRight :+:
  Rule     Sz   One      (S1 Sz)  One  MNoop :+:

  -- find first zero separator between the numbers
  Rule (S1 Sz)  One      (S1 Sz)  One MRight :+:
  Rule (S1 Sz) Zero      (S2 Sz) Zero  MNoop :+:

  -- go to position next to the first unit of the second number
  Rule (S2 Sz) Zero      (S2 Sz) Zero MRight :+:
  Rule (S2 Sz)  One      (S3 Sz)  One MRight :+:

  -- if we found zero here then it's the last item of the second number;
  -- so we stop after shifting the last unit;
  Rule (S3 Sz) Zero      (S4 Sz) Zero  MLeft :+:

  -- -- otherwise we continue shuttling
  Rule (S3 Sz)  One      (S7 Sz)  One  MLeft :+:

  -- moving the last unit to the first number
  Rule (S4 Sz)  One      (S5 Sz) Zero  MLeft :+:
  Rule (S5 Sz) Zero      (S5 Sz) Zero  MLeft :+:
  Rule (S5 Sz)  One      (S6 Sz)  One MRight :+:
  -- write unit here and halt
  Rule (S6 Sz) Zero (S1 $ S0 Sz)  One MRight :+:

  -- moving one unit and then returning back for the next one
  Rule (S7 Sz)  One      (S8 Sz) Zero  MLeft :+:
  Rule (S8 Sz) Zero      (S8 Sz) Zero  MLeft :+:
  Rule (S8 Sz)  One      (S9 Sz)  One MRight :+:
  Rule (S9 Sz) Zero      (S2 Sz)  One MRight :+:

  Nil
