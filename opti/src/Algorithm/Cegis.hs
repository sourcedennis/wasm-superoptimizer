
-- | CounterExample-Guided Inductive Synthesis general structure.
module Algorithm.Cegis where

-- | The general structure of CounterExample-Guided Inductive Synthesis. A
-- synthesizer produces a program. The verifier determines its correctness, or
-- produces a counter-example. The synthesizer then "learns" from this
-- counter-example. This process repeats until a valid program is found.
cegis :: Monad m => ( ins -> prog ) -> ( prog -> m (Maybe cex) ) -> ( ins -> cex -> ins ) -> ins -> m prog
cegis fGenerate fVerify fRefine ins =
  do
    let prog = fGenerate ins
    mCounterExample <- fVerify prog
    case mCounterExample of
      Nothing -> return prog
      Just counterExample ->
        let ins' = fRefine ins counterExample
        in cegis fGenerate fVerify fRefine ins'
