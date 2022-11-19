import FP

-- Test utility function
separate :: [[Char]] -> [Char]
separate [] = []
separate (x : xs) = x ++ "\n" ++ separate xs

-- Run a function on each of the provided inputs and inspect the results
-- runTest :: (a -> b) -> [a] -> [Char]
runTest function inputs = separate $ map (\(a, b) -> show a ++ "   ->   " ++ show b) $ zip inputs $ map function inputs

-- Inputs to run the function with
inputs = [
    Num 3,
    Plus (Num 4) (Num 5),
    Minus (Mult (Num 4) (Num 5)) (Num 5),
    Div (Num 4) (Num 2),
    Div (Num 4) (Num 0),
    Plus (Num 4) (Num $ -5),
    If0 (Num 0) (Num 0) (Num 1),
    If0 (Num 1) (Num 0) (Num 1),
    App (Lambda "a" (Num 2)) (Num 1),
    App (Lambda "b" (Id "b")) (Num 3),
    App (Lambda "c" (Id "c")) (If0 (Num 1) (Num 0) (Num 1)),
    App (Lambda "dd" (If0 (Num 2) (Num 4) (Id "dd"))) (If0 (Num 2) (Num 0) (Num 1)),
    App (Lambda " d" (If0 (Num 2) (Num 4) (Id "dd"))) (If0 (Num 2) (Num 0) (Num 1)),
    -- Dynamic VS Static scope
    -- bind n=1 in
    --	bind f = (lambda x in x+n) in
    --		bind n=2 in
    --			(f)(1)
    App
      (
        Lambda "n" (
          App
            (
              Lambda "f" (
                App
                  (
                    Lambda "n" (
                      App
                        (Id "f")
                        (Num 1)
                    )
                  )
                  (Num 2)
              )
            )
            (
              Lambda "x" (
                Plus (Id "x") (Id "n")
              )
            )
        )
      )
      (Num 1)
  ]

-- Running the tests (evalDyn and evalStat in this case)
evalDyn_outputs = runTest (\x -> evalDyn [] x) inputs
evalStat_outputs = runTest (\x -> evalStat [] x) inputs

-- NOTE: the tests do not check outputs yet. They only print the outputs
-- It would be good to modify this to automatically check the outputs

-- One more example
evalTerm_inputs = [
    NumX 3,
    PlusX (NumX 4) (NumX 5),
    MinusX (MultX (NumX 4) (NumX 5)) (NumX 5),
    DivX (NumX 4) (NumX 2),
    DivX (NumX 4) (NumX 0),
    PlusX (NumX 4) (NumX $ -5),
    If0X (NumX 0) (NumX 0) (NumX 1),
    If0X (NumX 1) (NumX 0) (NumX 1),
    AppX (LambdaX "a" (NumX 2)) (NumX 1),
    AppX (LambdaX "b" (IdX "b")) (NumX 3),
    AppX (LambdaX "c" (IdX "c")) (If0X (NumX 1) (NumX 0) (NumX 1)),
    AppX (LambdaX "dd" (If0X (NumX 2) (NumX 4) (IdX "dd"))) (If0X (NumX 2) (NumX 0) (NumX 1)),
    AppX (LambdaX " d" (If0X (NumX 2) (NumX 4) (IdX "dd"))) (If0X (NumX 2) (NumX 0) (NumX 1)),
    -- Dynamic VS Static scope
    AppX
      (
        LambdaX "n" (
          AppX
            (
              LambdaX "f" (
                AppX
                  (
                    LambdaX "n" (
                      AppX
                        (IdX "f")
                        (NumX 1)
                    )
                  )
                  (NumX 2)
              )
            )
            (
              LambdaX "x" (
                PlusX (IdX "x") (IdX "n")
              )
            )
        )
      )
      (NumX 1),
    BindX "n" (NumX 1) (
      BindX "f" (
        LambdaX "x" (
          PlusX (IdX "x") (IdX "n")
        )
      ) (
        BindX "n" (NumX 2) (
          AppX (IdX "f") (NumX 1)
        )
      )
    )
  ]
-- elabTerm doest not have tests as it is already tested as part of evalTerm
evalTerm_outputs = runTest (\x -> evalTerm [] x) evalTerm_inputs

-- Printing results of tests
main :: IO ()
main = putStrLn $
         "evalDyn:\n"++
          ((evalDyn_outputs) ++
          "\n\nevalStat:\n" ++
          (evalStat_outputs) ++
          "\n\nevalTerm:\n" ++
          (evalTerm_outputs))
