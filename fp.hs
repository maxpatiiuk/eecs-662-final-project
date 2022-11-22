{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- Imports for Monads

import Control.Monad

-- TY ::= Num | Boolean | TY -> TY

data TYPELANG = TNum
              | TBool
              | TArray TYPELANG
              | String :->: TERMLANG
              deriving (Show,Eq)

data VALUELANG where
  NumV :: Int -> VALUELANG
  BooleanV :: Bool -> VALUELANG
  ClosureV :: String -> TERMLANG -> ValueEnv -> VALUELANG
  ArrayV :: [VALUELANG] -> VALUELANG
  deriving (Show,Eq)

-- T ::= num | true | false | id | T + T | T - T | T * T | T / T
-- |  | bind id T T | if T then T else T | T && T | T || T | T <= T | isZero T
-- lambda (id:TY) in T | (T) (T)

data TERMLANG = Num Int
              | Plus TERMLANG TERMLANG
              | Minus TERMLANG TERMLANG
              | Mult TERMLANG TERMLANG
              | Div TERMLANG TERMLANG
              | Boolean Bool
              | And TERMLANG TERMLANG
              | Or TERMLANG TERMLANG
              | Leq TERMLANG TERMLANG
              | IsZero TERMLANG
              | If TERMLANG TERMLANG TERMLANG
              | Bind String TERMLANG TERMLANG
              | Id String
              | Lambda String TERMLANG
              | App TERMLANG TERMLANG
              | Array [TERMLANG]
              | Take TERMLANG TERMLANG
              | Drop TERMLANG TERMLANG
              | Length TERMLANG
              | At TERMLANG TERMLANG
              | Concat TERMLANG TERMLANG
              | Replicate TERMLANG TERMLANG
              | First TERMLANG
              | Second TERMLANG
              | Last TERMLANG
              | Reverse TERMLANG
                deriving (Show,Eq)

type ValueEnv = [(String, VALUELANG)]
type Cont = [(String,TYPELANG)]


evalM :: ValueEnv -> TERMLANG -> Maybe VALUELANG
evalM e (Num x) = if x<0 then Nothing else Just (NumV x)
evalM e (Plus l r) = do {
                       (NumV l') <- evalM e l;
                       (NumV r') <- evalM e r;
                       return $ NumV $ l'+r'
                     }
evalM e (Minus l r) = do {
                        (NumV l') <- evalM e l;
                        (NumV r') <- evalM e r;
                        if (l'-r') < 0
                        then Nothing
                        else return $ NumV $ l'-r'
                      }
evalM e (Mult l r) = do {
                       (NumV l') <- evalM e l;
                       (NumV r') <- evalM e r;
                       return $ NumV $ l'*r'
                     }
evalM e (Div l r) = do {
                      (NumV l') <- evalM e l;
                      (NumV r') <- evalM e r;
                      if r' == 0
                      then Nothing
                      else return $ NumV $ l' `div` r'
                    }
evalM e (Boolean b) = Just (BooleanV b)
evalM e (And l r) = do {
                      (BooleanV l') <- evalM e l;
                      (BooleanV r') <- evalM e r;
                      return $ BooleanV $ l' && r'
                    }
evalM e (Or l r) = do {
                     (BooleanV l') <- evalM e l;
                     (BooleanV r') <- evalM e r;
                     return $ BooleanV $ l' || r'
                   }
evalM e (Leq l r) = do {
                      (NumV l') <- evalM e l;
                      (NumV r') <- evalM e r;
                      return $ BooleanV $ l' <= r'
                    }
evalM e (IsZero x) = do {
                       (NumV x') <- evalM e x;
                       return $ BooleanV $ x' == 0
                     }
evalM e (If c t e') = do {
                        (BooleanV c') <- evalM e c;
                        t' <- evalM e t;
                        e'' <- evalM e e';
                        return $ if c' then t' else e''
                      }
evalM e (Bind i v b) = do {
                         v' <- evalM e v;
                         evalM ((i,v'):e) b
                       }
evalM e (Id i) = lookup i e
evalM e (Lambda i b) = return $ ClosureV i b e
evalM e (App f a) = do {
                      (ClosureV i b j) <- evalM e f;
                      v <- evalM e a;
                      evalM ((i,v):j) b
                    }
evalM e (Array a) = do {
                      a' <- liftMaybe $ map (\a -> evalM e a) a;
                      return $ ArrayV a'
                    }
evalM e (Take n a) = do {
                       (NumV n') <- evalM e n;
                       (ArrayV a') <- evalM e a;
                       return $ ArrayV $ take n' a'
                     }
evalM e (Drop n a) = do {
                       (NumV n') <- evalM e n;
                       (ArrayV a') <- evalM e a;
                       return $ ArrayV $ drop n' a'
                     }
evalM e (Length a) = do {
                       (ArrayV a') <- evalM e a;
                       return $ NumV $ length a'
                     }
evalM e (At i a) = do {
                       (NumV i') <- evalM e i;
                       (ArrayV a') <- evalM e a;
                       if (length a') < i' then Nothing else return $ a' !! i' 
                     }
evalM e (Concat l r) = do {
                         (ArrayV l') <- evalM e l;
                         (ArrayV r') <- evalM e r;
                         return $ ArrayV $ l' ++ r'
                       }
evalM e (Replicate n v) = do {
                            (NumV n') <- evalM e n;
                            v' <- evalM e v;
                            return $ ArrayV $ replicate n' v'
                          }
evalM e (First a) = do {
                      (ArrayV a') <- evalM e a;
                      if (length a') < 1 then Nothing else return $ a' !! 0
                    }
evalM e (Second a) = do {
                       (ArrayV a') <- evalM e a;
                       -- Indexing is 0 based
                      if (length a') < 2 then Nothing else return $ a' !! 1
                     }
evalM e (Last a) = do {
                     (ArrayV a') <- evalM e a;
                     return $ last a';
                   }
evalM e (Reverse a) = do {
                        (ArrayV a') <- evalM e a;
                        return $ ArrayV $ reverse a';
                      }

liftMaybe :: [Maybe a] -> Maybe [a]
liftMaybe [] = Just []
liftMaybe (i:a) = do {
                    i' <- i;
                    a' <- liftMaybe a;
                    return $ i':a'
                  }


typeofM :: Cont -> TERMLANG -> Maybe TYPELANG
typeofM c (Num x) = if x>= 0 then return TNum else Nothing
typeofM c (Boolean b) = return TBool
typeofM c (Plus l r) = do {
                         TNum <- typeofM c l;
                         TNum <- typeofM c r;
                         return TNum
                       }
typeofM c (Minus l r) = do {
                          TNum <- typeofM c l;
                          TNum <- typeofM c r;
                          return TNum
                        }
typeofM c (Mult l r) = do {
                         TNum <- typeofM c l;
                         TNum <- typeofM c r;
                         return TNum
                       }
typeofM c (Div l r) = do {
                        TNum <- typeofM c l;
                        TNum <- typeofM c r;
                        return TNum
                      }
typeofM c (And l r) = do {
                        TBool <- typeofM c l;
                        TBool <- typeofM c r;
                        return TBool
                      }
typeofM c (Or l r) = do {
                       TBool <- typeofM c l;
                       TBool <- typeofM c r;
                       return TBool
                     }
typeofM c (Leq l r) = do {
                        TNum <- typeofM c l;
                        TNum <- typeofM c r;
                        return TBool
                      }
typeofM c (IsZero x) = do {
                         TNum <- typeofM c x;
                         return TBool
                       }
typeofM c (If c' t e) = do {
                          TBool <- typeofM c c';
                          t' <- typeofM c t;
                          e' <- typeofM c e;
                          if t' == e' then return t' else Nothing
                        }
typeofM c (Bind i v b) = do {
                           tv <- typeofM c v;
                           typeofM ((i,tv):c) b
                         }
typeofM c (Id i) = lookup i c
typeofM c (Lambda i b) = do {
                             return $ i :->: b
                           }
typeofM c (App f a) = do {
                        a' <- typeofM c a;
                        i :->: b <- typeofM c f;
                        b' <- typeofM ((i,a'):c) b;
                        return b'
                      }
typeofM c (Array a) = do {
                        a' <- typeofM c $ head a;
                        return $ TArray a'
                      }
typeofM c (Take n a) = do {
                         TNum <- typeofM c n;
                         (TArray a') <- typeofM c a;
                         return $ TArray a'
                       }
typeofM c (Drop n a) = do {
                         TNum <- typeofM c n;
                         (TArray a') <- typeofM c a;
                         return $ TArray a'
                       }
typeofM c (Length a) = do {
                         TNum <- typeofM c a;
                         return TNum
                       }
typeofM c (At i a) = do {
                       TNum <- typeofM c i;
                       (TArray a') <- typeofM c a;
                       return a'
                     }
typeofM c (Concat l r) = do {
                       (TArray l') <- typeofM c l;
                       (TArray r') <- typeofM c r;
                       if l' == r' then return l' else Nothing
                     }
typeofM c (Replicate n v) = do {
                       TNum <- typeofM c n;
                       v' <- typeofM c v;
                       return $ TArray v'
                     }
typeofM c (First a) = do {
                       (TArray a') <- typeofM c a;
                       typeofM c a;
                     }
typeofM c (Second a) = do {
                       (TArray a') <- typeofM c a;
                       typeofM c a;
                     }
typeofM c (Last a) = do {
                       (TArray a') <- typeofM c a;
                       typeofM c a;
                     }
typeofM c (Reverse a) = do {
                       (TArray a') <- typeofM c a;
                       return $ TArray a';
                     }

-- Test utility function
separate :: [[Char]] -> [Char]
separate [] = []
separate (x : xs) = x ++ "\n" ++ separate xs

-- Run function on each input and check output agains expected output. Print results of failed runs
runTests :: (Show a, Show b) => (a -> b) -> [(a, b)] -> [Char]
runTests function tests = separate $ map (\((input, expectedOutput), realOutput) ->
    let inputString = show input
        expectedOutputString = show expectedOutput
        realOutputString = show realOutput
        pass = expectedOutputString == realOutputString
        format = if pass then "[🟩PASS]" else "[🟥FAIL] Input: " ++ inputString ++ "\n\t  Expected: " ++ expectedOutputString ++ "\n\t  Received: " ++ realOutputString
    in format
  ) $ zip tests $ map (\(a,_) -> function a) tests

tests = [
  (Num 3, Just (NumV 3)),
  (Plus (Num 4) (Num 5), Just (NumV 9)),
  (Minus (Mult (Num 4) (Num 5)) (Num 5), Just (NumV 15)),
  (Div (Num 4) (Num 2), Just (NumV 2)),
  (Div (Num 4) (Num 0), Nothing),
  (Plus (Num 4) (Num (-5)), Nothing),
  (If (IsZero (Num 0)) (Num 0) (Num 1), Just (NumV 0)),
  (If (IsZero (Num 1)) (Num 0) (Num 1), Just (NumV 1)),
  (App (Lambda "a" (Num 2)) (Num 1), Just (NumV 2)),
  (App (Lambda "b" (Id "b")) (Num 3), Just (NumV 3)),
  (App (Lambda "c" (Id "c")) (If (IsZero (Num 1)) (Num 0) (Num 1)), Just (NumV 1)),
  (App (Lambda "dd" (If (IsZero (Num 2)) (Num 4) (Id "dd"))) (If (IsZero (Num 2)) (Num 0) (Num 1)), Just (NumV 1)),
  (App (Lambda " d" (If (IsZero (Num 2)) (Num 4) (Id "dd"))) (If (IsZero (Num 2)) (Num 0) (Num 1)), Nothing),
  (
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
      (Num 1),
    Just (NumV 2)
  ),
  (
    Bind "n" (Num 1) (
        Bind "f" (
          Lambda "x" (
            Plus (Id "x") (Id "n")
          )
        ) (
          Bind "n" (Num 2) (
            App (Id "f") (Num 1)
          )
        )
      ),
      Just (NumV 2)
  ),
  (Array [], Just (ArrayV [])),
  (Length (Num 4), Nothing),
  (At (Num 1) (Num 4), Nothing),
  (First (Num 4), Nothing),
  (Last (Num 4), Nothing),
  (Second (Num 4), Nothing),
  (Array [Num 1,Num 2,Num 3,Num 4,Num 5], Just (ArrayV [NumV 1,NumV 2,NumV 3,NumV 4,NumV 5])),
  (Length (Array [Num 1,Num 2,Num 3,Num 4,Num 5]), Just (NumV 5)),
  (At (Num 4) (Array [Num 1,Num 2,Num 3,Num 4,Num 5]), Just (NumV 5)),
  (Concat (Array [Num 1,Num 2]) (Array [Num 3,Num 4,Num 5]), Just (ArrayV [NumV 1,NumV 2,NumV 3,NumV 4,NumV 5])),
  (Take (Num 4) (Array [Num 1,Num 2]), Just (ArrayV [NumV 1,NumV 2])),
  (Take (Num 4) (Array [Num 1,Num 2,Num 3,Num 4,Num 5]), Just (ArrayV [NumV 1,NumV 2,NumV 3,NumV 4])),
  (First (Array [Num 1,Num 2,Num 3,Num 4,Num 5]), Just (NumV 1)),
  (First (Array []), Nothing),
  (Second (Array [Num 1,Num 2,Num 3,Num 4,Num 5]), Just (NumV 2)),
  (Second (Array [Num 1]), Nothing),
  (
    Bind
      "I made this test fail intentionally, just to demo how the testing function works"
      (Num 0)
      (Last (Array [Num 1,Num 2,Num 3,Num 4,Num 5])),
    Just (NumV 4)
  ),
  (Drop (Num 2) (Array [Num 1,Num 2,Num 3,Num 4,Num 5]), Just (ArrayV [NumV 3,NumV 4,NumV 5])),
  (Reverse (Array [Num 1,Num 2,Num 3,Num 4,Num 5]), Just (ArrayV [NumV 5,NumV 4,NumV 3,NumV 2,NumV 1])),
  (Bind "arr" (Array [Num 1,Num 2,Num 3,Num 4,Num 5]) (Plus (First (Id "arr")) (Num 10)), Just (NumV 11)),
  (Bind "arr" (Array [Num 1,Num 2,Num 3,Num 4,Num 5]) (Plus (At (Num 4) (Id "arr")) (Last (Id "arr"))), Just (NumV 10)),
  (Bind "arr" (Replicate (Num 2) (Num 5)) (Length (Id "arr")), Just (NumV 2)),
  (Bind "arr" (Replicate (Num 2) (Num 5)) (Id "arr"), Just (ArrayV [NumV 5,NumV 5])),
  (Bind "arr" (Replicate (Num 1) (Num 5)) (Plus (First (Id "arr")) (Num 10)), Just (NumV 15)),
  (Bind "arr" (Array [Num 1,Num 2,Num 3,Num 4,Num 5]) (Reverse (Id "arr")), Just (ArrayV [NumV 5,NumV 4,NumV 3,NumV 2,NumV 1]))
  ]

evalM_tests = runTests (\x -> evalM [] x) tests

-- Printing results
main :: IO ()
main = putStrLn $ "Test Results:\n" ++ (evalM_tests)


