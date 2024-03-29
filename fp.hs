{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS -Wall #-}

-- Imports for Monads

-- TY ::= Num | Boolean | TY -> TY

data TYPELANG = TNum
              | TBool
              | TArray TYPELANG
              | TLambda String TERMLANG NULLABLE NULLABLE
              deriving (Show,Eq)

data NULLABLE = Type TYPELANG | NotProvided | NotFound deriving (Show,Eq)

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
              | TypedLambda String TERMLANG TYPELANG TYPELANG  -- Can explicitly provide the domain and range type
              | App TERMLANG TERMLANG
              | Fix TERMLANG TYPELANG TYPELANG
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
              | Comment String TERMLANG
                deriving (Show,Eq)

type ValueEnv = [(String, VALUELANG)]
type Cont = [(String,TYPELANG)]
-- Used for type-checking of recursive functions
-- Key is the stringified representation of the function
-- Value is the declared return type of the function
-- If return type was not specified,
type Recursion = [([Char],NULLABLE)]

subst :: String -> TERMLANG -> TERMLANG -> TERMLANG
subst _ _ (Num x) = Num x
subst i v (Plus l r) = Plus (subst i v l) (subst i v r)
subst i v (Minus l r) = Minus (subst i v l) (subst i v r)
subst i v (Mult l r) = Mult (subst i v l) (subst i v r)
subst i v (Div l r) = Div (subst i v l) (subst i v r)
subst _ _ (Boolean x) = Boolean x
subst i v (And l r) = And (subst i v l) (subst i v r)
subst i v (Or l r) = Or (subst i v l) (subst i v r)
subst i v (Leq l r) = Leq (subst i v l) (subst i v r)
subst i v (IsZero x) = IsZero (subst i v x)
subst i v (If c t f) = If (subst i v c) (subst i v t) (subst i v f)
subst i v (Bind i' v' b') = Bind i' (subst i v v') (if i == i' then b' else (subst i v b'))
subst i v (Id i') = if i == i' then v else (Id i')
subst i v (Lambda i' b) = Lambda i' (if i == i' then b else (subst i v b))
subst i v (TypedLambda i' b _ _) = Lambda i' (if i == i' then b else (subst i v b))
subst i v (App f a) = App (subst i v f) (subst i v a)
subst i v (Fix f d r) = Fix (subst i v f) d r
subst i v (Array a) = Array (map (\x -> (subst i v x)) a)
subst i v (Take n a) = Take (subst i v n) (subst i v a)
subst i v (Drop n a) = Drop (subst i v n) (subst i v a)
subst i v (Length a) = Length (subst i v a)
subst i v (At i' a) = At (subst i v i') (subst i v a)
subst i v (Concat l r) = Concat (subst i v l) (subst i v r)
subst i v (Replicate n v') = Replicate (subst i v n) (subst i v v')
subst i v (First a) = First (subst i v a)
subst i v (Second a) = Second (subst i v a)
subst i v (Last a) = Last (subst i v a)
subst i v (Reverse a) = Reverse (subst i v a)
subst i v (Comment c b) = Comment c (subst i v b)

evalM :: ValueEnv -> TERMLANG -> Maybe VALUELANG
evalM _ (Num x) = if x<0 then Nothing else Just (NumV x)
evalM e (Plus l r) = do {
                       (NumV l') <- evalM e l;
                       (NumV r') <- evalM e r;
                       return $ NumV $ l'+r'
                     }
evalM e (Minus l r) = do {
                        (NumV l') <- evalM e l;
                        (NumV r') <- evalM e r;
                        if   (l'-r') < 0
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
                      if   r' == 0
                      then Nothing
                      else return $ NumV $ l' `div` r'
                    }
evalM _ (Boolean b) = Just (BooleanV b)
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
                        if c' then evalM e t else evalM e e'
                      }
evalM e (Bind i v b) = do {
                         v' <- evalM e v;
                         evalM ((i,v'):e) b
                       }
evalM e (Id i) = lookup i e
evalM e (Lambda i b) = return $ ClosureV i b e
evalM e (TypedLambda i b _ _) = return $ ClosureV i b e
evalM e (App f a) = do {
                      (ClosureV i b e') <- evalM e f;
                      v <- evalM e a;
                      evalM ((i,v):e') b
                    }
evalM e (Fix f d r) = do {
                    (ClosureV i b e') <- evalM e f;
                    evalM e' (subst i (Fix (Lambda i b) d r) b)
                  }
evalM e (Array a) = do {
                      a' <- liftMaybe $ map (\i -> evalM e i) a;
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
evalM e (Comment _ b) = evalM e b

liftMaybe :: [Maybe a] -> Maybe [a]
liftMaybe [] = Just []
liftMaybe (i:a) = do {
                    i' <- i;
                    a' <- liftMaybe a;
                    return $ i':a'
                  }


typeofM :: (Cont, Recursion) -> TERMLANG -> Maybe TYPELANG
typeofM _ (Num x) = if x>= 0 then return TNum else Nothing
typeofM _ (Boolean _) = return TBool
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
typeofM (c, r) (Bind i v b) = do {
                                tv <- typeofM (c,r) v;
                                typeofM (((i,tv):c),r) b
                              }
typeofM (c, _) (Id i) = lookup i c
typeofM _ (Lambda i b) = Just $ TLambda i b NotProvided NotProvided
typeofM _ (TypedLambda i b d r) = Just $ TLambda i b (Type d) (Type r)
typeofM (c, r) (App f a) = do {
                             a' <- typeofM (c,r) a;
                             (TLambda i b d rt) <- typeofM (c,r) f;
                             -- Check if function is in a recursion and if so, make sure return type has been provided
                             let returnType = lookupRecursion r (show b)
                              in if   d == NotProvided || show d == show (Type a')
                                 then if   rt == NotProvided
                                      then typeofM (((i,a'):c), r) b
                                      else if  returnType == NotFound
                                           then do {
                                                  realReturn <- typeofM (((i,a'):c), ((show b, rt):r)) b;
                                                  if   rt == NotProvided || show rt == show (Type realReturn)
                                                  then Just realReturn
                                                  else Nothing  -- Actual return type did not match defined type
                                                }
                                           else if   returnType == NotProvided
                                                then Nothing  -- Recursive functions must have a return type provided
                                                else toTypeLang returnType
                                 else Nothing
                           }
typeofM c (Fix f d r) = do {
                          (TLambda i b _ _) <- typeofM c f;
                          (TLambda i' b' _ _) <- typeofM c b;
                          typeofM c (TypedLambda i' (subst i (Fix (Lambda i b) d r) b') d r)
                        }
typeofM c (Array a) = do {
                        a' <- if length a == 0 then Nothing else typeofM c $ head a;
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
                         (TArray _) <- typeofM c a;
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
                        return a';
                      }
typeofM c (Second a) = do {
                         (TArray a') <- typeofM c a;
                         return a';
                       }
typeofM c (Last a) = do {
                       (TArray a') <- typeofM c a;
                       return a';
                     }
typeofM c (Reverse a) = do {
                          (TArray a') <- typeofM c a;
                          return $ TArray a';
                        }
typeofM c (Comment _ b) = typeofM c b


lookupRecursion :: Recursion -> [Char] -> NULLABLE
lookupRecursion [] _ = NotFound
lookupRecursion ((body, returnType) : rest) target = if   body == target
                                                     then returnType
                                                     else lookupRecursion rest target

toTypeLang :: NULLABLE -> Maybe TYPELANG
toTypeLang NotProvided = Nothing
toTypeLang NotFound = Nothing
toTypeLang (Type t) = Just t

interpTypeEval :: TERMLANG -> Maybe VALUELANG
interpTypeEval e = if   typeofM ([], []) e == Nothing
                   then Nothing
                   else evalM [] e


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
    in if   pass
       then ("[🟩PASS] " ++ inputString ++ "\n         Output: " ++ expectedOutputString)
       else ("[🟥FAIL] " ++ inputString
                        ++ "\n         Expected: "
                        ++ expectedOutputString
                        ++ "\n         Received: "
                        ++ realOutputString)
  ) $ zip tests $ map (\(a,_) -> function a) tests

theories:: [(TERMLANG, Maybe VALUELANG)]
theories = [
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
  -- Empty array is not supported when type checking is enabled as we can't infer it's type
  -- But, this works while type checking is disabled
  (Array [], Nothing),
  (Array [Num 4], Just (ArrayV [NumV 4])),
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
    Comment
      "I made this test fail intentionally, just to demo how the testing function works"
      (Last (Array [Num 1,Num 2,Num 3,Num 4,Num 5])),
    Just (NumV 3)
  ),
  (Drop (Num 2) (Array [Num 1,Num 2,Num 3,Num 4,Num 5]), Just (ArrayV [NumV 3,NumV 4,NumV 5])),
  (Reverse (Array [Num 1,Num 2,Num 3,Num 4,Num 5]), Just (ArrayV [NumV 5,NumV 4,NumV 3,NumV 2,NumV 1])),
  (Bind "arr" (Array [Num 1,Num 2,Num 3,Num 4,Num 5]) (Plus (First (Id "arr")) (Num 10)), Just (NumV 11)),
  (Bind "arr" (Array [Num 1,Num 2,Num 3,Num 4,Num 5]) (Plus (At (Num 4) (Id "arr")) (Last (Id "arr"))), Just (NumV 10)),
  (Bind "arr" (Replicate (Num 2) (Num 5)) (Length (Id "arr")), Just (NumV 2)),
  (Bind "arr" (Replicate (Num 2) (Num 5)) (Id "arr"), Just (ArrayV [NumV 5,NumV 5])),
  (Bind "arr" (Replicate (Num 1) (Num 5)) (Plus (First (Id "arr")) (Num 10)), Just (NumV 15)),
  (
    Bind "arr" (Array [Num 1,Num 2,Num 3,Num 4,Num 5]) (Reverse (Id "arr")),
    Just (ArrayV [NumV 5,NumV 4,NumV 3,NumV 2,NumV 1])
  ),
  (App (Fix (Lambda "g" (Lambda "x" (Num 6))) TNum TNum) (Num 3), Just (NumV 6)),
  (
    Fix (Lambda "g" (Lambda "x" (Id "g"))) TNum TNum,
    Just (ClosureV "x" (Fix (Lambda "g" (Lambda "x" (Id "g"))) TNum TNum) [])
  ),
  (
    App (Fix (Lambda "g" (Lambda "x" (Id "x"))) TNum TNum) (Num 42),
    Just (NumV 42)
  ),
  (
    App
      (Fix
        (Lambda "g"
          (Lambda "x"
            (If (IsZero (Id "x"))
              (Num 2)
              (Num 0)
            )
          )
        ) TNum TNum
      )
      (Num 0),
    Just (NumV 2)
  ),
  (
    Fix
      (Lambda "g"
        (Lambda "x"
          (If (IsZero (Id "x"))
            (Num 0)
            (App (Id "g") (Num 0))
          )
        )
      ) TNum TNum,
    Just (
      ClosureV "x"
        (If (IsZero (Id "x"))
          (Num 0)
          (App
            (Fix
              (Lambda "g"
                (Lambda "x"
                  (If (IsZero (Id "x"))
                    (Num 0)
                    (App (Id "g") (Num 0))
                  )
                )
              ) TNum TNum
            )
            (Num 0)
          )
        )
      []
    )
  ),
  (
    App
      (Fix
        (Lambda "g"
          (Lambda "x"
            (If (IsZero (Id "x"))
              (Num 0)
              (App (Id "g") (Num 0))
            )
          )
        ) TNum TNum
      )
      (Num 0),
    Just (NumV 0)
  ),
  (
    Fix (
      Lambda "g" (
        (
          Lambda "x" (
            If (IsZero (Id "x"))
              (Num 1)
              (
                Mult
                (Id "x")
                (
                  App
                    (Id "g")
                    (Minus (Id "x") (Num 1))
                )
              )
          )
        )
      )
    ) TNum TNum,
    Just (
      ClosureV "x" (
        If (IsZero (Id "x"))
          (Num 1)
          (
            Mult
              (Id "x")
              (App
                (Fix
                  (Lambda "g"
                    (Lambda "x"
                      (If (IsZero (Id "x"))
                        (Num 1)
                        (Mult
                          (Id "x")
                          (App (Id "g") (Minus (Id "x") (Num 1)))
                        )
                      )
                    )
                  ) TNum TNum
                )
                (Minus (Id "x") (Num 1))
              )
          )
        )
        []
    )
  ),
  (
    Bind "factorial" (
      Lambda "g" (
        (
          Lambda "x" (
            If (IsZero (Id "x"))
              (Num 1)
              (
                App
                  (Id "g")
                  (Num 0)
              )
          )
        )
      )
    ) (
      App
        (Fix (Id "factorial") TNum TNum)
        (Num 3)
    ),
    Just (NumV 1)
  ),
  -- bind factorial = (lambda g in (lambda x in if x=0 then 1 else x*(g)(x-1))) in ((fix)(factorial))(3)
  (
    Bind "factorial" (
      Lambda "g" (
        (
          Lambda "x" (
            If (IsZero (Id "x"))
              (Num 1)
              (
                Mult
                  (Id "x")
                  (
                    App
                      (Id "g")
                      (Minus (Id "x") (Num 1)))
                  )
              )
          )
      )
    ) (
      App
        (Fix (Id "factorial") TNum TNum)
        (Num 3)
    ),
    Just (NumV 6)
  ),

  -- bind factorial = (lambda g in (lambda x in if x=0 then 1 else x*(g)(x-1))) in bind fact = (fix)(factorial) in (fact)(3)
  (
    Bind "factorial" (
      Lambda "g" (
        (
          Lambda "x" (
            If (IsZero (Id "x"))
              (Num 1)
              (
                Mult
                  (Id "x")
                  (
                    App
                      (Id "g")
                      (Minus (Id "x") (Num 1)))
                  )
              )
          )
      )
    ) (
      Bind "fact"
        (Fix (Id "factorial") TNum TNum)
        (App (Id "fact") (Num 10))
    ),
    Just (NumV 3628800)
  ),
  -- Optionally, can specify return type explicitly
  (TypedLambda "x" (Num 0) TNum TNum, Just (ClosureV "x" (Num 0) [])),
  (App (TypedLambda "x" (Num 0) TNum TNum) (Num 1), Just (NumV 0)),
  -- Invalid argument type
  (App (TypedLambda "x" (Num 0) TNum TNum) (Boolean True), Nothing),
  -- Invalid return type
  (App (TypedLambda "x" (Boolean True) TNum TNum) (Num 1), Nothing)
  ]

evalM_tests:: [Char]
-- This is using evalM instead of interpTypeEval, because type checking
-- of a recursive function causes infinite loop. Need to figure out how
-- to fix that
evalM_tests = runTests interpTypeEval theories

subst_tests:: [Char]
subst_tests = runTests (\(i, v, x) -> subst i v x) [
  (
    (
      "g",
      Fix (Lambda "g" (Lambda "x" (Id "g"))) TNum TNum,
      Bind "arr" (Replicate (Num 1) (Num 5)) (Plus (First (Id "arr")) (Num 10))
    ),
    Bind "arr" (Replicate (Num 1) (Num 5)) (Plus (First (Id "arr")) (Num 10))
  ),
  (
    (
      "g",
      Fix (Lambda "g" (Lambda "x" (Id "g"))) TNum TNum,
      Id "g"
    ),
    Fix (Lambda "g" (Lambda "x" (Id "g"))) TNum TNum
  ),
  (
    (
      "g",
      Fix (Lambda "g" (Lambda "x" (Id "g"))) TNum TNum,
      Lambda "x" (Id "g")
    ),
    Lambda "x" (Fix (Lambda "g" (Lambda "x" (Id "g"))) TNum TNum)
  ),
  (
    (
      "g",
      Fix
        (Lambda "g"
          (Lambda "x"
            (If (IsZero (Id "x"))
              (Num 0)
              (App (Id "g") (Num 1))
            )
          )
        ) TNum TNum,
      Lambda "x"
        (If (IsZero (Id "x"))
          (Num 0)
          (App (Id "g") (Num 1))
        )
    ),
    Lambda "x"
      (If (IsZero (Id "x"))
        (Num 0)
        (App
          (Fix
            (Lambda "g"
              (Lambda "x"
                (If (IsZero (Id "x"))
                  (Num 0)
                  (App (Id "g") (Num 1)))
              )
            ) TNum TNum
          )
          (Num 1)
        )
      )
  )
  ]


-- Printing results
main :: IO ()
main = putStrLn $ "Interp Test Results:\n" ++ evalM_tests ++ "\n\nSubst Test Results:\n" ++ subst_tests


