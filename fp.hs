{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- Imports for Monads

import Control.Monad

-- TY ::= Num | Boolean | TY -> TY

data TYPELANG = TNum
              | TBool
              | TYPELANG :->: TYPELANG
              deriving (Show,Eq)

data VALUELANG where
  NumV :: Int -> VALUELANG
  BooleanV :: Bool -> VALUELANG
  ClosureV :: String -> TERMLANG -> ValueEnv -> VALUELANG
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
              | Lambda String TYPELANG TERMLANG
              | App TERMLANG TERMLANG
                deriving (Show,Eq)

type ValueEnv = [(String, VALUELANG)]
type Cont = [(String,TYPELANG)]


evalM :: ValueEnv -> TERMLANG -> Maybe VALUELANG
evalM e (Num x) = if x<0 then Nothing else Just (NumV x)
evalM e (Plus l r) = do {
                       (NumV l') <- evalM e l;
                       (NumV r') <- evalM e r;
                       return (NumV (l'+r') )
                     }
evalM e (Minus l r) = do {
                        (NumV l') <- evalM e l;
                        (NumV r') <- evalM e r;
                        if (l'-r') < 0
                        then Nothing
                        else return (NumV (l'-r') )
                      }
evalM e (Mult l r) = do {
                       (NumV l') <- evalM e l;
                       (NumV r') <- evalM e r;
                       return (NumV (l'*r') )
                     }
evalM e (Div l r) = do {
                      (NumV l') <- evalM e l;
                      (NumV r') <- evalM e r;
                      if r' == 0
                      then Nothing
                      else return ( NumV(l' `div` r') )
                    }
evalM e (Boolean b) = Just (BooleanV b)
evalM e (And l r) = do {
                      (BooleanV l') <- evalM e l;
                      (BooleanV r') <- evalM e r;
                      return (BooleanV (l' && r'))
                    }
evalM e (Or l r) = do {
                     (BooleanV l') <- evalM e l;
                     (BooleanV r') <- evalM e r;
                     return (BooleanV (l' || r'))
                   }
evalM e (Leq l r) = do {
                      (NumV l') <- evalM e l;
                      (NumV r') <- evalM e r;
                      return (BooleanV (l' <= r'))
                    }
evalM e (IsZero x) = do {
                       (NumV x') <- evalM e x;
                       return (BooleanV (x' == 0))
                     }
evalM e (If c t e') = do {
                        (BooleanV c') <- evalM e c;
                        t' <- evalM e t;
                        e'' <- evalM e e';
                        return (if c' then t' else e'')
                      }
evalM e (Bind i v b) = do {
                         v' <- evalM e v;
                         evalM ((i,v'):e) b
                       }
evalM e (Id i) = lookup i e
evalM e (Lambda i d b) = return (ClosureV i b e)
evalM e (App f a) = do {
                      (ClosureV i b j) <- evalM e f;
                      v <- evalM e a;
                      evalM ((i,v):j) b
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
typeofM c (Lambda i d b) = do {
                             r <- typeofM ((i,d):c) b;
                             return (d :->: r)
                           }
typeofM c (App f a) = do {
                        a' <- typeofM c a;
                        d :->: r <- typeofM c f;
                        if a'==d then return r else Nothing
                      }

