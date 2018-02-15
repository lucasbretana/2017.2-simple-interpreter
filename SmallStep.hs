module SmallStep where 

import Estado
--import InferenciaTipo

data Exp = Num Int
            | Var String
            | Sum Exp Exp
            | Sub Exp Exp
            | Mul Exp Exp
            | TRUE
            | FALSE
            | Not Exp
            | And Exp Exp
            | Or  Exp Exp
            | Ig  Exp Exp
            | Skip
            | Throw
            | TryCatch Exp Exp
            | If Exp Exp Exp
            | While Exp Exp
            | Seq Exp Exp
            | Atrib Exp Exp
  deriving(Show)

--dataExp =Exp1 Exp |Exp2 Exp |Exp3 Exp
--  deriving(Show)

data Type = VOID | INT | BOOL | Error Exp Exp Type

instance Show Type where
  show VOID = "VOID"
  show INT = "INT"
  show BOOL = "BOOL"
  show (Error exp arg rt) = "Type error \n" ++
    "Argument \'" ++ (show arg) ++ "\' had type \'" ++ (show $iTipo arg) ++ "\'\n" ++
    "In expression \'" ++ (show exp) ++ "\'\n" ++
    "Excpected type \'" ++ (show rt) ++ "\'\n" ++
    "In expression \'" ++ (ritgh exp) ++ "\'"
    --"Expected type \'" ++ (show rt) ++ "\' for the \'" ++ (show arg) ++ "\'\nin the expression \'" ++ (show exp) ++ "\' that have format \'" ++ (ritgh exp) ++ "\'"

ritgh :: Exp -> String
ritgh (Num _) = "Num"
ritgh (Var _) = "Var"
ritgh (Sum _ _) = "Sum INT INT"
ritgh (Sub _ _) = "Sub INT INT"
ritgh (Mul _ _) = "Mul INT INT"
ritgh (Not _) = "Not BOOL"
ritgh (And _ _) = "And BOOL BOOL"
ritgh (Or _ _) = "Or BOOL BOOL"
ritgh (Ig _ _) = "Ig BOOL BOOL"
ritgh (TryCatch _ _) = "TryCatch VOID VOID"
ritgh (If _ _ _) = "If BOOL VOID VOID"
ritgh (While _ _) = "While BOOL VOID"
ritgh (Seq _ _) = "Seq VOID VOID"
ritgh (Atrib _ _) = "Atrib INT INT"
ritgh a = show a
--------------------------------------------------------------------
-- Aritmetcs rules
interpret :: (Exp,Estado) -> (Exp,Estado)
interpret (a,s) = if isFinal a then (a,s) else interpret (smallStep (a,s))

isFinal :: Exp -> Bool
isFinal (Num a) = True
isFinal TRUE   = True
isFinal FALSE  = True
isFinal Skip   = True
isFinal Throw  = True
isFinal _      = False

smallStep :: (Exp,Estado) -> (Exp,Estado)
-- Var
smallStep (Var x,s) = (Num (procuraVar s x),s)
-- Sum
smallStep (Sum (Num x) (Num y), s) = (Num (x+y),s)
smallStep (Sum (Num x) e2, s) = let (ef, sf) = smallStep (e2 ,s)
                                    in (Sum (Num x) ef,sf)
smallStep (Sum e1 e2, s) = let (ef,sf) = smallStep (e1, s)
                                    in (Sum ef e2, sf)
-- Subtraction
smallStep (Sub (Num n1) (Num n2), s) = (Num (n1-n2), s)
smallStep (Sub (Num n) e, s) = let (ef, sf) = smallStep (e, s) 
                                      in (Sub (Num n) ef, sf)
smallStep (Sub e1 e2, s) = let (ef, sf) = smallStep (e1, s)
                                      in (Sub ef e2, sf)
-- Multiplication
smallStep (Mul (Num n1) (Num n2), s) = (Num (n1*n2), s)
smallStep (Mul (Num n) e, s) = let (ef, sf)  = smallStep (e, s)
                                      in (Sum (Num n) ef, sf)
smallStep (Mul e1 e2, s) = let (ef, sf) = smallStep (e1, s)
                                      in (Sum ef e2, sf)

--------------------------------------------------------------------
-- Boolean rules
--------------------------------------------------------------------
-- Not
smallStep (Not FALSE,s)    = (TRUE, s)
smallStep (Not TRUE,s)     = (FALSE, s)
smallStep (Not b, s) = let (bn,sn) = smallStep (b,s)
                            in (Not bn ,sn)
-- And
smallStep (And TRUE b2,s)  = (b2,s)
smallStep (And FALSE b2,s) = (FALSE,s)
smallStep (And b1 b2,s) = let (bn,sn) = smallStep (b1,s)
                            in (And bn b2,sn)
-- Or
smallStep (Or TRUE _, s)   = (TRUE, s)
smallStep (Or FALSE b, s)  = (b, s)
smallStep (Or b1 b2, s)  = let (bf, _) = smallStep (b1, s)
                          in (Or bf b2, s)
-- Ig
smallStep (Ig (Num n1) (Num n2), s)
  | n1 == n2  = (TRUE, s)
  | otherwise = (FALSE, s)
smallStep (Ig (Num n) e, s) = let (ef, _) = smallStep (e, s)
                                  in (Ig (Num n) ef, s)
smallStep (Ig e1 e2, s) = let (ef, _) = smallStep (e1, s)
                                  in (Ig ef e2, s)

--------------------------------------------------------------------
-- Command interpreter
--------------------------------------------------------------------
-- If
smallStep (If TRUE c1 c2, s) = (c1, s)
smallStep (If FALSE c1 c2, s) = (c2, s)
smallStep (If b c1 c2, s) = (If bf c1 c2, s)
  where
    (bf, _) = smallStep (b, s)
-- Seq
smallStep (Seq Skip c, s) = (c, s)
smallStep (Seq Throw _, s) = (Throw, s)
smallStep (Seq c1 c2, s) = (Seq cf c2, sf)
  where
    (cf, sf) = smallStep (c1, s)
-- While
smallStep (While b c, s) = (If b (Seq c (While b c)) Skip, s)
-- Atrib
smallStep (Atrib (Var name) (Num n), s) = (Skip, mudaVar s name n)
smallStep (Atrib (Var name) e, s) = let (ef, sf) = smallStep (e, s)
                                    in (Atrib (Var name) ef, sf)
smallStep (Atrib v e, s) = let (vf, sf) = smallStep (vf, sf)
                          in (Atrib vf e, sf)
-- TryCatch
smallStep (TryCatch Skip _, s) = (Skip, s)
smallStep (TryCatch Throw (c), s) = (c, s)
smallStep (TryCatch c1 c2, s) = (TryCatch cf c2, sf)
  where
    (cf, sf) = smallStep (c1, s)


--------------------------------------------------------------------
-- Type inference
--------------------------------------------------------------------
iTipo :: Exp -> Type
iTipo (Num _) = INT
iTipo (Var _) = INT
iTipo (Sum a b) = case (iTipo a) of
                    Error a b c -> Error a b c
                    INT -> case (iTipo b) of
                            Error a b c -> Error a b c
                            INT -> INT
                            otherwise -> Error (Sum a b) b INT
                    otherwise -> Error (Sum a b) a INT
iTipo (Sub a b) = case (iTipo a) of
                    Error a b c -> Error a b c
                    INT -> case (iTipo b) of
                            Error a b c -> Error a b c
                            INT -> INT
                            otherwise -> Error (Sub a b) b INT
                    otherwise -> Error (Sub a b) a INT
iTipo (Mul a b) = case (iTipo a) of
                    Error a b c -> Error a b c
                    INT -> case (iTipo b) of
                            Error a b c -> Error a b c
                            INT -> INT
                            otherwise -> Error (Mul a b) b INT
                    otherwise -> Error (Mul a b) a INT
iTipo TRUE = BOOL
iTipo FALSE = BOOL
iTipo (Not a) = case (iTipo a) of
              Error a b c -> Error a b c
              BOOL -> BOOL
              otherwise -> Error (Not a) a BOOL
iTipo (And a b) = case (iTipo a) of
                    Error a b c -> Error a b c
                    BOOL -> case (iTipo b) of
                            Error a b c -> Error a b c
                            BOOL -> BOOL
                            otherwise -> Error (And a b) b BOOL
                    otherwise -> Error (And a b) a BOOL
iTipo (Or a b) = case (iTipo a) of
                    Error a b c -> Error a b c
                    BOOL -> case (iTipo b) of
                            Error a b c -> Error a b c
                            BOOL -> BOOL
                            otherwise -> Error (Or a b) b BOOL
                    otherwise -> Error (Or a b) a BOOL
iTipo (Ig a b) = case (iTipo a) of
                    Error a b c -> Error a b c
                    INT -> case (iTipo b) of
                            Error a b c -> Error a b c
                            INT -> BOOL
                            otherwise -> Error (Ig a b) b INT
                    otherwise -> Error (Ig a b) a INT
iTipo Skip = VOID
iTipo Throw = VOID
iTipo (TryCatch c1 c2) = case (iTipo c1) of
                          Error a b c -> Error a b c
                          VOID -> case (iTipo c2) of 
                                  Error a b c -> Error a b c
                                  VOID -> VOID
                                  otherwise -> Error (TryCatch c1 c2) c2 VOID
                          otherwise -> Error (TryCatch c1 c2) c1 VOID
iTipo (If b c1 c2) = case (iTipo b) of
                      Error a b c -> Error a b c
                      BOOL -> case (iTipo c1) of
                              Error a b c -> Error a b c
                              VOID -> case (iTipo c2) of
                                      Error a b c -> Error a b c
                                      VOID -> VOID
                                      otherwise -> Error (If b c1 c2) c2 VOID
                              otherwise -> Error (If b c1 c2) c1 VOID
                      otherwise -> Error (If b c1 c2) b BOOL
iTipo (While b c) = case (iTipo b) of
                    Error a b c -> Error a b c
                    BOOL -> case (iTipo c) of 
                            Error a b c -> Error a b c
                            VOID -> VOID
                            otherwise -> Error (While b c) c VOID
                    otherwise -> Error (While b c) b BOOL
iTipo (Seq c1 c2) = case (iTipo c1) of
                    Error a b c -> Error a b c
                    VOID -> case (iTipo c2) of
                            Error a b c -> Error a b c
                            VOID -> VOID
                            otherwise -> Error (Seq c1 c2) c2 VOID
                    otherwise -> Error (Seq c1 c2) c1 VOID
iTipo (Atrib v e) = case (iTipo v) of
                    Error a b c -> Error a b c
                    INT -> case (iTipo e) of
                           Error a b c -> Error a b c
                           INT -> VOID
                           otherwise -> Error (Atrib v e) e INT
                    otherwise -> Error (Atrib v e) v INT

