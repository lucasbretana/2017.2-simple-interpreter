import Estado
--import InferenciaTipo

data AExp = Num Int
            |Var String
            |Sum AExp AExp
            |Sub AExp AExp
            |Mul AExp AExp
  deriving(Show)

data BExp = TRUE
            | FALSE
            | Not BExp
            | And BExp BExp
            | Or  BExp BExp
            | Ig  AExp AExp
  deriving(Show)

data CExp = While BExp CExp
            | If BExp CExp CExp
            | Seq CExp CExp
            | Atrib AExp AExp
            | Skip
            | Throw
            | TryCatch CExp CExp
  deriving(Show)

data Exp = Exp1 AExp | Exp2 BExp | Exp3 CExp
  deriving(Show)

data Type = VOID | INT | BOOL
  deriving(Show)

--------------------------------------------------------------------
-- Aritmetcs rules
interpretA :: (AExp,Estado) -> (AExp,Estado)
interpretA (a,s) = if isFinalA a then (a,s) else interpretA (aSmallStep (a,s))

isFinalA :: AExp -> Bool
isFinalA (Num a) = True
isFinalA _       = False

aSmallStep :: (AExp,Estado) -> (AExp,Estado)
-- Var
aSmallStep (Var x,s) = (Num (procuraVar s x),s)
-- Sum
aSmallStep (Sum (Num x) (Num y), s) = (Num (x+y),s)
aSmallStep (Sum (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)
                                    in (Sum (Num x) ef,s)
aSmallStep (Sum e1 e2, s) = let (ef,_) = aSmallStep (e1, s)
                                    in (Sum ef e2,s)
-- Subtraction
aSmallStep (Sub (Num n1) (Num n2), s) = (Num (n1-n2), s)
aSmallStep (Sub (Num n) e, s) = let (ef, _) = aSmallStep (e, s) 
                                      in (Sub (Num n) ef, s)
aSmallStep (Sub e1 e2, s) = let (ef, _) = aSmallStep (e1, s)
                                      in (Sum ef e1, s)
-- Multiplication
aSmallStep (Mul (Num n1) (Num n2), s) = (Num (n1*n2), s)
aSmallStep (Mul (Num n) e, s) = let (ef, _)  = aSmallStep (e, s)
                                      in (Sum (Num n) ef, s)
aSmallStep (Mul e1 e2, s) = let (ef, _) = aSmallStep (e1, s)
                                      in (Sum ef e2, s)

--------------------------------------------------------------------
-- Boolean rules
--------------------------------------------------------------------
interpretB :: (BExp,Estado) -> (BExp,Estado)
interpretB (b,s) = if isFinalB b then (b,s) else interpretB (bSmallStep (b,s))

isFinalB :: BExp -> Bool
isFinalB TRUE   = True
isFinalB FALSE  = True
isFinalB _      = False


bSmallStep :: (BExp,Estado) -> (BExp,Estado)
-- Not
bSmallStep (Not FALSE,s)    = (TRUE, s)
bSmallStep (Not TRUE,s)     = (FALSE, s)
bSmallStep (Not b, s) = let (bn,sn) = bSmallStep (b,s)
                            in (Not bn ,sn)
-- And
bSmallStep (And TRUE b2,s)  = (b2,s)
bSmallStep (And FALSE b2,s) = (FALSE,s)
bSmallStep (And b1 b2,s) = let (bn,sn) = bSmallStep (b1,s)
                            in (And bn b2,sn)
-- Or
bSmallStep (Or TRUE _, s)   = (TRUE, s)
bSmallStep (Or FALSE b, s)  = (b, s)
bSmallStep (Or b1 b2, s)  = let (bf, _) = bSmallStep (b1, s)
                          in (Or bf b2, s)
-- Ig
bSmallStep (Ig (Num n1) (Num n2), s)
  | n1 == n2  = (TRUE, s)
  | otherwise = (FALSE, s)
bSmallStep (Ig (Num n) e, s) = let (ef, _) = aSmallStep (e, s)
                                  in (Ig (Num n) ef, s)
bSmallStep (Ig e1 e2, s) = let (ef, _) = aSmallStep (e1, s)
                                  in (Ig ef e2, s)

--------------------------------------------------------------------
-- Command interpreter
--------------------------------------------------------------------

interpretC :: (CExp,Estado) -> (CExp,Estado)
interpretC (c,s) = if isFinalC c then (c, s) else interpretC (cSmallStep (c, s))

isFinalC :: CExp -> Bool
isFinalC Skip   = True
isFinalC Throw  = True
isFinalC _      = False

cSmallStep :: (CExp,Estado) -> (CExp,Estado)
-- If
cSmallStep (If TRUE c1 c2, s) = (c1, s)
cSmallStep (If FALSE c1 c2, s) = (c2, s)
cSmallStep (If b c1 c2, s) = (If bf c1 c2, s)
  where
    (bf, _) = bSmallStep (b, s)
-- Seq
cSmallStep (Seq Skip c, s) = (c, s)
cSmallStep (Seq Throw _, s) = (Throw, s)
cSmallStep (Seq c1 c2, s) = (Seq cf c2, sf)
  where
    (cf, sf) = cSmallStep (c1, s)
-- While
cSmallStep (While b c, s) = (If b (Seq c (While b c)) Skip, s)
-- Atrib
cSmallStep (Atrib (Var name) (Num n), s) = (Skip, mudaVar s name n)
cSmallStep (Atrib (Var n1) (Var n2), s) = (Atrib (Var n1) (Num (procuraVar s n2)), s)
-- TryCatch
cSmallStep (TryCatch Skip _, s) = (Skip, s)
cSmallStep (TryCatch Throw (c), s) = (c, s)
cSmallStep (TryCatch c1 c2, s) = (TryCatch cf c2, sf)
  where
    (cf, sf) = cSmallStep (c1, s)


--------------------------------------------------------------------
-- Type inference
--------------------------------------------------------------------
iTipo :: Exp -> Type
iTipo (Exp3 Skip) = VOID
iTipo (Exp3 (If b c1 c2)) = case (iTipo b) of
                     BOOL -> case (iTipo c1) of
                                VOID -> case (iTipo c2) of
                                          VOID -> VOID
                                          otherwise -> error "Third argument must be VOID"
                                otherwise -> error "Second argument must be VOID"
                     otherwise -> error "First argument must be BOOL"

--------------------------------------------------------------------
-- Other stuff
--------------------------------------------------------------------
cleanMem :: Estado
cleanMem = [("x",0), ("y",0), ("z",313)]

bExe1 :: BExp
bExe1 = And (And TRUE (Not FALSE)) (Or (Not (Ig (Var "x") (Var "y"))) TRUE)

cExce1 :: CExp
cExce1 = Seq (Atrib (Var "x") (Num 5)) (TryCatch Skip (Atrib (Var "x") (Var "z")))

cExce2 :: CExp
cExce2 = Seq Throw cExce1

--------------------------------------------------------------------
--------------------------------------------------------------------
meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]

exemplo :: AExp
exemplo = Sum (Num 3) (Sum (Var "x") (Var "y"))

exemplo2 :: BExp
exemplo2 = And (And TRUE (Not FALSE)) (And (Not (Not TRUE)) TRUE)
