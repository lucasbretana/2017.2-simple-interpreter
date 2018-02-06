import Estado


data AExp = Num Int
            |Var String
            |Som AExp AExp
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
  deriving(Show)

-- Aritmetcs rules
interpretA :: (AExp,Estado) -> (AExp,Estado)
interpretA (a,s) = if isFinalA a then (a,s) else interpretA (aSmallStep (a,s))

isFinalA :: AExp -> Bool
isFinalA (Num a) = True
isFinalA x = False

aSmallStep :: (AExp,Estado) -> (AExp,Estado)
-- Var
aSmallStep (Var x,s) = (Num (procuraVar s x),s)
-- Sum
aSmallStep (Som (Num x) (Num y), s) = (Num (x+y),s)
aSmallStep (Som (Num x) e2, s) = let (ef,_) = aSmallStep (e2 ,s)
                                    in (Som (Num x) ef,s)
aSmallStep (Som e1 e2, s) = let (ef,_) = aSmallStep (e1, s)
                                    in (Som ef e2,s)
-- Subtraction
aSmallStep (Sub (Num n1) (Num n2), s) = (Num (n1-n2), s)
aSmallStep (Sub (Num n) e, s) = let (ef, _) = aSmallStep (e, s) 
                                      in (Sub (Num n) ef, s)
aSmallStep (Sub e1 e2, s) = let (ef, _) = aSmallStep (e1, s)
                                      in (Som ef e1, s)
-- Multiplication
aSmallStep (Mul (Num n1) (Num n2), s) = (Num (n1*n2), s)
aSmallStep (Mul (Num n) e, s) = let (ef, _)  = aSmallStep (e, s)
                                      in (Som (Num n) ef, s)
aSmallStep (Mul e1 e2, s) = let (ef, _) = aSmallStep (e1, s)
                                      in (Som ef e2, s)

-- Boolean rules
interpretB :: (BExp,Estado) -> (BExp,Estado)
interpretB (b,s) = if isFinalB b then (b,s) else interpretB (bSmallStep (b,s))

isFinalB :: BExp -> Bool
isFinalB TRUE   = True
isFinalB FALSE  = True
isFinalB x      = False


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
bSmallStep (Or TRUE _, s) = (TRUE, s)
bSmallStep (Or FALSE b)   = (b, s)
bSmallStep (Or b1 b2, s) = let (bf, _) = bSmallStep (b1, s)
                          in bSmallStep (bf e2, s)
-- Ig
bSmallStep (Ig (Num n1) (Num n2)  = (n1==n2, s)
bSmallStep (Ig (Num n) e) = let (ef, _) = bSmallStep (e, s)
                                  in bSmallStep (Num n) ef, s)
bSmallStep (Ig e1 e2) = let (ef, _) = bSmallStep (e1, s)
                                  in bSmallStep (ef e2, s)


interpretC :: (CExp,Estado) -> (CExp,Estado)
interpretC (c,s) = if isFinalC c then (c, s) else interpretC (cSmallStep (c, s))

isFinalC :: CExp -> Bool
isFinalC Skip = True
isFinalC _    = False

cSmallStep :: (CExp,Estado) -> (CExp,Estado)
-- Skip
cSmallStep (Skip, s) = (Skip, s)
-- If
cSmallStep (If TRUE c1 c2) = (c1, s)
cSmallStep (If FALSE c1 c2) = (c2, s)
cSmallStep (If b c1 c2) = (If bf c1 c2, s)
  where
    (bf, _) = bSmallStep (b, s)
-- Seq
cSmallStep (Seq Skip c, s) = (c, s)
cSmallStep (Seq c1 c2, s) = (cf c2, sf)
  where
    (cf, sf) = (cSmallStep c1, s)
-- While
cSmallStep (While b c) = (If b (Seq c (While b c)) Skip, s)
-- Atrib
cSmallStep (Atrib (Var name) (Num n)) = (Skip, mudaVar s name n)

--cbigStep (If b c1 c2,s) = 
--cbigStep (Seq c1 c2,s)  = 
--cbigStep (Atrib (Var x) e,s) = 



meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0)]


exemplo :: AExp
exemplo = Som (Num 3) (Som (Var "x") (Var "y"))

-- RODANDO O EXEMPLO:
-- Hugs> interpretA (exemplo, meuEstado)

exemplo2 :: BExp
exemplo2 = And (And TRUE (Not FALSE)) (And (Not (Not TRUE)) TRUE)

-- *Main> interpretB (exemplo2,meuEstado)
-- (TRUE,[("x",3),("y",0),("z",0)])


