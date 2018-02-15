import Estado
import SmallStep

--------------------------------------------------------------------
-- Other stuff
--------------------------------------------------------------------
test :: Estado
test = [("x",3), ("y",0), ("z",0)]

cleanMem :: Estado
cleanMem = [("x",0), ("y",0), ("z",313), ("w", 0)]

anotherMem :: Estado
anotherMem = [("x", 1), ("y", 2), ("z", 3), ("w", 0)]

fullMem :: Estado
fullMem = [("x", 1), ("y", 1), ("z", 313), ("w", 400)]

meuEstado :: Estado
meuEstado = [("x",3), ("y",0), ("z",0), ("w", 0)]

inc :: Exp -> Exp
inc (Var a) = Atrib (Var a) (Sum (Var a) (Num 1))

dec :: Exp -> Exp
dec (Var a) = Atrib (Var a) (Sub (Var a) (Num 1))

--------------------------------------------------------------------
--------------------------------------------------------------------

exemplo :: Exp
exemplo = Sum (Num 3) (Sum (Var "x") (Var "y"))

exemplo2 :: Exp
exemplo2 = And (And TRUE (Not FALSE)) (And (Not (Not TRUE)) TRUE)

bExe1 :: Exp
bExe1 = And (And TRUE (Not FALSE)) (Or (Not (Ig (Var "x") (Var "y"))) TRUE)

cExce2 :: Exp
cExce2 = Seq Throw bExe1

cExce3 :: Exp
cExce3 = Seq Throw (Var "x")

--------------------------------------------------------------------
--------------------------------------------------------------------
-- Exemplos de exceções
--
-- Com TryCatch
exemploExcecao1 :: Exp
exemploExcecao1 = Seq (Atrib (Var "x") (Num 5)) (TryCatch Throw (Atrib (Var "x") (Var "z")))

exemploExcecao2 :: Exp
exemploExcecao2 = While (Ig (Var "x") (Var "y")) (TryCatch (Seq (Atrib (Var "x") (Sum (Var "x") (Num 1))) Throw) (Atrib (Var "x") (Num 313)))

-- Apenas com Throw
exemploExcecao3 :: Exp
exemploExcecao3 = 
    While (And (Ig (Var "x") (Var "y") ) (Not (Ig (Var "z") (Var "w"))))
        (Seq 
          (Seq (inc (Var "x")) 
               (inc (Var "y"))
          ) 
          (If (Ig (Var "x") (Var "z")) 
                Throw 
                (dec (Var "w"))
          )
        )

--------------------------------------------------------------------
-- Exemplos de inferência de tipo que dão erro
exemploInferencia1 :: Exp
exemploInferencia1 = While (And (Ig (Var "x") (Var "y")) (Not (Ig (Var "z") (Var "w")))) (Seq (Seq (Var "x") (Var "y")) (If (Ig (Var "x") (Var "z")) Throw (Var "w")))

exemploInferencia2 :: Exp
exemploInferencia2 = And (Var "x") (Or TRUE (Ig (Var "x") (Var "y")))

exemploInferencia3 :: Exp
exemploInferencia3 = TryCatch Throw (Seq (Num 1) exemploExcecao1)
