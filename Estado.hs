module Estado where

type Estado = [(String,Int)]

adiciona :: Estado -> String -> a -> Estado
adiciona e s i = (s,i):e

procuraVar :: Estado -> String -> a
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v

mudaVar :: Estado -> String -> a -> Estado
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n
