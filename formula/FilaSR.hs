module FilaSR(FilaSR, add, removerPrimeiro, pegarPrimeiro, filaSRVazia, tamanho) where

data FilaSR a = Fila [a] deriving (Show, Eq)

add :: Eq a => FilaSR a -> a -> FilaSR a
add (Fila fila) elemento
  | pertence (Fila fila) elemento = Fila fila -- se o elemento ja esta na fila
  | otherwise = Fila (fila ++ [elemento]) -- adiciona o elemnento nao esta na fila

pertence :: Eq a => FilaSR a -> a -> Bool
pertence (Fila [])    elemento = False
pertence (Fila (a:b)) elemento
  | a == elemento = True
  | otherwise = pertence (Fila b) elemento

removerPrimeiro :: FilaSR a -> FilaSR a
removerPrimeiro (Fila []) = error "Fila vazia"
removerPrimeiro (Fila (a:b)) = Fila b

pegarPrimeiro :: FilaSR a -> a
pegarPrimeiro (Fila []) = error "Fila vazia"
pegarPrimeiro (Fila (a:b)) = a

filaSRVazia :: FilaSR a
filaSRVazia = Fila []

tamanho :: FilaSR a -> Int 
tamanho (Fila fila) = length fila