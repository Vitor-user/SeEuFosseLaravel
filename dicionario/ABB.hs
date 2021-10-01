{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module ABB (ABB, insert, search, emptyTree, printTree, searchValue) where
--                  arvEsquerda valor arvDireita 
data ABB a = Vazio | Node (ABB a) a (ABB a) deriving (Show, Eq, Ord)

insert :: Ord a => ABB a -> a -> ABB a
insert Vazio element = Node Vazio element Vazio
insert (Node left value right) element
  | value == element = Node left element right -- nao importa qual ira adicionar em caso geral, mas para o provlema em questao importa
  | value > element  = Node (insert left element) value right
  | value < element  = Node left value (insert right element)

search :: Ord a => ABB a -> a -> Bool
search Vazio element = False
search (Node left value right) element
  | value == element = True
  | value > element  = search left element
  | value < element  = search right element

searchValue :: Ord a => ABB a -> a -> a
searchValue Vazio element = error "Value not found"
searchValue (Node left value right) element
  | value == element = value -- nao importa qual ira retornar em caso geral, mas para o problema em questao importa
  | value > element  = searchValue left element
  | value < element  = searchValue right element

emptyTree :: ABB a
emptyTree = Vazio

printTree :: Show a => ABB a -> String
printTree Vazio = ""
printTree (Node left value right) = (show value) ++ "\n" ++ (printTree left) ++ (printTree right)

-- Arvore de inteiros para testes
teste :: ABB Int
teste = Node (Node (Node Vazio 2 Vazio) 5 Vazio) 10 (Node Vazio 20 Vazio)