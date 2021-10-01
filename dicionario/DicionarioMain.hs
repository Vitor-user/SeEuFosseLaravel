module DicionarioMain () where

import ABB
import Dicionario

teste :: String
teste = "redes aula redes aula haskell aula"

main :: IO()
main = do
          putStr (imprimir teste)

imprimir :: String -> String
imprimir texto = imprimirAuxiliar (words texto) emptyTree

imprimirAuxiliar :: [String] -> ABB Dicionario -> String
imprimirAuxiliar palavras arvore = printTree (contagem palavras arvore)

contagem :: [String] -> ABB Dicionario -> ABB Dicionario
contagem [] arvore = arvore
contagem (palavra:b) arvore
  | search arvore noNovo = contagem b (insert arvore noRepetido) -- caso o valor ja esteja na arvore
  | otherwise = contagem b (insert arvore noNovo)
  where
    noNovo = novoDicionario palavra
    noRepetido = adicionarRepeticao (searchValue arvore noNovo)
