module Dicionario (Dicionario, adicionarRepeticao, novoDicionario) where

type Repeticao = Int
type Valor = String

data Dicionario = Dic Repeticao Valor

instance Show Dicionario where
  show (Dic repeticao valor) = valor ++ " - " ++ (show repeticao)

instance Eq Dicionario where
  (Dic repeticao1 valor1) == (Dic repeticao2 valor2) = valor1 == valor2

instance Ord Dicionario where
  (Dic repeticao1 valor1) <= (Dic repeticao2 valor2) = valor1 <= valor2
  (Dic repeticao1 valor1) >= (Dic repeticao2 valor2) = valor1 >= valor2
  (Dic repeticao1 valor1) > (Dic repeticao2 valor2) = valor1 > valor2
  (Dic repeticao1 valor1) < (Dic repeticao2 valor2) = valor1 < valor2

adicionarRepeticao :: Dicionario -> Dicionario
adicionarRepeticao (Dic repeticao valor) = (Dic (repeticao + 1) valor)

novoDicionario :: String -> Dicionario
novoDicionario palavra = Dic 1 palavra