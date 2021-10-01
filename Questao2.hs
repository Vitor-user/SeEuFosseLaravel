module Questao2 where

import Pilha

verificarParentesesAux :: String -> PilhaData Char -> Bool
verificarParentesesAux   []  pilha = isEmpty pilha
verificarParentesesAux (caractere:restante) pilha
    | caractere == '(' = verificarParentesesAux restante (push 'X' pilha)
    | caractere == ')' = verificarParentesesAux restante (pop pilha)
    | otherwise = verificarParentesesAux restante pilha

verificarParenteses :: String -> Bool
verificarParenteses entrada = verificarParentesesAux entrada pilhaVazia

-- main :: IO()
-- main = 
--     do
--         putStrLn "Digite uma sequencia:"
--         sequenciaDeCaracteres <- getLine
--         parentesesValidos <- verificarParenteses sequenciaDeCaracteres
--         if parentesesValidos then
--             putStrLn "Tudo correto!"
--         else
--             putStrLn "Deu errado pai kkk"
        