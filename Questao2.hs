module Questao2 where

import Pilha

verificarParenteses :: String -> PilhaData Char -> IO Bool
verificarParenteses   []  pilha = if isEmpty pilha then return True else return False
verificarParenteses (caractere:restante) pilha
    | caractere == '(' = verificarParenteses restante (push 'X' pilha)
    | caractere == ')' = verificarParenteses restante (pop pilha)
    | otherwise = verificarParenteses restante pilha

main :: IO()
main = 
    do
        putStrLn "Digite uma sequencia:"
        sequenciaDeCaracteres <- getLine
        parentesesValidos <- verificarParenteses sequenciaDeCaracteres pilhaVazia
        if parentesesValidos then
            putStrLn "Tudo correto!"
        else
            putStrLn "Deu errado pai kkk"
        