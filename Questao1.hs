module Questao1 where

import Pilha

operar :: Double -> Double -> Char -> Double
operar operando1 operando2 '+' = operando1 + operando2
operar operando1 operando2 '-' = operando1 - operando2
operar operando1 operando2 '*' = operando1 * operando2
operar operando1 operando2 '/' = operando1 / operando2

ehOperador :: Char -> Bool
ehOperador '+' = True
ehOperador '-' = True
ehOperador '*' = True
ehOperador '/' = True
ehOperador  _  = False

ehDigito :: Char -> Bool
ehDigito simbolo = (simbolo >= '0' && simbolo <= '9') || simbolo == '.'



avaliar :: String -> PilhaData Double -> String -> IO Double
avaliar "" (Pilha []) _ = error "Expressao vazia"
avaliar "" pilha _ = return (top pilha)
avaliar (simbolo:expressao) pilha numero =
                                          if ehOperador simbolo then
                                            avaliar expressao (push (operar esqOp dirOp simbolo) (pop (pop pilha))) ""
                                          else if ehDigito simbolo then
                                            avaliar expressao pilha (numero++[simbolo])
                                          else if simbolo == ' ' && (numero /= "") then
                                            avaliar expressao (push (read numero::Double) pilha) ""
                                          else
                                            avaliar expressao pilha ""
                                          where
                                            dirOp = top pilha
                                            esqOp = top (pop pilha)

main :: IO()
main =
    do
        putStrLn "Digite uma expressao pos-fixa (+,-,*,/):"
        expressao <- getLine
        resultadoExp <- avaliar expressao pilhaVazia ""
        putStrLn ("Resultado da expressao: " ++ (show resultadoExp))
