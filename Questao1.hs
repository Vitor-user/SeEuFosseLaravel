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



avaliarAux :: String -> PilhaData Double -> String -> IO Double
avaliarAux "" (Pilha []) _ = error "Expressao vazia"
avaliarAux "" pilha _ = return (top pilha)
avaliarAux (simbolo:expressao) pilha numero
  | ehOperador simbolo = avaliarAux expressao (push (operar esqOp dirOp simbolo) (pop (pop pilha))) ""
  | ehDigito simbolo = avaliarAux expressao pilha (numero++[simbolo])
  | simbolo == ' ' && (numero /= "") = avaliarAux expressao (push (read numero::Double) pilha) ""
  | otherwise = avaliarAux expressao pilha ""
    where
      dirOp = top pilha
      esqOp = top (pop pilha)

avaliar :: String -> IO Double
avaliar expressao = avaliarAux expressao pilhaVazia ""

-- main :: IO()
-- main =
--     do
--         putStrLn "Digite uma expressao pos-fixa (+,-,*,/):"
--         expressao <- getLine
--         resultadoExp <- avaliar expressao
--         putStrLn ("Resultado da expressao: " ++ (show resultadoExp))
