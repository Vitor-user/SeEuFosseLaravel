module Utils (
    validarCpf,
    ehDigito
) where

import TiposAgenda

ehDigito :: Char -> Bool
ehDigito simbolo = (simbolo >= '0' && simbolo <= '9')

naoRepeteSimboloAux :: String -> Char -> Bool
naoRepeteSimboloAux [] _ = False
naoRepeteSimboloAux (simbAtual:str) simbolo
    | simbAtual /= simbolo = True
    | simbAtual == simbolo = naoRepeteSimboloAux str simbolo

naoRepeteSimbolo :: String -> Bool
naoRepeteSimbolo [] = True
naoRepeteSimbolo (simbolo:str) = naoRepeteSimboloAux str simbolo

validarCpf :: CPF -> Bool
validarCpf cpf = (validarCpfAux cpfSoNumeros 10 0 0) && (naoRepeteSimbolo cpfSoNumeros)
    where
        cpfSoNumeros = (filter ehDigito cpf)

validarCpfAux :: CPF -> Int -> Int -> Int -> Bool
validarCpfAux [] _ _ _ = False
validarCpfAux [a, b] 1 acc1 acc2 = verificacaoDigito1 && verificacaoDigito2
    where
            aInt = (read [a])::Int
            bInt = (read [b])::Int
            verificacaoDigito1 = ((acc1)*10 `mod` 11) == aInt
            verificacaoDigito2 = ((acc2 + (aInt*2))*10 `mod` 11) == bInt
validarCpfAux (digito:cpf) divisor acc1 acc2 = validarCpfAux cpf (divisor-1) acumulador1 acumulador2
    where
        digitoInt = read [digito]::Int
        acumulador1 = (acc1 + (divisor*digitoInt))
        acumulador2 = (acc2 + ((divisor+1)*digitoInt))