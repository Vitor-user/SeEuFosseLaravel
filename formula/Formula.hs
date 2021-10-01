{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Formula where

import FilaSR

data Formula = Var String | Not Formula | And Formula Formula | Or Formula Formula deriving (Show, Eq)

type Linhas = [Bool]
data Coluna = Col Formula Linhas deriving Show

type Tabela = [Coluna]

formula :: Formula
formula = Not (Or (Var "A") (And (Var "B") (Var "A")))

enfileiraVariaveis :: Formula -> FilaSR String -> FilaSR String
enfileiraVariaveis (Var a) fila = add fila a
enfileiraVariaveis (Not formula) fila = enfileiraVariaveis formula fila
enfileiraVariaveis (Or esquerda direita) fila = enfileiraVariaveis direita (enfileiraVariaveis esquerda fila)
enfileiraVariaveis (And esquerda direita) fila = enfileiraVariaveis direita (enfileiraVariaveis esquerda fila)
{-
enfileiraVariaveis (Or (Var a) direita) fila = enfileiraVariaveis direita (add fila a)
enfileiraVariaveis (Or esquerda (Var a)) fila = enfileiraVariaveis esquerda (add fila a)
enfileiraVariaveis (And (Var a) direita) fila = enfileiraVariaveis direita (add fila a)
enfileiraVariaveis (And esquerda (Var a)) fila = enfileiraVariaveis esquerda (add fila a)
-}

-- conta a quantidade de linhas que serao necessarias para montar a tabela
quantidadeLinhas :: FilaSR String -> Int
quantidadeLinhas fila
  | fila == filaSRVazia = 1
  | otherwise = 2 * quantidadeLinhas (removerPrimeiro fila)

montarTabelaInicial :: Formula -> Tabela
montarTabelaInicial formula = montarTabelaInicialAuxiliar fila numeroLinhas numeroLinhas
  where
    fila = enfileiraVariaveis formula filaSRVazia -- monta a fila de variaveis
    numeroLinhas = quantidadeLinhas fila -- calcula o numero de variaveis

montarTabelaInicialAuxiliar :: FilaSR String -> Int -> Int -> Tabela
montarTabelaInicialAuxiliar fila numeroLinhas intervalo
  | tamanho fila == 1 = novaColuna -- a fila so tem 1 elemento
  | otherwise = novaColuna ++ montarTabelaInicialAuxiliar (removerPrimeiro fila) numeroLinhas novoIntervalo  -- a fila tem mais de 1 elemento
  where
    novoIntervalo = div intervalo 2 -- calcula o valor do novo intervalo (sequencia de True ou False)
    linhas = gerarLinhas numeroLinhas novoIntervalo novoIntervalo True -- gera as linhas de uma coluna
    novaColuna = [Col (Var (pegarPrimeiro fila)) linhas]

-- usa a funcao gerarNLinhas para gerar todas as linhas de uma coluna da tabela
-- de acordo com um intervalo
gerarLinhas :: Int -> Int -> Int -> Bool -> Linhas
gerarLinhas quantidade intervalo incremento booleano
  | quantidade == intervalo = linhas
  | otherwise = linhas ++ gerarLinhas quantidade (intervalo + incremento) incremento (not booleano)
    where
      linhas = gerarNLinhas incremento booleano

-- gera n quantidade de linhas na tabela
gerarNLinhas :: Int -> Bool -> Linhas
gerarNLinhas 0 valor = []
gerarNLinhas quantidade valor = valor : gerarNLinhas (quantidade - 1) valor

pegarLinha :: Coluna -> Linhas
pegarLinha (Col formula linhas) = linhas

calculaLinha :: Coluna -> Coluna -> (Bool -> Bool -> Bool) -> Linhas
calculaLinha (Col formula1 []) (Col formula2 []) operador = []
calculaLinha (Col formula1 (elemento1:linhas1)) (Col formula2 (elemento2:linhas2)) operador =
  valor : resto -- inclui valor na lista gerada pela funcao resto
  where
    valor = operador elemento1 elemento2 -- Bool
    resto = calculaLinha (Col formula1 linhas1) (Col formula1 linhas2) operador -- Funcao que retorna tipo Linhas

buscaColVar :: Tabela -> Formula -> Coluna
buscaColVar [] (Var valor) = error "A variavel  variavel"
buscaColVar (Col (Var a) valores :tabela) (Var valor)
  | valor == a = Col (Var a) valores
  | otherwise = buscaColVar tabela (Var valor)
buscaColVar (coluna:tabela) (Var valor) = buscaColVar tabela (Var valor)
buscaColVar _ _ = error "A formula nao eh uma variavel"

montarTabelaFinal :: Tabela -> Formula -> Tabela
montarTabelaFinal tabela formula = tabela ++ montarTabelaFinalAuxiliar tabela formula

montarTabelaFinalAuxiliar :: Tabela -> Formula -> Tabela
montarTabelaFinalAuxiliar tabela (Var valor) = [buscaColVar tabela (Var valor)] -- Retorna a coluna que possui a variavel
montarTabelaFinalAuxiliar tabela (Or esquerda direita) =
  [Col (Or esquerda direita) linhas]
  where
    tabelaEsqueda = montarTabelaFinalAuxiliar tabela esquerda -- Tabela
    tabelaDireita = montarTabelaFinalAuxiliar tabela direita  -- Tabela
    colunaEsquerda = head tabelaEsqueda                       -- Coluna
    colunaDireita = head tabelaDireita                        -- Coluna
    linhas = calculaLinha colunaEsquerda colunaDireita (||)   -- Linhas
montarTabelaFinalAuxiliar tabela (And esquerda direita) =
  [Col (And esquerda direita) linhas]
  where
    tabelaEsqueda = montarTabelaFinalAuxiliar tabela esquerda -- Tabela
    tabelaDireita = montarTabelaFinalAuxiliar tabela direita  -- Tabela
    colunaEsquerda = head tabelaEsqueda                       -- Coluna
    colunaDireita = head tabelaDireita                        -- Coluna
    linhas = calculaLinha colunaEsquerda colunaDireita (&&)   -- Linhas
montarTabelaFinalAuxiliar tabela (Not formula) = 
  [Col (Not formula) (map (\x -> not x) linha)]
  where
    tabelaNova = montarTabelaFinalAuxiliar tabela formula -- Tabela
    coluna = head tabelaNova -- Coluna
    linha = pegarLinha coluna
  
imprimirTabela :: Tabela -> String
imprimirTabela [e] = show e ++ "\n"
imprimirTabela (a:b) = show a ++ "\n" ++ imprimirTabela b


{-
calculaColuna :: Coluna -> Coluna -> Formula -> (Bool -> Bool -> Bool) -> Coluna
calculaColuna (Col formula1 []) (Col formula2 []) formula operador = Col formula []
calculaColuna (Col formula1 (elemento1:linhas1)) (Col formula2 (elemento2:linhas2)) formula operador = coluna
  where
    valor = operador elemento1 elemento2 -- Bool
    resto = calculaColuna (Col formula1 linhas1) (Col formula1 linhas2) formula operador -- Coluna
    coluna = Col formula (valor : pegarLinha resto) -- Coluna
-}


{-
--calcula a formula
calcula :: Formula -> Bool
calcula (Var a) = a
calcula (Or (Var a) (Var b)) = a || b
calcula (And (Var a) (Var b)) = a && b
calcula (Not (Var a)) = not a
calcula (Or esquerda direita) = calcula esquerda || calcula direita
calcula (And esquerda direita) = calcula esquerda && calcula direita
calcula (Not formula) = not (calcula formula)
-}