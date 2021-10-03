module Main where

import Formula

main :: IO()
main = mainAuxiliar []

mainAuxiliar :: [Formula] -> IO()
mainAuxiliar formulas =
  do
    putStr (">1 Entrar com uma formula e montar tabela\n" ++
            ">2 Montar tabela de uma formula do historico (historico eh apagado ao fim da execucao)\n" ++
            ">3 Sair\n" ++
            ">")
    opcao <- getLine
    case opcao of
      "1" ->
        do
          putStrLn instrucoes
          putStr "Formula: "
          formulaString <- getLine
          let formula = (read formulaString :: Formula)
          --imprimindo a formula
          tabela <- montarTabelaCompleta formula
          tabelaString <- imprimirTabela tabela
          putStrLn tabelaString
          ehTautologia <- tautologia tabela
          putStrLn ehTautologia
          --inclui a nova formula na lista e chama a funcao mainAuxiliar
          mainAuxiliar (formula : formulas)
      "2" ->
        do
          if null formulas then
            do
              putStrLn "Historico vazio"
              mainAuxiliar formulas
          else
            do
              putStrLn "Escolha uma das formulas"
              putStrLn (imprimirLista formulas 0)
              putStr ">"
              posicaoString <- getLine
              let posicao = read posicaoString
              if posicao > (length formulas -1) || posicao<0 then
                do
                  putStrLn "Posicao invalida"
                  mainAuxiliar formulas
              else
                do
                  --imprimindo a formula
                  tabela <- montarTabelaCompleta (formulas !! posicao)
                  tabelaString <- imprimirTabela tabela
                  putStrLn tabelaString
                  ehTautologia <- tautologia tabela
                  putStrLn ehTautologia
                  --chama a funcao mainAuxiliar passando a formulas
                  mainAuxiliar formulas
      "3" -> putStrLn "Saindo..."
      _   ->
        do
          putStrLn "Opcao invalida, tente novamente"
          mainAuxiliar formulas

imprimirLista :: Show a => [a] -> Int -> String
imprimirLista [e] cont =  show cont ++ ". " ++ show e
imprimirLista (a:b) cont = show cont ++ ". " ++ show a ++ "\n" ++ imprimirLista b (cont+1)

instrucoes :: String
instrucoes = "As formulas sao montadas da seguinte forma:\n" ++
              "Var String :: Formula\n" ++
              "Not Formula :: Formula\n" ++
              "And Formula Formula :: Formula\n" ++
              "Or Formula Formula :: Formula\n" ++
              "Exemplo: Not (Or (Var \"A\") (And (Var \"B\") (Var \"A\")))\n"

montarTabelaCompleta :: Formula -> IO Tabela
montarTabelaCompleta formula =
  do
    let tabela = montarTabelaInicial formula -- inicio da tabela, somente com as variaveis
    let tabelaFinal = montarTabelaFinal tabela formula
    return tabelaFinal

tautologia :: Tabela -> IO String
tautologia tabela =
  do
    let ehTautologia = verificaTautologia tabela -- recebe true se for tautologia
    if ehTautologia then
      return "Eh tautologia"
    else
      return "Nao eh tautologia"

------------------- FUNCOES PARA IMPRIMIR A TABELA -------------------------

imprimirTabela :: Tabela -> IO String
imprimirTabela tabela =
  do
    let matriz = montarMatriz tabela -- [[String]]
    let texto = transformaEmTexto matriz
    return texto


montarMatriz :: Tabela -> [[String]]
montarMatriz b
  = map
      (\ a -> [show (pegarFormula a)] ++ montarColuna (pegarLinha a)) b

montarColuna :: Linhas -> [String]
montarColuna [] = []
montarColuna (a:b)
  | a = "V" : montarColuna b
  | otherwise = "F" : montarColuna b

transformaEmTexto :: [[String]] -> String
transformaEmTexto [] = []
transformaEmTexto (a:b) = transformaEmLinha a ++ "\n" ++ transformaEmTexto b

transformaEmLinha :: [String] -> String
transformaEmLinha [e] = e
transformaEmLinha (a:b) = a ++ "    " ++ transformaEmLinha b