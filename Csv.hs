module Csv (
    Caminho,
    Csv,
    Linha,
    CsvObj,

    carregarArquivo,
    escreverEmArquivo,
) where

type Caminho = String
type Csv = String
type Linha = [String]
type CsvObj = [Linha]

interpretarArquivo :: String -> String -> [String] -> CsvObj -> IO CsvObj
interpretarArquivo [] acumulador linha csvObj = return (csvObj++[linha++[acumulador]])
interpretarArquivo (simbolo:conteudo) acumulador linha csvObj
    | simbolo == ','  = interpretarArquivo conteudo "" (linha++[acumulador]) csvObj
    | simbolo == '\n' = interpretarArquivo conteudo "" [] (csvObj++[linha++[acumulador]])
    | otherwise = interpretarArquivo conteudo (acumulador++[simbolo]) linha csvObj

carregarArquivo :: Caminho -> IO CsvObj
carregarArquivo nomeArquivo =
    do
        conteudo <- readFile nomeArquivo
        interpretarArquivo conteudo "" [] []

csvLinhaParaString :: Linha -> String
csvLinhaParaString [] = ""
csvLinhaParaString [ultimoItem] = ultimoItem
csvLinhaParaString (item:linha) = item++","++(csvLinhaParaString linha)

csvObjParaString :: CsvObj -> String
csvObjParaString [] = ""
csvObjParaString [ultimaLinha] = csvLinhaParaString ultimaLinha
csvObjParaString (linha:obj) = (csvLinhaParaString linha)++"\n"++(csvObjParaString obj)

escreverEmArquivo :: Caminho -> CsvObj -> IO()
escreverEmArquivo nomeArquivo csvObj = writeFile nomeArquivo (csvObjParaString csvObj)