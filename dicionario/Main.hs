module Main where

import DicionarioMain

main :: IO()
main =
  do
    putStr (">1 Contar as palavras de um arquivo existente\n" ++
            ">2 Criar um arquivo com um texto e contar as palavras\n" ++
            ">3 Contar palavras de uma frase sem salvar em arquivo\n" ++
            ">4 Sair\n" ++
            ">")
    opcao <- getLine
    case opcao of
      "1" -> 
        do
          arquivoExistente
          main
      "2" ->
        do
          arquivoNovo
          main
      "3" ->
        do
          semArquivo
          main
      "4" -> putStrLn "Saindo..."
      _   -> main

arquivoExistente :: IO()
arquivoExistente =
  do
    putStr "Digite o nome do arquivo com a extensao: "
    caminho <- getLine
    texto <- readFile caminho
    writeFile ("contagem_" ++ caminho) (imprimir texto) -- cria o arquivo com a contagem

arquivoNovo :: IO()
arquivoNovo =
  do
    putStr "Digite o nome do arquivo (sem extensao): "
    caminho <- getLine
    putStr "Digite a mensagem: \n> "
    mensagem <- getLine 
    writeFile (caminho ++ ".txt") mensagem                           -- cria o arquivo com a mensagem e nome passados pelo usuario
    writeFile ("contagem_" ++ caminho ++ ".txt") (imprimir mensagem) -- cria o arquivo com a contagem

semArquivo :: IO()
semArquivo =
  do
    putStr "Digite a mensagem: \n> "
    mensagem <- getLine 
    putStrLn "Contagem:"
    putStrLn (imprimir mensagem)

