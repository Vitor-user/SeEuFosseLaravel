module Agenda (
    criarContato,
    adicionarContato,

    mostrarContato,
    listarContatos,
    
    pesquisarContatoPorCpf,
    pesquisarContatoPorNome,
    
    apagarContatoPorCpf,
    apagarContatoPorNome,
    apagarContatoPorPosicao,
    
    alterarContatoPorCpf,
    alterarContatoPorNome,
    alterarContatoPorPosicao,
    
    carregarAgenda,
    salvarAgenda)
where

import Utils
import TiposAgenda
import Csv

criarContato :: CPF -> Nome -> Telefone -> Email -> ContatoData
criarContato cpf nome telefone email
    | validarCpf cpf =  Contato (filter ehDigito cpf) nome telefone email
    | otherwise = Invalido

adicionarContato :: ContatoData -> Agenda -> IO Agenda
adicionarContato Invalido agenda = return agenda
adicionarContato contato agenda = return (agenda++[contato])
    {-do
        agendaDeArquivo <- carregarAgenda
        salvarAgenda (agendaDeArquivo++[contato])
        return (agendaDeArquivo++[contato])-}

mostrarContato :: ContatoData -> Int -> String
mostrarContato (Contato cpf nome telefone email) (-1) = "CPF: "++cpf ++ "\n" ++ "Nome: "++ nome ++ "\n" ++ "Telefone: " ++ telefone ++ "\n" ++ "E-mail: " ++ email
mostrarContato (Contato cpf nome telefone email) pos = "[" ++ (show pos) ++"] CPF: "++cpf ++ "\n" ++ "Nome: "++ nome ++ "\n" ++ "Telefone: " ++ telefone ++ "\n" ++ "E-mail: " ++ email

listarContatosAux :: Agenda -> Int -> String
listarContatosAux [] _ = ""
listarContatosAux (contato:agenda) pos = (mostrarContato contato pos) ++ "\n\n" ++ (listarContatosAux agenda (pos+1) )

listarContatos :: Agenda -> IO()
listarContatos agenda = putStrLn (listarContatosAux agenda 0)
    {-do
        agenda <- carregarAgenda
        putStrLn (listarContatosAux agenda)-}


pesquisarContato :: Dado -> Agenda -> (ContatoData -> Dado) -> Int -> Int
pesquisarContato _ [] _ _ = -1 
pesquisarContato valor (contato:agenda) retiraDado pos
    | valor == (retiraDado contato) = pos
    | otherwise = pesquisarContato valor agenda retiraDado (pos+1)

pesquisarContatoPorCpf :: CPF -> Agenda -> Int
pesquisarContatoPorCpf cpf agenda = pesquisarContato cpf agenda cpfContato 0

pesquisarContatoPorNome :: Nome -> Agenda -> Int
pesquisarContatoPorNome nome agenda = pesquisarContato nome agenda nomeContato 0

apagarContatoPorPosicao :: Int -> Agenda -> Agenda
apagarContatoPorPosicao _  [] = []
apagarContatoPorPosicao 0 (contato:agenda) = agenda
apagarContatoPorPosicao posicao (contato:agenda) = contato:(apagarContatoPorPosicao (posicao-1) agenda)

apagarContatoPorCpf :: CPF -> Agenda -> Agenda
apagarContatoPorCpf cpf agenda
    | posicao > -1 = apagarContatoPorPosicao posicao agenda
    | otherwise = agenda
        where posicao = (pesquisarContatoPorCpf cpf agenda)

apagarContatoPorNome :: Nome -> Agenda -> Agenda
apagarContatoPorNome nome agenda
    | posicao > -1 = apagarContatoPorPosicao posicao agenda
    | otherwise = agenda
        where posicao = (pesquisarContatoPorNome nome agenda)

alterarContatoPorPosicao :: ContatoData -> Agenda -> Int -> Agenda
alterarContatoPorPosicao _ [] _ = []
alterarContatoPorPosicao Invalido agenda _ = agenda
alterarContatoPorPosicao contato (contatoAtual:agenda) 0 = contato:agenda
alterarContatoPorPosicao contato (contatoAtual:agenda) posicao = [contatoAtual]++(alterarContatoPorPosicao contato agenda (posicao-1))

alterarContatoPorCpf :: ContatoData -> Agenda -> CPF -> Agenda
alterarContatoPorCpf contato agenda cpf
    | posicao > -1 = alterarContatoPorPosicao contato agenda posicao
    | otherwise = agenda
    where
        posicao = (pesquisarContatoPorCpf cpf agenda)

alterarContatoPorNome :: ContatoData -> Agenda -> Nome -> Agenda
alterarContatoPorNome contato agenda nome
    | posicao > -1 = alterarContatoPorPosicao contato agenda posicao
    | otherwise = agenda
    where
        posicao = (pesquisarContatoPorNome nome agenda)

converterParaAgenda :: CsvObj -> Agenda -> IO Agenda
converterParaAgenda [] acc = return acc
converterParaAgenda (registro:csvObj) acc = converterParaAgenda csvObj ((Contato cpf nome telefone email):acc)
    where
        [cpf, nome, telefone, email] = registro


carregarAgenda :: IO Agenda
carregarAgenda =
    do
        arquivoCarregado <- carregarArquivo "agenda.csv"
        converterParaAgenda arquivoCarregado []

agendaParaCsvObj :: Agenda -> CsvObj
agendaParaCsvObj [] = []
agendaParaCsvObj (registro:agenda) = [[cpf, nome, telefone, email]]++(agendaParaCsvObj agenda)
    where
        cpf = cpfContato registro
        nome = nomeContato registro
        telefone = telefoneContato registro
        email = emailContato registro

salvarAgenda :: Agenda -> IO()
salvarAgenda agenda = escreverEmArquivo "agenda.csv" (agendaParaCsvObj agenda)