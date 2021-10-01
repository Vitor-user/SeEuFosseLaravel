module Questao3 where

import Agenda
import Utils
import TiposAgenda

main :: IO()
main =
    do
        agenda <- carregarAgenda
        loopPrincipal agenda


loopPrincipal :: Agenda -> IO()
loopPrincipal agenda =
    do
        mostrarMenu
        opcao <- getLine
        case opcao of
            "1" -> do
                agendaAlterada <- cadastrarContato agenda
                salvarAgenda agendaAlterada
                loopPrincipal agendaAlterada
            "2" -> do
                listarContatos agenda
                loopPrincipal agenda
            "3" -> do
                buscarContato agenda
                loopPrincipal agenda
            "4" -> do
                agendaAlterada <- atualizarContato agenda
                salvarAgenda agendaAlterada
                loopPrincipal agendaAlterada
            "5" -> do
                agendaAlterada <- apagarContato agenda
                salvarAgenda agendaAlterada
                loopPrincipal agendaAlterada
            "0" -> return ()
            _ -> loopPrincipal agenda --(mostrarErro agenda)

mostrarMenu :: IO()
mostrarMenu =
    do
        putStrLn "\n******* MENU *******\n"
        putStrLn "\t[1] Cadastrar contato"
        putStrLn "\t[2] Mostrar Contatos"
        putStrLn "\t[3] Procurar Contato"
        putStrLn "\t[4] Atualizar Contato"
        putStrLn "\t[5] Apagar contato"
        putStrLn "\t[0] Sair"
        putStr "Digite o numero desejado: "

receberEntrada :: String -> IO String
receberEntrada texto =
    do
        putStrLn texto
        getLine


cadastrarContato :: Agenda -> IO Agenda
cadastrarContato agenda =
    do
        cpf <- receberEntrada "Digite seu CPF: "
        nome <- receberEntrada "Digite seu nome: "
        telefone <- receberEntrada "Digite seu telefone: "
        email <- receberEntrada "Digite seu e-mail: "

        if validarCpf cpf then
            do
                agendaAlterada <- adicionarContato (criarContato cpf nome telefone email) agenda
                salvarAgenda agendaAlterada
                return agendaAlterada
        else
            do
                putStr "\n======= Cpf invalido! Inscricao indeferida ======\n"
                return agenda

buscarContato :: Agenda -> IO()
buscarContato agenda =
    do
        opcao <- receberEntrada "\n[0] Pesquisar por nome\n[1] Pesquisar por CPF\nDigite sua opcao: "
        case opcao of
            "0" -> do
                nome <- receberEntrada "Digite o nome: "
                putStrLn (mostrarContato (agenda !! pesquisarContatoPorNome nome agenda) (-1))
            "1" -> do
                cpf <- receberEntrada "Digite o CPF: "
                putStrLn (mostrarContato (agenda !! pesquisarContatoPorCpf cpf agenda) (-1))

pedirPraMudarContato :: ContatoData -> IO ContatoData
pedirPraMudarContato (Contato cpf nome telefone email) = 
    do
        putStrLn "O que voce pode mudar:"
        putStrLn "\t[1] Nome"
        putStrLn "\t[2] Telefone"
        putStrLn "\t[3] Email"
        opcao <- receberEntrada "Digite a opcao desejada: "
        case opcao of
            "1" -> do
                novoNome <- receberEntrada "Digite o novo nome: "
                let contato = Contato cpf novoNome telefone email
                putStrLn (mostrarContato contato (-1))
                pedirPraMudarContato contato
            "2" -> do
                novoTelefone <- receberEntrada "Digite o novo Telefone: "
                let contato = Contato cpf novoTelefone telefone email
                putStrLn (mostrarContato contato (-1))
                pedirPraMudarContato contato
            "3" -> do
                novoEmail <- receberEntrada "Digite o novo Email: "
                let contato = Contato cpf novoEmail telefone email
                putStrLn (mostrarContato contato (-1))
                pedirPraMudarContato contato
            _ -> do
                putStrLn "Entrada invalida!"
                pedirPraMudarContato (Contato cpf nome telefone email)
            

atualizarContato :: Agenda -> IO Agenda
atualizarContato agenda =
    do
        putStrLn "\n========--  ALTERAR CONTATO --========="
        putStrLn "==== Listagem de contatos ===="
        listarContatos agenda
        putStrLn "Opcoes disponiveis:"
        putStrLn "\t[0] Alterar por nome"
        putStrLn "\t[1] Alterar por CPF"
        putStrLn "\t[2] Alterar por posicao"
        opcao <- receberEntrada "Digite sua opcao: "
        case opcao of
            "0" -> do
                nome <- receberEntrada "\nDigite seu nome: "
                let contato = pesquisarContatoPorNome nome agenda
                if contato == -1 then
                    do
                        putStrLn "\n===== CONTATO NAO ENCONTRADO ==== \n"
                        return agenda
                else
                    do
                        putStrLn (mostrarContato (agenda !! contato) (-1))
                        novoContato <- pedirPraMudarContato (agenda !! contato)
                        return (alterarContatoPorNome novoContato agenda nome)
            "1" -> do
                cpf <- receberEntrada "\nDigite seu CPF: "
                let contato = pesquisarContatoPorCpf cpf agenda
                if contato == -1 then
                    do
                        putStrLn "\n===== CONTATO NAO ENCONTRADO ==== \n"
                        return agenda
                else
                    do
                        putStrLn (mostrarContato (agenda !! contato) (-1))
                        novoContato <- pedirPraMudarContato (agenda !! contato)
                        return (alterarContatoPorNome novoContato agenda cpf)
            "2" -> do
                posicao <- receberEntrada "\nDigite a posicao: "
                let posicaoInt = read posicao
                if (posicaoInt >= length agenda) || (posicaoInt < 0) then
                    do
                        putStrLn "\n===== CONTATO NAO ENCONTRADO ==== \n"
                        return agenda
                else
                    do
                        putStrLn (mostrarContato (agenda !! posicaoInt) (-1))
                        novoContato <- pedirPraMudarContato (agenda !! posicaoInt)
                        return (alterarContatoPorPosicao novoContato agenda posicaoInt)
            _ -> return agenda

apagarContato :: Agenda -> IO Agenda
apagarContato agenda =
    do
        putStrLn "\n========--  APAGAR CONTATO --========="
        putStrLn "==== Listagem de contatos ===="
        listarContatos agenda
        putStrLn "Opcoes disponiveis:"
        putStrLn "\t[0] Apagar por nome"
        putStrLn "\t[1] Apagar por CPF"
        putStrLn "\t[2] Apagar por posicao"
        opcao <- receberEntrada "Digite sua opcao: "
        case opcao of
            "0" -> do
                nome <- receberEntrada "\nDigite o nome: "
                return (apagarContatoPorNome nome agenda)
            "1" -> do
                cpf <- receberEntrada "\nDigite o CPF: "
                return (apagarContatoPorCpf cpf agenda)
            "2" -> do
                posicao <- receberEntrada "\nDigite a posicao: "
                return (apagarContatoPorPosicao (read posicao) agenda)
            _ -> return agenda





