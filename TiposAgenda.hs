module TiposAgenda where

type CPF = String
type Nome = String
type Telefone = String
type Email = String
type Dado = String
data ContatoData = Invalido | Contato CPF Nome Telefone Email deriving Show
type Agenda = [ContatoData]

cpfContato :: ContatoData -> CPF
cpfContato (Contato cpf _ _ _) = cpf

nomeContato :: ContatoData -> Nome
nomeContato (Contato _ nome _ _) = nome

telefoneContato :: ContatoData -> Telefone
telefoneContato (Contato _ _ telefone _) = telefone

emailContato :: ContatoData -> Email
emailContato (Contato _ _ _ email) = email