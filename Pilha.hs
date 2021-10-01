module Pilha where

data PilhaData a = Pilha [a] deriving Show

pilhaVazia :: PilhaData a
pilhaVazia = Pilha []

isEmpty :: PilhaData a -> Bool
isEmpty (Pilha [])  = True
isEmpty _ = False

pop :: PilhaData a -> PilhaData a
pop (Pilha [])    = Pilha []
pop (Pilha (cabeca:lista)) = Pilha lista

top :: PilhaData a -> a
top (Pilha []) = error "Pilha vazia"
top (Pilha (cabeca:lista)) = cabeca

push :: a -> PilhaData a -> PilhaData a
push elemento (Pilha lista) = Pilha (elemento:lista)