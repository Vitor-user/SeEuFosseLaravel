module Stack(Stack, empty, isEmpty, push, top, pop) where

data Stack a = Stack [a] deriving Show

empty :: Stack a
empty = Stack []

isEmpty :: Stack a -> Bool
isEmpty (Stack []) = True
isEmpty _          = False

push :: Stack a -> a -> Stack a
push (Stack stack) element = Stack (element:stack)

top :: Stack a -> a
top (Stack [])    = error "Stack empty"
top (Stack (a:b)) = a

pop :: Stack a -> Stack a
pop (Stack [])    = error "Stack empty"
pop (Stack (a:b)) = Stack b