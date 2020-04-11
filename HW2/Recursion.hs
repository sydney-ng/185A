module Recursion where

-- Type for WFF of propositional logic.
data WFF = T
         | F
         | Neg WFF
         | Conj WFF WFF
         | Disj WFF WFF
         deriving Show

-- Number type.
data Numb = E
          | S Numb
          deriving Show

-- Short-hands.
zero = E
one = S zero
two = S one
three = S two
four = S three
five = S four

isOne :: Numb -> Bool
isOne E = False
isOne (S n) = case n of E -> True
                        S n' -> False

add :: Numb -> Numb -> Numb
add E n = n
add (S m) n = S (add m n)

double :: Numb -> Numb
double E = E
double (S n) = S (S (double n))
