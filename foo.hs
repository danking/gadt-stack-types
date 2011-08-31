{-# LANGUAGE GADTs, FlexibleInstances #-}

data Stack a where
  Empty :: () -> Stack ()
  Push :: b -> Stack a -> Stack (b,Stack a)

instance Show (Stack ()) where
  show (Empty ()) = "()"

instance (Show a, Show b) => Show (Stack (a,b)) where
  show (Push m ms) = "(" ++ show m ++ " . " ++ show ms ++ ")"


-- pushTwice :: a -> Stack s -> Stack (a, Stack (a, Stack s))
-- pushTwice x s =
--   Push x (Push x s)

-- popTwice :: Stack (a, Stack (a, Stack s)) -> (a, Stack s)
-- popTwice s =
--   case s of Push m1 (Push m2 ms) -> (m1, ms)

pop :: Stack (a, Stack s) -> (a, Stack s)
pop (Push a s) = (a, s)

push :: a -> Stack s -> Stack (a, Stack s)
push a b = Push a b

data State =
  S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8

data Intgr = Intgr
data Star = Star
data Plus = Plus
data Exp = Exp
data Term = Term

type S1Stack = Stack ()
type S2Stack a = Stack (Exp, a)
type S3Stack a = Stack (Plus, Stack (Exp, a))
type S4Stack a = Stack (Term, a)
type S5Stack a = Stack (Term, a)
type S6Stack a = Stack (Star, Stack (Term, a))
type S7Stack a = Stack (Intgr, Stack (Star, Stack (Term, a)))
type S8Stack a = Stack (Intgr, a)

s1tos2 :: S1Stack -> Stack (Exp, S1Stack)
s1tos2 s = Push Exp s

s2tos3 :: S2Stack (Stack a) -> Stack (Plus, S2Stack (Stack a))
s2tos3 s = Push Plus s

s3tos4 :: S3Stack (Stack a) -> Stack (Term, S3Stack (Stack a))
s3tos4 s = Push Term s

s3tos8 :: S3Stack (Stack a) -> Stack (Intgr, S3Stack (Stack a))
s3tos8 s = Push Intgr s

s4tos6 :: S4Stack (Stack a) -> Stack (Star, S4Stack (Stack a))
s4tos6 s = Push Star s

s1tos5 :: S1Stack -> Stack (Term, S1Stack)
s1tos5 s = Push Term s

s5tos6 :: S5Stack (Stack a) -> Stack (Star, S5Stack (Stack a))
s5tos6 s = Push Star s

s6tos7 :: S6Stack (Stack a) -> Stack (Intgr, S6Stack (Stack a))
s6tos7 s = Push Intgr s

s1tos8 :: S1Stack -> Stack (Intgr, S1Stack)
s1tos8 s = Push Intgr s

r1 :: Stack( Term ,Stack( Plus ,Stack( Exp ,Stack a))) -> Stack a
r1 s =
  let (exp,s1) = pop s
  in let (plus,s2) = pop s1
     in let (term,s3) = pop s2
        in s3

r2 :: Stack( Term ,Stack a) -> Stack a
r2 s =
  let (term, s1) = pop s
  in s1

r3 :: Stack( Intgr ,Stack( Star ,Stack( Term ,Stack a))) -> Stack a
r3 s =
  let (term,s1) = pop s
  in let (star,s2) = pop s1
     in let (intgr,s3) = pop s2
        in s3

r4 :: Stack( Intgr ,Stack a) -> Stack a
r4 s =
  let (intgr,s1) = pop s
  in s1
