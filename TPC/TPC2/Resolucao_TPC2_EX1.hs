{---------------------------------------------------------------------------------

    Programação Ciber-Física 
            TPC 2

Afonso Xavier Cardoso Marques :: PG53601

---------------------------------------------------------------------------------

Considere a seguinte linguagem imperativa simples:

    Prog(X) ∋ x := t | wait_n(p) | p ; q | if b then p else q | while b do { p }

Note que t é um termo linear e n é um número natural.
O programa wait_n(p) é lido como "wait n seconds and then run program p".
São adotadas as seguintes regras semânticas:


              ⟨t, σ⟩ ⇓ r          
        ----------------------  (asg)
        ⟨x := t, σ⟩ ⇓ 0, σ[r/x] 


              ⟨p, σ⟩ ⇓ n, σ′
        ------------------------  (wait)
        ⟨waitm(p), σ⟩ ⇓ m + n, σ′ 


        ⟨p, σ⟩ ⇓ n, σ′ ⟨q, σ′⟩ ⇓ m, σ′′
        ------------------------------  (seq)
            ⟨p ; q, σ⟩ ⇓ n + m, σ′′ 


            ⟨b, σ⟩ ⇓ tt ⟨p, σ⟩ ⇓ n, σ′
        -------------------------------  (if1)
        ⟨if b then p else q, σ⟩ ⇓ n, σ′


           ⟨b, σ⟩ ⇓ ff ⟨q, σ⟩ ⇓ n, σ′
        ------------------------------  (if2)
        ⟨if b then p else q, σ⟩ ⇓ n, σ′


        ⟨b, σ⟩ ⇓ tt ⟨p, σ⟩ ⇓ n, σ′    ⟨while b do { p }, σ′⟩ ⇓ m, σ′′
        -----------------------------------------------------------  (wh1)
                   ⟨while b do { p }, σ⟩ ⇓ n + m, σ′′ 


                ⟨b, σ⟩ ⇓ ff
        ----------------------------  (wh2)
        ⟨while b do { p }, σ⟩ ⇓ 0, σ

Podemos definir uma noção natural de equivalência para os programas: dizemos que 
dois programas p e q são equivalentes (p ~ q) se para todos os ambientes σ tivermos 
⟨p, σ⟩ ⇓ n, σ′ iff ⟨q, σ⟩ ⇓ n, σ′

---------------------------------------------------------------------------------}

module TPC2_EX1 where
import Control.Concurrent

-- Implementar em Haskell a linguagem descrita acima e as suas semânticas.
-- Sugestão: use o código desenvolvido em aulas anteriores.


-- Operações aritmétricas da linguagem (soma e multiplicação)
data LTerm = Leaf (Either Vars Double) 
           | Mult Double LTerm  
           | Add LTerm LTerm
        deriving Show


-- Semântica para termos lineares
linear_sem :: LTerm -> (Vars -> Double) -> Double
linear_sem (Leaf (Left v)) m = m v
linear_sem (Leaf (Right r)) m = r

linear_sem (Mult s t) m = 
        let r = linear_sem t m 
            in s * r

linear_sem (Add t1 t2) m = 
        let r1 = linear_sem t1 m
            r2 = linear_sem t2 m 
            in r1 + r2


-- Termos booleanos (operações lógicas de menor ou igual, conjução e negação)
data BTerm = Leq LTerm LTerm
           | Conj BTerm BTerm 
           | Neg BTerm 
        deriving Show


-- Semânticas para termos booleanos
boolean_sem :: BTerm -> (Vars -> Double) -> Bool
boolean_sem (Leq t1 t2) m = 
        let r1 = linear_sem t1 m
            r2 = linear_sem t2 m
            in if r1 <= r2 then True else False

boolean_sem (Conj b1 b2) m = 
        let v1 = boolean_sem b1 m
            v2 = boolean_sem b2 m
            in v1 && v2

boolean_sem (Neg b) m = 
        let v = boolean_sem b m 
        in not v


-- Programas while (operações de assignment (:=), wait calls (wait_n); sequência de operações (p;q); if then else; while loops)
data WhileProg = Asg Vars LTerm 
               | Wc Int WhileProg
               | Seq WhileProg WhileProg    
               | IfElse BTerm WhileProg WhileProg 
               | WhLoop BTerm WhileProg
               deriving Show


-- Gestão de memória para as atribuições de valor
chMem :: Vars -> Double -> (Vars -> Double) -> (Vars -> Double)
chMem v r m u = case u of
                  x | x == v    -> r
                    | otherwise -> m x


-- Semânticas para programas while
while_sem :: WhileProg -> (Vars -> Double) -> IO (Vars -> Double)
while_sem (Asg v t) m = do
    let updatedMemory = chMem v (linear_sem t m) m
    return updatedMemory

while_sem (Seq p q) m = do
    m' <- while_sem p m
    m'' <- while_sem q m'
    return m''

while_sem (IfElse b p q) m = do
    if (boolean_sem b m)
        then while_sem p m
        else while_sem q m

while_sem (WhLoop b p) m = do
    if (boolean_sem b m)
        then do
            m' <- while_sem p m
            while_sem (WhLoop b p) m'
        else return m

while_sem (Wc s p) m = do
    putStrLn $ "Waiting for " ++ show s ++ " seconds..."
    threadDelay s   -- o threadDelay executa a espera em segundos
    while_sem p m


---------------------------------------------------------------------------------

-- Teste simples
data Vars = X deriving (Show, Eq)

-- exampleProgram:
-- inicializa a variável X com o valor 5.0 e executa um loop que espera por 5 unidades de tempo. 
-- Dentro deste loop, outro loop é executado, esperando por 2 unidades de tempo, e atualiza o 
-- valor de X adicionando 10 ao valor atual de X. O resultado final será X=15, ou seja o valor 
-- de X após a execução do programa.
exampleProgram = Seq (Asg X (Leaf (Right 5.0)))(Wc 5 ((Wc 2 ((Asg X (Add (Leaf (Left X)) (Leaf (Right 10.0))))))))

main :: IO ()
main = do
  let initialMemory = const 0.0
  updatedMemory <- while_sem exampleProgram initialMemory
  let finalValueOfX = updatedMemory X
  putStrLn ("Final value of X: " ++ show finalValueOfX ++ "\n")

---------------------------------------------------------------------------------


