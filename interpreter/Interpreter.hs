module Interpreter (run) where

-- global imports
import Control.Monad.State
import Control.Monad.Except

-- local imports
import AbsTiny
import ErrM
import LexTiny
import ParTiny

type IState = Ident -> Maybe Integer

type InterpreterM a = ExceptT String (StateT IState IO) a

getValue :: Ident -> InterpreterM Integer
getValue x = do
    s <- get
    case (s x) of
        Nothing -> throwError "use of undeclared variable"
        Just n  -> return n

-- integer expressions
evalExp :: Exp -> InterpreterM Integer

evalExp (ENum n) = return n

evalExp (EVar x) = do
    getValue x

evalExp (EAdd e1 e2) = do
    n1 <- evalExp e1
    n2 <- evalExp e2
    return $ n1 + n2

evalExp (ESub e1 e2) = do
    n1 <- evalExp e1
    n2 <- evalExp e2
    return $ n1 - n2

evalExp (EMul e1 e2) = do
    n1 <- evalExp e1
    n2 <- evalExp e2
    return $ n1 * n2

evalExp (EDiv e1 e2) = do
    n1 <- evalExp e1
    n2 <- evalExp e2
    if n2 == 0
        then throwError "attempted division by zero"
        else return $ n1 `quot` n2

evalExp (EMod e1 e2) = do
    n1 <- evalExp e1
    n2 <- evalExp e2
    if n2 == 0
        then throwError "attempted division by zero"
        else return $ n1 `mod` n2

-- boolean expressions
evalBExp :: BExp -> InterpreterM Bool

evalBExp BTrue = return True

evalBExp BFalse = return False

evalBExp (BLeq e1 e2) = do
    n1 <- evalExp e1
    n2 <- evalExp e2
    return (n1 <= n2)

evalBExp (BNeg b) = do
    vb <- evalBExp b
    return $ not vb

evalBExp (BAnd b1 b2) = do
    vb1 <- evalBExp b1
    vb2 <- evalBExp b2
    return $ vb1 && vb2

-- statements
runStm :: Stm -> InterpreterM ()

runStm SSkip = return ()

runStm (SAss x e) = do
    n <- evalExp e
    s <- get
    let s' = (\y -> if y == x then Just n else s y)
    put s'

runStm (SIfel b s1 s2) = do
    vb <- evalBExp b
    if vb then
        runStm s1
    else
        runStm s2

runStm loop@(SWhile b s) = do
    vb <- evalBExp b
    if vb then
        do runStm s
           runStm loop
    else
        return ()

runStm (SPrint e) = do
    n <- evalExp e
    liftIO $ putStrLn $ show n

runStm (SBlock []) = return ()
runStm (SBlock (stm:stms)) = do
    runStm stm
    runStm (SBlock stms)

-- the exported function
run :: Prog -> IO ()
run' (Program stm) = fmap fst $ (flip runStateT) (\_ -> Nothing) $
             runExceptT $ runStm stm 
run p = do
    result <- run' p
    case result of
        Left err -> putStrLn err
        Right () -> return ()
