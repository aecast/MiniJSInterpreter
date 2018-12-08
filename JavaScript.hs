module JavaScript where

  import Prelude hiding (LT, GT, EQ, id)
  import Control.Monad
  import Operators
  
  data Checked a = Good a | Error String
    deriving Show
  
  data Value = IntV  Int
             | BoolV Bool
             | ClosureV String Exp Env
             | AddressV Int        -- new
             | RecordV Env
             | Undefined
    deriving (Eq, Show)
  
  type Memory = [Value]
  
  type Stateful t = Memory -> (t, Memory)
  
  data Exp = Literal   Value
           | Unary     UnaryOp Exp
           | Binary    BinaryOp Exp Exp
           | If        Exp Exp Exp
           | Variable  String
           | Declare   String Exp Exp
           | Function  String Exp
           | Call      Exp Exp
           | Seq       Exp Exp
           | Assign    Exp Exp   -- new
           | TryCatch  Exp Exp
           | Record    [(String, Exp)]
           | Field     Exp String
    deriving (Eq, Show)
    
  type Env = [(String, Value)]
  
  data CheckedStateful t = CST (Memory -> (Checked t, Memory))
  
  instance Functor CheckedStateful where
    fmap  = liftM
    
  instance Applicative CheckedStateful where
    pure val = checkedToCST (Good val)
    (<*>) = ap 
  
  instance Monad CheckedStateful where
    return = pure
    (CST c) >>= f = 
      CST (\m -> 
        let (ch, m') = c m in
          case ch of 
            Error msg -> (Error msg, m')
            Good val -> 
              let CST f' = f val in
                f' m'
        )
  
  evalObj :: [(String, Exp)] -> Env -> CheckedStateful Value
  evalObj [] env = return (RecordV [])
  evalObj ((xstr, xexp) : xs) env = do
    vexp <- evaluate xexp env
    RecordV newXS <- evalObj(xs) env
    return (RecordV ((xstr, vexp) : newXS))
  
  doLookup :: Value -> String -> Value
  doLookup (RecordV ((id, exp) : fields)) str =
    if id == str then exp
    else
      case fields of
        (x:xs) -> doLookup (RecordV (x:xs)) str
        []     -> Undefined
      
    
  -- checkAddress :: Value -> Value
  -- checkAddress v = case v of
  --   AddressV i -> readMemory i
  --   _          -> v
  
  
  lookupField :: Value -> String -> CheckedStateful Value
  lookupField val str = do
    case doLookup val str of
      IntV i              -> return (IntV i)
      BoolV b             -> return (BoolV b)
      ClosureV x body env -> return (ClosureV x body env)
      AddressV i          -> readMemory i
      Undefined           -> case doLookup val "prototype" of
        RecordV r -> return $ doLookup (RecordV r) str
        Undefined -> return Undefined
  
  evaluate :: Exp -> Env -> CheckedStateful Value
  -- basic operations
  evaluate (Literal l) env = return l
  evaluate (Unary op a) env = do
    av <- evaluate a env
    checkedToCST $ checked_unary op av
  evaluate (Binary op a b) env = do
    av <- evaluate a env
    bv <- evaluate b env
    checkedToCST $ checked_binary op av bv
  evaluate (If a b c) env = do
    av <- evaluate a env
    case av of
      BoolV cond -> evaluate (if cond then b else c) env
      _ -> myError ("Expected boolean but found " ++ show av)
  -- variables and declarations
  evaluate (Variable x) env    =
    case lookup x env of
      Nothing -> myError ("Variable " ++ x ++ " undefined")
      Just v  ->
        case v of 
          AddressV i -> readMemory i
          _ -> return v
  
  evaluate (Declare x e body) env = do    -- non-recursive case
    ev <- evaluate e env
    a <- newMemory ev
    let newEnv = (x, a) : env
    evaluate body newEnv
  
  evaluate (Record r) env = evalObj r env

  evaluate (Call (Field obj method) arg) env = do
    ov <- evaluate obj env
    av <- evaluate arg env
    case ov of
      RecordV r -> do
        fun <- lookupField ov method
        case fun of
          ClosureV x body closeEnv -> let newEnv = ("this", ov) : (x, av) : closeEnv in
            evaluate body newEnv
          _ -> myError ("Expected function but found " ++ show fun)
      _ -> myError ("Expected object but found " ++ show ov)
  
  -- try catch
  evaluate (TryCatch e c) env =
    handleCatch (evaluate e env) (evaluate c env)
  
  -- function definitions and function calls
  evaluate (Function x body) env = return (ClosureV x body env)
  evaluate (Call fun arg) env = do
    funv <- evaluate fun env
    case funv of
      ClosureV x body closeEnv -> do
        argv <- evaluate arg env
        let newEnv = (x, argv) : closeEnv
        evaluate body newEnv
      _ -> myError ("Expected function but found " ++ show funv)
  
  evaluate (Field obj f) env = do
    ov <- evaluate obj env
    case ov of
      RecordV r -> lookupField ov f
      _ -> myError ("Expected object but found " ++ show ov)
  
  -- mutation operations
  evaluate (Seq a b) env = do
    evaluate a env
    evaluate b env
            
  evaluate (Assign a e) env = do
    v <- leftEvaluate a env
    case v of  
      AddressV i -> do 
        ev <- evaluate e env
        updateMemory ev i
      _ -> myError ("Can't assign to " ++ show v)
  
  leftEvaluate (Variable x) env    =
    case lookup x env of
      Nothing -> myError ("Variable " ++ x ++ " undefined")
      Just v  -> return v
  leftEvaluate exp _ = myError ("Can't assign to " ++ show exp)
  
  handleCatch (CST f) v2 = CST(\m->
    let (cv, m') = f m in
      case cv of
        Good v -> (Good v, m')
        Error m -> let CST f = v2 in f m')
  
  checkedToCST ch = CST (\m-> (ch, m))
  myError msg = checkedToCST (Error msg)
  
  -- primitive helper functions to access and update memory 
  getMemory = CST( \m -> (Good m, m))
  putMemory m = CST( \_ -> (Good Undefined, m))
  
  -- highlevel helper functions
  newMemory val = do
    mem <- getMemory
    putMemory (mem ++ [val])
    return (AddressV (length mem))
  
  readMemory i = do
    mem <- getMemory
    if 0 <= i && i < length mem
    then return (mem !! i)
    else myError "Access to invalid memory address"
  
  updateMemory val i = do
    mem <- getMemory
    if 0 <= i && i < length mem
    then putMemory (update i val mem)
    else myError "Update of invalid memory address"
    
  update :: Int -> Value -> Memory -> Memory
  update i val mem =
    let (before, _ : after) = splitAt i mem in
      before ++ [val] ++ after
  
  runStateful (CST c) = 
     let (val, mem) = c [] in val
  
  checked_unary :: UnaryOp -> Value -> Checked Value
  checked_unary Not (BoolV b) = Good (BoolV (not b))
  checked_unary Neg (IntV i)  = Good (IntV (-i))
  checked_unary op   v         = 
      Error ("Unary " ++ show op ++ " called with invalid argument " ++ show v)
  
  checked_binary :: BinaryOp -> Value -> Value -> Checked Value
  checked_binary Add (IntV a)  (IntV b)  = Good (IntV (a + b))
  checked_binary Sub (IntV a)  (IntV b)  = Good (IntV (a - b))
  checked_binary Mul (IntV a)  (IntV b)  = Good (IntV (a * b))
  checked_binary Div _         (IntV 0)  = Error "Divide by zero"
  checked_binary Div (IntV a)  (IntV b)  = Good (IntV (a `div` b))
  checked_binary And (BoolV a) (BoolV b) = Good (BoolV (a && b))
  checked_binary Or  (BoolV a) (BoolV b) = Good (BoolV (a || b))
  checked_binary LT  (IntV a)  (IntV b)  = Good (BoolV (a < b))
  checked_binary LE  (IntV a)  (IntV b)  = Good (BoolV (a <= b))
  checked_binary GE  (IntV a)  (IntV b)  = Good (BoolV (a >= b))
  checked_binary GT  (IntV a)  (IntV b)  = Good (BoolV (a > b))
  checked_binary EQ  a         b         = Good (BoolV (a == b))
  checked_binary op  a         b         = 
      Error ("Binary " ++ show op ++ 
             " called with invalid arguments " ++ show a ++ ", " ++ show b)
  
  
  