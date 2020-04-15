module Interpreter where
import Control.Arrow (second)
import Control.Monad (liftM)
import Control.Monad.Except (liftIO, throwError)
import Control.Monad.Trans.Except
import Environment
import Error
import Value

-- |Interpreter holding an execution environment and the result of a
-- computation.
newtype Interp e a = Interp { runInterp :: e -> IOThrowsError (e, a) }

-- |When the interpreter must enter an invalid state, this function make it
-- |return the right error.
failWith :: YmirError -> Interp e a
failWith err = Interp $ \_ -> throwError err

environment :: Interp Env Env
environment = Interp $ \e -> return (e, e)

instance Functor (Interp e) where
  fmap f i = Interp (fmap (fmap f) . runInterp i)

instance Applicative (Interp e) where
  pure a = Interp $ \e -> return (e, a)

  fi <*> ai = Interp $ \e -> do
    (e2, f) <- runInterp fi e
    (e3, a) <- runInterp ai e2
    return (e3, f a)

instance Monad (Interp e) where
  return = pure

  mi >>= f = Interp $ \e -> do
    (e2, a) <- runInterp mi e
    runInterp (f a) e2

-- |Read a variable from the interpreter's environment.
readVar :: String -> Interp Env YmirValue
readVar var = Interp $ \e -> do
  val <- liftThrows $ getVar e var
  return (e, val)

-- |Write into an existing variable in the interpreter's environment.
writeVar :: String -> YmirValue -> Interp Env ()
writeVar var val = Interp $ \e -> do
  e2 <- liftThrows $ setVar e var val
  return (e2, ())

-- |Define a new variable in the interpreter's environment.
defineNewVar :: String -> YmirValue -> Interp Env ()
defineNewVar var val = Interp $ \e -> do
  e2 <- liftThrows $ defineVar e var val
  return (e2, ())

eval :: YmirValue -> Interp Env YmirValue
eval val@(String _) = return val
eval val@(Char _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (Atom id) = readVar id
eval (DottedList x xs) = do
  x' <- mapM eval x
  xs' <- eval xs
  return $ DottedList x' xs'
eval (List [Atom "define", Atom var, form]) = do
  val <- eval form
  defineNewVar var val
  return val
eval badForm = failWith $ BadSpecialForm "Unrecognized special form" badForm
