{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LispVal (
   LispVal(..),
   ) where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Map as Map

data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool

data IFunc = IFunc { fn :: [LispVal] -> Eval LispVal }
type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval { unEval :: ReaderT EnvCtx IO a }
  deriving ( Monad
           , Functor
           , Applicative
           , MonadReader EnvCtx
           , MonadIO)
