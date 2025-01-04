{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Typing.NodeEnv
  ( NodeEnv (),
    addNode,
    runNodeEnv,
    toNodeEnv,
  )
where

import Commons.Ast
import Commons.Ids
import Commons.Position
import Commons.TypingError
import Control.Monad.State.Strict
import Control.Monad.Writer
import qualified Data.List as List
import Data.Map.Lazy ((!))
import qualified Data.Map.Lazy as Map
import Typing.Ast
import Typing.Environments

type NodeWriter = CanFail [(NodeIdent, TNode)]

newtype NodeEnv a = NodeEnv (StateT NodeMapping (Writer NodeWriter) a)
  deriving (Functor, Applicative, Monad)

validateNode :: TNode -> CanFail TNode
validateNode = pure

addNode :: Pos Ident -> CanFail TNode -> NodeEnv ()
addNode nName nNode =
  let nodeId = NodeIdent nName
   in do
        nMap <- NodeEnv get
        case Map.lookup nodeId nMap of
          Just _ -> writeError $ "Node " <> show nName <> " is already declared."
          Nothing -> addNodeMapping nodeId nMap >> writeNode nodeId
  where
    writeError err = NodeEnv . tell $ addError nNode nName err
    addNodeMapping nId nMap = NodeEnv . put $ Map.insert nId (buildSig <$> nNode) nMap
    writeNode nId =
      let vNode = collapse $ validateNode <$> nNode
       in NodeEnv . tell $ List.singleton . (,) nId <$> vNode

    buildSig :: TNode -> NodeSignature
    buildSig (Node n _) =
      let varTyp = nodeVarTypes n
          inputTyp = (!) varTyp . FromIdent <$> nodeInput n
          outputTyp = (!) varTyp . FromIdent <$> nodeOutput n
       in NodeSignature (length inputTyp) inputTyp outputTyp

runNodeEnv :: NodeEnv () -> CanFail TAst
runNodeEnv (NodeEnv m) =
  let (((), s), w) = runWriter $ runStateT m Map.empty in Ast <$> sequenceA s <*> w

toNodeEnv :: (NodeMapping -> a) -> NodeEnv a
toNodeEnv = NodeEnv . gets