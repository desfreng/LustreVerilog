{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Verilog.BaseLibrary (addRequirements) where

import Commons.Ids (Ident (Ident))
import Commons.Types (BVSize (BVSize))
import Compiling.Ast (CBinOp (..), CUnOp (..))
import Data.Foldable
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Verilog.Ast

base_suffix :: ModuleImport
base_suffix = ".v"

computeReqs :: Ast -> Set BaseModule
computeReqs (Ast _ m _) = foldMap (computeModuleReqs . moduleBody) m

computeModuleReqs :: NonEmpty Expr -> Set BaseModule
computeModuleReqs l = Set.fromList . catMaybes $ computeExprReqs <$> toList l

computeExprReqs :: Expr -> Maybe BaseModule
computeExprReqs (AssignExpr _ _) = Nothing
computeExprReqs (InstExpr ModuleInst {name = Base m}) = Just m
computeExprReqs (InstExpr ModuleInst {name = Custom _}) = Nothing
computeExprReqs (InstExpr ModuleInst {name = MainModule}) = Nothing

addRequirements :: Ast -> Ast
addRequirements ast =
  let reqs = Set.toList $ computeReqs ast
      reqsMod = buildModule <$> reqs
      reqsSigs = foldMap ((\m -> Map.singleton (moduleName m) m) . fst) reqsMod
   in ast <> Ast reqsSigs [] (snd <$> reqsMod)

buildModule :: BaseModule -> (ModuleHead, ModuleImport)
buildModule (UnOpModule op) = buildUnOpModules op
buildModule (BinOpModule op) = binOpModules op
buildModule FbyModule = fbyModule
buildModule IfModule = ifModule
buildModule ConcatModule = concatModule
buildModule SliceModule = sliceModule
buildModule SelectModule = selectModule

buildUnOpModules :: CUnOp -> (ModuleHead, ModuleImport)
buildUnOpModules op = (moduleHead, show (UnOpModule op) <> base_suffix)
  where
    size = Ident "N"

    moduleHead =
      ModuleHead
        { moduleName = Base $ UnOpModule op,
          staticVars = [StaticDecl size (Just $ BVSize 1)],
          controlVars = Nothing,
          inputVars = [WireDecl (VariableSize size) $ Ident "arg"],
          outputVars = NonEmpty.singleton $ WireDecl (VariableSize size) $ Ident "res"
        }

binOpInSize :: Ident
binOpInSize = Ident "N"

buildBinOpModule :: CBinOp -> VarSize -> (ModuleHead, ModuleImport)
buildBinOpModule op outSize = (moduleHead, show (BinOpModule op) <> base_suffix)
  where
    moduleHead =
      ModuleHead
        { moduleName = Base $ BinOpModule op,
          staticVars = [StaticDecl binOpInSize (Just $ BVSize 1)],
          controlVars = Nothing,
          inputVars =
            [ WireDecl (VariableSize binOpInSize) $ Ident "lhs",
              WireDecl (VariableSize binOpInSize) $ Ident "rhs"
            ],
          outputVars = NonEmpty.singleton $ WireDecl outSize $ Ident "res"
        }

binOpModules :: CBinOp -> (ModuleHead, ModuleImport)
binOpModules op@CBinEq = buildBinOpModule op (FixedSize (BVSize 1))
binOpModules op@CBinSignedLt = buildBinOpModule op (FixedSize (BVSize 1))
binOpModules op@CBinUnsignedLt = buildBinOpModule op (FixedSize (BVSize 1))
binOpModules op@CBinAdd = buildBinOpModule op (VariableSize binOpInSize)
binOpModules op@CBinSub = buildBinOpModule op (VariableSize binOpInSize)
binOpModules op@CBinAnd = buildBinOpModule op (VariableSize binOpInSize)
binOpModules op@CBinOr = buildBinOpModule op (VariableSize binOpInSize)

fbyModule :: (ModuleHead, ModuleImport)
fbyModule = (moduleHead, show FbyModule <> base_suffix)
  where
    size = Ident "N"
    clockName = Ident "clock"
    initName = Ident "init"

    moduleHead =
      ModuleHead
        { moduleName = Base FbyModule,
          staticVars = [StaticDecl size (Just $ BVSize 1)],
          controlVars =
            Just
              ModuleControl
                { clockVar = WireDecl (FixedSize $ BVSize 1) clockName,
                  initVar = WireDecl (FixedSize $ BVSize 1) initName
                },
          inputVars =
            [ WireDecl (VariableSize size) $ Ident "init_val",
              WireDecl (VariableSize size) $ Ident "next_val"
            ],
          outputVars = NonEmpty.singleton $ WireDecl (VariableSize size) $ Ident "res"
        }

ifModule :: (ModuleHead, ModuleImport)
ifModule = (moduleHead, show IfModule <> base_suffix)
  where
    size = Ident "N"

    moduleHead =
      ModuleHead
        { moduleName = Base IfModule,
          staticVars = [StaticDecl size (Just $ BVSize 1)],
          controlVars = Nothing,
          inputVars =
            [ WireDecl (FixedSize $ BVSize 1) $ Ident "cond",
              WireDecl (VariableSize size) $ Ident "true_branch",
              WireDecl (VariableSize size) $ Ident "false_branch"
            ],
          outputVars = NonEmpty.singleton $ WireDecl (VariableSize size) $ Ident "res"
        }

concatModule :: (ModuleHead, ModuleImport)
concatModule = (moduleHead, show ConcatModule <> base_suffix)
  where
    lhsSize = Ident "M"
    rhsSize = Ident "N"

    moduleHead =
      ModuleHead
        { moduleName = Base ConcatModule,
          staticVars = [StaticDecl lhsSize (Just $ BVSize 1), StaticDecl rhsSize (Just $ BVSize 1)],
          controlVars = Nothing,
          inputVars =
            [ WireDecl (VariableSize lhsSize) $ Ident "lhs",
              WireDecl (VariableSize rhsSize) $ Ident "rhs"
            ],
          outputVars = NonEmpty.singleton $ WireDecl (SumSize lhsSize rhsSize) $ Ident "res"
        }

sliceModule :: (ModuleHead, ModuleImport)
sliceModule = (moduleHead, show SliceModule <> base_suffix)
  where
    size = Ident "N"
    fstIndex = Ident "I"
    sndIndex = Ident "J"

    moduleHead =
      ModuleHead
        { moduleName = Base SliceModule,
          staticVars =
            [ StaticDecl size (Just $ BVSize 1),
              StaticDecl fstIndex (Just $ BVSize 0),
              StaticDecl sndIndex (Just $ BVSize 1)
            ],
          controlVars = Nothing,
          inputVars = [WireDecl (VariableSize size) $ Ident "arg"],
          outputVars = NonEmpty.singleton $ WireDecl (DeltaSize fstIndex sndIndex) $ Ident "res"
        }

selectModule :: (ModuleHead, ModuleImport)
selectModule = (moduleHead, show SelectModule <> base_suffix)
  where
    size = Ident "N"
    arg = Ident "I"

    moduleHead =
      ModuleHead
        { moduleName = Base SelectModule,
          staticVars = [StaticDecl size (Just $ BVSize 1), StaticDecl arg (Just $ BVSize 0)],
          controlVars = Nothing,
          inputVars = [WireDecl (VariableSize size) $ Ident "arg"],
          outputVars = NonEmpty.singleton $ WireDecl (FixedSize $ BVSize 1) $ Ident "res"
        }
