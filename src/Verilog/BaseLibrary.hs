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
computeExprReqs (VarExpr _ _) = Nothing
computeExprReqs (ConstExpr _ _) = Nothing
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
buildModule (UnOpModule op@CUnNot) = buildUnOpModules (UnOpModule op) Unsigned
buildModule (UnOpModule op@CUnNeg) = buildUnOpModules (UnOpModule op) Signed
buildModule (BinOpModule op) = binOpModules op
buildModule FbyModule = fbyModule
buildModule IfModule = ifModule

buildUnOpModules :: BaseModule -> VarType -> (ModuleHead, ModuleImport)
buildUnOpModules name kind = (moduleHead, show name <> base_suffix)
  where
    size = Ident "N"

    moduleHead =
      ModuleHead
        { moduleName = Base name,
          staticVars = [StaticDecl size (Just $ BVSize 1)],
          controlVars = Nothing,
          inputVars = [WireDecl kind (VariableSize size) $ Ident "arg"],
          outputVars = NonEmpty.singleton $ WireDecl kind (VariableSize size) $ Ident "res"
        }

binOpInSize :: Ident
binOpInSize = Ident "N"

buildBinOpModule :: BaseModule -> VarType -> VarSize -> (ModuleHead, ModuleImport)
buildBinOpModule name kind outSize = (moduleHead, show name <> base_suffix)
  where
    moduleHead =
      ModuleHead
        { moduleName = Base name,
          staticVars = [StaticDecl binOpInSize (Just $ BVSize 1)],
          controlVars = Nothing,
          inputVars =
            [ WireDecl kind (VariableSize binOpInSize) $ Ident "lhs",
              WireDecl kind (VariableSize binOpInSize) $ Ident "rhs"
            ],
          outputVars = NonEmpty.singleton $ WireDecl kind outSize $ Ident "res"
        }

binOpModules :: CBinOp -> (ModuleHead, ModuleImport)
binOpModules op@CBinEq =
  buildBinOpModule (BinOpModule op) Unsigned (FixedSize (BVSize 1))
binOpModules op@CBinSignedLt =
  buildBinOpModule (BinOpModule op) Signed (FixedSize (BVSize 1))
binOpModules op@CBinSignedGe =
  buildBinOpModule (BinOpModule op) Signed (FixedSize (BVSize 1))
binOpModules op@CBinUnsignedLt =
  buildBinOpModule (BinOpModule op) Unsigned (FixedSize (BVSize 1))
binOpModules op@CBinUnsignedGe =
  buildBinOpModule (BinOpModule op) Unsigned (FixedSize (BVSize 1))
binOpModules op@CBinAdd =
  buildBinOpModule (BinOpModule op) Unsigned (VariableSize binOpInSize)
binOpModules op@CBinSub =
  buildBinOpModule (BinOpModule op) Signed (VariableSize binOpInSize)
binOpModules op@CBinAnd =
  buildBinOpModule (BinOpModule op) Unsigned (VariableSize binOpInSize)
binOpModules op@CBinOr =
  buildBinOpModule (BinOpModule op) Unsigned (VariableSize binOpInSize)

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
                { clockVar = WireDecl Unsigned (FixedSize $ BVSize 1) clockName,
                  initVar = WireDecl Unsigned (FixedSize $ BVSize 1) initName
                },
          inputVars =
            [ WireDecl Unsigned (VariableSize size) $ Ident "init_val",
              WireDecl Unsigned (VariableSize size) $ Ident "next_val"
            ],
          outputVars = NonEmpty.singleton $ WireDecl Unsigned (VariableSize size) $ Ident "res"
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
            [ WireDecl Unsigned (FixedSize $ BVSize 1) $ Ident "cond",
              WireDecl Unsigned (VariableSize size) $ Ident "true_branch",
              WireDecl Unsigned (VariableSize size) $ Ident "false_branch"
            ],
          outputVars = NonEmpty.singleton $ WireDecl Unsigned (VariableSize size) $ Ident "res"
        }
