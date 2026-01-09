{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Verilog.BaseLibrary (addRequirements) where

import Commons.Ids (Ident (Ident), SizeIdent (..))
import Commons.Size (Size, constantSize, subVar, sumVar, varSize)
import Compiling.Ast (CBinOp (..), CUnOp (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Verilog.Ast

baseSuffix :: ModuleImport
baseSuffix = ".v"

computeReqs :: Ast -> Set BaseModule
computeReqs (Ast _ m _) = foldMap computeModuleReqs m
  where
    computeModuleReqs :: Module -> Set BaseModule
    computeModuleReqs Module {moduleBody} = foldMap computeBodyReqs moduleBody

    computeBodyReqs :: ModuleSection -> Set BaseModule
    computeBodyReqs ModuleSection {sectionBody} =
      foldMap computeExprReqs sectionBody

    computeExprReqs :: Expr -> Set BaseModule
    computeExprReqs (AssignExpr _ _) = mempty
    computeExprReqs (InstExpr ModuleInst {name = Base bModule}) = Set.singleton bModule
    computeExprReqs (InstExpr ModuleInst {name = Custom _}) = mempty
    computeExprReqs (InstExpr ModuleInst {name = MainModule}) = mempty

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
buildUnOpModules op = (mHead, show (UnOpModule op) <> baseSuffix)
  where
    size = SizeIdent $ Ident "N"

    mHead =
      ModuleHead
        { moduleName = Base $ UnOpModule op,
          moduleSize = [size],
          controlVars = Nothing,
          inputVars = [WireDecl (varSize size) $ Ident "arg"],
          outputVars = NonEmpty.singleton $ WireDecl (varSize size) $ Ident "res"
        }

binOpInSize :: SizeIdent
binOpInSize = SizeIdent $ Ident "N"

buildBinOpModule :: CBinOp -> Size -> (ModuleHead, ModuleImport)
buildBinOpModule op outSize = (mHead, show (BinOpModule op) <> baseSuffix)
  where
    mHead =
      ModuleHead
        { moduleName = Base $ BinOpModule op,
          moduleSize = [binOpInSize],
          controlVars = Nothing,
          inputVars =
            [ WireDecl (varSize binOpInSize) $ Ident "lhs",
              WireDecl (varSize binOpInSize) $ Ident "rhs"
            ],
          outputVars = NonEmpty.singleton $ WireDecl outSize $ Ident "res"
        }

binOpModules :: CBinOp -> (ModuleHead, ModuleImport)
binOpModules op@CBinEq = buildBinOpModule op (constantSize 1)
binOpModules op@CBinSignedLt = buildBinOpModule op (constantSize 1)
binOpModules op@CBinUnsignedLt = buildBinOpModule op (constantSize 1)
binOpModules op@CBinAdd = buildBinOpModule op (varSize binOpInSize)
binOpModules op@CBinSub = buildBinOpModule op (varSize binOpInSize)
binOpModules op@CBinAnd = buildBinOpModule op (varSize binOpInSize)
binOpModules op@CBinOr = buildBinOpModule op (varSize binOpInSize)

fbyModule :: (ModuleHead, ModuleImport)
fbyModule = (mHead, show FbyModule <> baseSuffix)
  where
    size = SizeIdent $ Ident "N"
    clockName = Ident "clock"
    initName = Ident "init"

    mHead =
      ModuleHead
        { moduleName = Base FbyModule,
          moduleSize = [size],
          controlVars =
            Just
              ModuleControl
                { clockVar = WireDecl (constantSize 1) clockName,
                  initVar = WireDecl (constantSize 1) initName
                },
          inputVars =
            [ WireDecl (varSize size) $ Ident "init_val",
              WireDecl (varSize size) $ Ident "next_val"
            ],
          outputVars = NonEmpty.singleton $ WireDecl (varSize size) $ Ident "res"
        }

ifModule :: (ModuleHead, ModuleImport)
ifModule = (mHead, show IfModule <> baseSuffix)
  where
    size = SizeIdent $ Ident "N"

    mHead =
      ModuleHead
        { moduleName = Base IfModule,
          moduleSize = [size],
          controlVars = Nothing,
          inputVars =
            [ WireDecl (constantSize 1) $ Ident "cond",
              WireDecl (varSize size) $ Ident "true_branch",
              WireDecl (varSize size) $ Ident "false_branch"
            ],
          outputVars = NonEmpty.singleton $ WireDecl (varSize size) $ Ident "res"
        }

concatModule :: (ModuleHead, ModuleImport)
concatModule = (mHead, show ConcatModule <> baseSuffix)
  where
    lhsSize = SizeIdent $ Ident "M"
    rhsSize = SizeIdent $ Ident "N"

    mHead =
      ModuleHead
        { moduleName = Base ConcatModule,
          moduleSize = [lhsSize, rhsSize],
          controlVars = Nothing,
          inputVars =
            [ WireDecl (varSize lhsSize) $ Ident "lhs",
              WireDecl (varSize rhsSize) $ Ident "rhs"
            ],
          outputVars = NonEmpty.singleton $ WireDecl (sumVar lhsSize rhsSize) $ Ident "res"
        }

sliceModule :: (ModuleHead, ModuleImport)
sliceModule = (mHead, show SliceModule <> baseSuffix)
  where
    size = SizeIdent $ Ident "N"
    fstIndex = SizeIdent $ Ident "I"
    sndIndex = SizeIdent $ Ident "J"

    mHead =
      ModuleHead
        { moduleName = Base SliceModule,
          moduleSize = [size, fstIndex, sndIndex],
          controlVars = Nothing,
          inputVars = [WireDecl (varSize size) $ Ident "arg"],
          outputVars = NonEmpty.singleton $ WireDecl (subVar fstIndex sndIndex) $ Ident "res"
        }

selectModule :: (ModuleHead, ModuleImport)
selectModule = (mHead, show SelectModule <> baseSuffix)
  where
    size = SizeIdent $ Ident "N"
    arg = SizeIdent $ Ident "I"

    mHead =
      ModuleHead
        { moduleName = Base SelectModule,
          moduleSize = [size, arg],
          controlVars = Nothing,
          inputVars = [WireDecl (varSize size) $ Ident "arg"],
          outputVars = NonEmpty.singleton $ WireDecl (constantSize 1) $ Ident "res"
        }
