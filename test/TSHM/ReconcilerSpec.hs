module TSHM.ReconcilerSpec (spec) where

import           Prelude
import           TSHM.Reconciler (reconcile)
import           TSHM.TypeScript
import           Test.Hspec

spec :: Spec
spec = describe "TSHM.Reconciler" $ do
  describe "reconciliation of overloaded functions" $ do
    it "reconciles" $ do
      let misc1 = ScopedStatementMisc Exported ("a", StatementAlias $ Alias Nothing TAny)
          misc2 = ScopedStatementMisc Exported ("b", StatementAlias $ Alias Nothing TUnknown)
          asDec n xs = ScopedStatementMisc Exported (n, StatementFunctionDec $ fromList xs)
          fn1 = [FunctionDec $ Lambda Nothing [] $ TString "1"]
          fn2 = [FunctionDec $ Lambda Nothing [] $ TString "2"]
          fn3 = [FunctionDec $ Lambda Nothing [] $ TString "3"]
          fn4 = [FunctionDec $ Lambda Nothing [] $ TString "4"]
          before' = fromList
            [ misc1
            , asDec "f" fn1
            , asDec "f" fn4
            , misc2
            , asDec "g" fn3
            , asDec "f" fn2
            ]
          after' = fromList
            [ misc1
            , asDec "f" $ fn1 <> fn4 <> fn2
            , misc2
            , asDec "g" fn3
            ]
      reconcile before' `shouldBe` after'

  describe "reconciliation of exports" $ do
    it "maintains every export" $ do
      let before' = fromList
            [ ScopedStatementMisc Exported ("a", StatementAlias $ Alias Nothing TAny)
            , ScopedStatementExportDec $ ExportDef "a"
            , ScopedStatementExportDec . ExportNamedRefs . pure $ ExportNamedRefRenamed "a" "b"
            ]
          after' = fromList
            [ ScopedStatementMisc Exported ("a", StatementAlias $ Alias Nothing TAny)
            , ScopedStatementMisc Exported ("default", StatementAlias $ Alias Nothing TAny)
            , ScopedStatementMisc Exported ("b", StatementAlias $ Alias Nothing TAny)
            ]
      reconcile before' `shouldBe` after'
