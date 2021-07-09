module TSHM.Reconciler (reconcile) where

import qualified Data.Map        as M
import           Prelude
import           TSHM.TypeScript
import           Utils           (arrangeBy, concatWhereBy)

-- | The parser cannot parse everything to be "correct by construction". Where
-- this is the case, the reconciler steps in to take the naive AST and return
-- a fixed one. This operation is infallible.
reconcile :: ParsedAST -> ReconciledAST
reconcile = reconcileExports . reconcileOverloadedFunctions

-- | Expose unexported declarations which have been exported with `export`
-- statements, before or after the declaration in question. This operation
-- causes the non-empty list to lose its length guarantee, for example in the
-- case of a single node, a default export, in which it points to a
-- non-existent identifier.
--
-- Note that duplicate exports for the same identifier are invalid in
-- TypeScript, so this function is dumb in that respect.
reconcileExports :: ParsedAST -> ReconciledAST
reconcileExports xs = unrollStmts `concatMap` xs
  where unrollStmts :: ScopedStatement -> [ScopedStatement]
        unrollStmts (ScopedStatementExportDec (ExportDef n))        = maybeToList $ identsToStmt n "default"
        unrollStmts (ScopedStatementExportDec (ExportNamedRefs ys)) = namedExportToStmt `mapMaybe` ys
        unrollStmts x                                               = pure x

        namedExportToStmt :: ExportNamedRef -> Maybe ScopedStatement
        namedExportToStmt (ExportNamedRefUnchanged n)  = identsToStmt n n
        namedExportToStmt (ExportNamedRefRenamed n n') = identsToStmt n n'

        identsToStmt :: Text -> Text -> Maybe ScopedStatement
        identsToStmt n n' = ScopedStatementMisc Exported . (n',) <$> M.lookup n stmts

        stmts :: Map StatementName StatementType
        stmts = M.fromList . mapMaybe getStmt . toList $ xs
          where getStmt (ScopedStatementMisc _ x) = Just x
                getStmt _                         = Nothing

-- | Merge function declarations of the same name as overloaded functions at the
-- site of the first member with the scope of the first member. The scope
-- doesn't need verifying as TypeScript disallows differing scopes for
-- overloaded functions.
reconcileOverloadedFunctions :: ParsedAST -> ParsedAST
reconcileOverloadedFunctions = fromList . (overloadMerge `concatWhereBy` overloadMatch) . arrangeBy overloadMatch . toList
  where overloadMatch
          (ScopedStatementMisc _ (n1, StatementFunctionDec _))
          (ScopedStatementMisc _ (n2, StatementFunctionDec _)) = n1 == n2
        overloadMatch _ _                                      = False

        overloadMerge
          (ScopedStatementMisc s (n, StatementFunctionDec fs1))
          (ScopedStatementMisc _ (_, StatementFunctionDec fs2)) =
            ScopedStatementMisc s (n, StatementFunctionDec $ fs1 <> fs2)
        -- This should technically never happen per our predicate in
        -- `concatWhereBy`
        overloadMerge x _                                       = x



