module TSHM.Reconciler (reconcile) where

import           Prelude
import           TSHM.TypeScript
import           Utils           (arrangeBy, concatWhereBy)

-- | The parser cannot parse everything to be "correct by construction". Where
-- this is the case, the reconciler steps in to take the naive AST and return
-- a fixed one. This operation is infallible.
reconcile :: AST -> AST
reconcile = reconcileOverloadedFunctions

-- | Merge function declarations of the same name as overloaded functions at the
-- site of the first member with the scope of the first member. The scope
-- doesn't need verifying as TypeScript disallows differing scopes for
-- overloaded functions.
reconcileOverloadedFunctions :: AST -> AST
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



