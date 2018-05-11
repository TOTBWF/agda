{-# LANGUAGE CPP                   #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Agda.Interaction.BasicOps where

import Prelude hiding (null)

import Control.Arrow ((***), first, second)
import Control.Applicative hiding (empty)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Maybe
import Data.Traversable hiding (mapM, forM, for)
import Data.Monoid

import qualified Agda.Syntax.Concrete as C -- ToDo: Remove with instance of ToConcrete
import Agda.Syntax.Position
import Agda.Syntax.Abstract as A hiding (Open, Apply, Assign)
import Agda.Syntax.Abstract.Views as A
import Agda.Syntax.Abstract.Pretty
import Agda.Syntax.Common
import Agda.Syntax.Info (ExprInfo(..),MetaInfo(..),emptyMetaInfo,exprNoRange,defaultAppInfo_,defaultAppInfo)
import qualified Agda.Syntax.Info as Info
import Agda.Syntax.Internal as I
import Agda.Syntax.Literal
import Agda.Syntax.Translation.InternalToAbstract
import Agda.Syntax.Translation.AbstractToConcrete
import Agda.Syntax.Translation.ConcreteToAbstract
import Agda.Syntax.Scope.Base
import Agda.Syntax.Scope.Monad
import Agda.Syntax.Fixity(Precedence(..), argumentCtx_)
import Agda.Syntax.Parser

import Agda.TheTypeChecker
import Agda.TypeChecking.Constraints
import Agda.TypeChecking.Conversion
import Agda.TypeChecking.Errors ( stringTCErr )
import Agda.TypeChecking.Monad as M hiding (MetaInfo)
import Agda.TypeChecking.MetaVars
import Agda.TypeChecking.Reduce
import Agda.TypeChecking.Substitute
import Agda.TypeChecking.Telescope
import Agda.TypeChecking.With
import Agda.TypeChecking.Coverage
import Agda.TypeChecking.Records
import Agda.TypeChecking.Irrelevance (wakeIrrelevantVars)
import Agda.TypeChecking.Pretty (prettyTCM)
import Agda.TypeChecking.Free
import Agda.TypeChecking.CheckInternal
import Agda.TypeChecking.SizedTypes.Solve
import qualified Agda.TypeChecking.Pretty as TP
import Agda.TypeChecking.Warnings (runPM)

import Agda.Termination.TermCheck (termMutual)

import Agda.Utils.Except ( MonadError(catchError, throwError) )
import Agda.Utils.Functor
import Agda.Utils.Lens
import Agda.Utils.List
import Agda.Utils.Maybe
import Agda.Utils.Monad
import Agda.Utils.Null
import Agda.Utils.Pretty
import Agda.Utils.Permutation
import Agda.Utils.Size

#include "undefined.h"
import Agda.Utils.Impossible

-- | Parses an expression.

parseExpr :: Range -> String -> TCM C.Expr
parseExpr rng s = do
  C.ExprWhere e wh <- runPM $ parsePosString exprWhereParser pos s
  unless (null wh) $ typeError $ GenericError $
    "where clauses are not supported in holes"
  return e
  where pos = fromMaybe (startPos Nothing) $ rStart rng

parseExprIn :: InteractionId -> Range -> String -> TCM Expr
parseExprIn ii rng s = do
    mId <- lookupInteractionId ii
    updateMetaVarRange mId rng
    mi  <- getMetaInfo <$> lookupMeta mId
    e   <- parseExpr rng s
    concreteToAbstract (clScope mi) e

giveExpr :: UseForce -> Maybe InteractionId -> MetaId -> Expr -> TCM ()
-- When translator from internal to abstract is given, this function might return
-- the expression returned by the type checker.
giveExpr force mii mi e = do
    mv <- lookupMeta mi
    -- In the context (incl. signature) of the meta variable,
    -- type check expression and assign meta
    withMetaInfo (getMetaInfo mv) $ do
      metaTypeCheck mv (mvJudgement mv)
  where
    metaTypeCheck mv IsSort{}      = __IMPOSSIBLE__
    metaTypeCheck mv (HasType _ t) = disableDestructiveUpdate $ do
      reportSDoc "interaction.give" 20 $
        TP.text "give: meta type =" TP.<+> prettyTCM t
      -- Here, we must be in the same context where the meta was created.
      -- Thus, we can safely apply its type to the context variables.
      ctx <- getContextArgs
      t' <- t `piApplyM` permute (takeP (length ctx) $ mvPermutation mv) ctx
      traceCall (CheckExprCall e t') $ do
        reportSDoc "interaction.give" 20 $
          TP.text "give: instantiated meta type =" TP.<+> prettyTCM t'
        v <- checkExpr e t'
        case mvInstantiation mv of

          InstV xs v' -> unlessM ((Irrelevant ==) <$> asks envRelevance) $ do
            reportSDoc "interaction.give" 20 $ TP.sep
              [ TP.text "meta was already set to value v' = " TP.<+> prettyTCM v'
                TP.<+> TP.text " with free variables " TP.<+> return (fsep $ map pretty xs)
              , TP.text "now comparing it to given value v = " TP.<+> prettyTCM v
              , TP.text "in context " TP.<+> inTopContext (prettyTCM ctx)
              ]
            -- The number of free variables should be at least the size of the context
            -- (Ideally, if we implemented contextual type theory, it should be the same.)
            when (length xs < size ctx) __IMPOSSIBLE__
            -- if there are more free variables than the context has
            -- we need to abstract over the additional ones (xs2)
            let (_xs1, xs2) = splitAt (size ctx) xs
            v' <- return $ foldr mkLam v' xs2
            reportSDoc "interaction.give" 20 $ TP.sep
              [ TP.text "in meta context, v' = " TP.<+> prettyTCM v'
              ]
            equalTerm t' v v'  -- Note: v' now lives in context of meta

          _ -> do -- updateMeta mi v
            reportSLn "interaction.give" 20 "give: meta unassigned, assigning..."
            args <- getContextArgs
            nowSolvingConstraints $ assign DirEq mi args v

        reportSDoc "interaction.give" 20 $ TP.text "give: meta variable updated!"
        unless (force == WithForce) $ redoChecks mii
        wakeupConstraints mi
        solveSizeConstraints DontDefaultToInfty
        unless (force == WithForce) $ do
          -- Double check.
          reportSDoc "interaction.give" 20 $ TP.text "give: double checking"
          vfull <- instantiateFull v
          checkInternal vfull t'

-- | After a give, redo termination etc. checks for function which was complemented.
redoChecks :: Maybe InteractionId -> TCM ()
redoChecks Nothing = return ()
redoChecks (Just ii) = do
  reportSLn "interaction.give" 20 $
    "give: redoing termination check for function surrounding " ++ show ii
  ip <- lookupInteractionPoint ii
  case ipClause ip of
    IPNoClause -> return ()
    IPClause f _ _ -> do
      mb <- mutualBlockOf f
      terErrs <- local (\ e -> e { envMutualBlock = Just mb }) $ termMutual []
      unless (null terErrs) $ typeError $ TerminationCheckFailed terErrs
  -- TODO redo positivity check!

-- | Try to fill hole by expression.
--
--   Returns the given expression unchanged
--   (for convenient generalization to @'refine'@).
give
  :: UseForce       -- ^ Skip safety checks?
  -> InteractionId  -- ^ Hole.
  -> Maybe Range
  -> Expr           -- ^ The expression to give.
  -> TCM Expr       -- ^ If successful, the very expression is returned unchanged.
give force ii mr e = liftTCM $ do
  -- if Range is given, update the range of the interaction meta
  mi  <- lookupInteractionId ii
  whenJust mr $ updateMetaVarRange mi
  reportSDoc "interaction.give" 10 $ TP.text "giving expression" TP.<+> prettyTCM e
  reportSDoc "interaction.give" 50 $ TP.text $ show $ deepUnscope e
  -- Try to give mi := e
  do setMetaOccursCheck mi DontRunMetaOccursCheck -- #589, #2710: Allow giving recursive solutions.
     giveExpr force (Just ii) mi e
    `catchError` \ case
      -- Turn PatternErr into proper error:
      PatternErr -> typeError . GenericDocError =<< do
        withInteractionId ii $ TP.text "Failed to give" TP.<+> prettyTCM e
      err -> throwError err
  removeInteractionPoint ii
  return e


-- | Try to refine hole by expression @e@.
--
--   This amounts to successively try to give @e@, @e ?@, @e ? ?@, ...
--   Returns the successfully given expression.
refine
  :: UseForce       -- ^ Skip safety checks when giving?
  -> InteractionId  -- ^ Hole.
  -> Maybe Range
  -> Expr           -- ^ The expression to refine the hole with.
  -> TCM Expr       -- ^ The successfully given expression.
refine force ii mr e = do
  mi <- lookupInteractionId ii
  mv <- lookupMeta mi
  let range = fromMaybe (getRange mv) mr
      scope = M.getMetaScope mv
  reportSDoc "interaction.refine" 10 $
    TP.text "refining with expression" TP.<+> prettyTCM e
  reportSDoc "interaction.refine" 50 $
    TP.text $ show $ deepUnscope e
  -- We try to append up to 10 meta variables
  tryRefine 10 range scope e
  where
    tryRefine :: Int -> Range -> ScopeInfo -> Expr -> TCM Expr
    tryRefine nrOfMetas r scope e = try nrOfMetas e
      where
        try :: Int -> Expr -> TCM Expr
        try 0 e = throwError $ stringTCErr "Cannot refine"
        try n e = give force ii (Just r) e `catchError` (\_ -> try (n - 1) =<< appMeta e)

        -- Apply A.Expr to a new meta
        appMeta :: Expr -> TCM Expr
        appMeta e = do
          let rng = rightMargin r -- Andreas, 2013-05-01 conflate range to its right margin to ensure that appended metas are last in numbering.  This fixes issue 841.
          -- Make new interaction point
          ii <- registerInteractionPoint False rng Nothing
          let info = Info.MetaInfo
                { Info.metaRange = rng
                , Info.metaScope = scope { scopePrecedence = [argumentCtx_] }
                    -- Ulf, 2017-09-07: The `argumentCtx_` above is causing #737.
                    -- If we're building an operator application the precedence
                    -- should be something else.
                , metaNumber = Nothing -- in order to print just as ?, not ?n
                , metaNameSuggestion = ""
                }
              metaVar = QuestionMark info ii

              count x e = getSum $ foldExpr isX e
                where isX (A.Var y) | x == y = Sum 1
                      isX _                  = mempty

              lamView (A.Lam _ (DomainFree _ x) e) = Just (x, e)
              lamView (A.Lam i (DomainFull (TypedBindings r (Arg ai (TBind br (x : xs) a)))) e)
                | null xs   = Just (dget x, e)
                | otherwise = Just (dget x, A.Lam i (DomainFull $ TypedBindings r $ Arg ai $ TBind br xs a) e)
              lamView _ = Nothing

              -- reduce beta-redexes where the argument is used at most once
              smartApp i e arg =
                case lamView $ unScope e of
                  Just (x, e) | count x e < 2 -> mapExpr subX e
                    where subX (A.Var y) | x == y = namedArg arg
                          subX e = e
                  _ -> App i e arg
          return $ smartApp (defaultAppInfo r) e $ defaultNamedArg metaVar

-- Andreas, 2017-12-16:
-- Ulf, your attempt to fix #737 introduced regression #2873.
-- Going through concrete syntax does some arbitrary disambiguation
-- of constructors, which subsequently makes refine fail.
-- I am not convinced of the printing-parsing shortcut to address problems.
-- (Unless you prove the roundtrip property.)
--
--           rescopeExpr scope $ smartApp (defaultAppInfo r) e $ defaultNamedArg metaVar
-- -- | Turn an abstract expression into concrete syntax and then back into
-- --   abstract. This ensures that context precedences are set correctly for
-- --   abstract expressions built by hand. Used by refine above.
-- rescopeExpr :: ScopeInfo -> Expr -> TCM Expr
-- rescopeExpr scope = withScope_ scope . (concreteToAbstract_ <=< runAbsToCon . preserveInteractionIds . toConcrete)

{-| Evaluate the given expression in the current environment -}
evalInCurrent :: Expr -> TCM Expr
evalInCurrent e =
    do  (v, t) <- inferExpr e
        v' <- {- etaContract =<< -} normalise v
        reify v'


evalInMeta :: InteractionId -> Expr -> TCM Expr
evalInMeta ii e =
   do   m <- lookupInteractionId ii
        mi <- getMetaInfo <$> lookupMeta m
        withMetaInfo mi $
            evalInCurrent e

-- | Modifier for interactive commands,
--   specifying the amount of normalization in the output.
--
data Rewrite =  AsIs | Instantiated | HeadNormal | Simplified | Normalised
    deriving (Show, Read)

normalForm :: (Reduce t, Simplify t, Normalise t) => Rewrite -> t -> TCM t
normalForm AsIs         t = return t
normalForm Instantiated t = return t   -- reify does instantiation
normalForm HeadNormal   t = {- etaContract =<< -} reduce t
normalForm Simplified   t = {- etaContract =<< -} simplify t
normalForm Normalised   t = {- etaContract =<< -} normalise t

-- | Modifier for the interactive computation command,
--   specifying the mode of computation and result display.
--
data ComputeMode = DefaultCompute | IgnoreAbstract | UseShowInstance
  deriving (Show, Read, Eq)

computeIgnoreAbstract :: ComputeMode -> Bool
computeIgnoreAbstract DefaultCompute  = False
computeIgnoreAbstract IgnoreAbstract  = True
computeIgnoreAbstract UseShowInstance = True
  -- UseShowInstance requires the result to be a string literal so respecting
  -- abstract can only ever break things.

computeWrapInput :: ComputeMode -> String -> String
computeWrapInput UseShowInstance s = "show (" ++ s ++ ")"
computeWrapInput _               s = s

showComputed :: ComputeMode -> Expr -> TCM Doc
showComputed UseShowInstance e =
  case e of
    A.Lit (LitString _ s) -> pure (text s)
    _                     -> (text "Not a string:" $$) <$> prettyATop e
showComputed _ e = prettyATop e

-- | Modifier for interactive commands,
--   specifying whether safety checks should be ignored.
data UseForce
  = WithForce     -- ^ Ignore additional checks, like termination/positivity...
  | WithoutForce  -- ^ Don't ignore any checks.
  deriving (Eq, Read, Show)

data OutputForm a b = OutputForm Range [ProblemId] (OutputConstraint a b)
  deriving (Functor)

data OutputConstraint a b
      = OfType b a | CmpInType Comparison a b b
                   | CmpElim [Polarity] a [b] [b]
      | JustType b | CmpTypes Comparison b b
                   | CmpLevels Comparison b b
                   | CmpTeles Comparison b b
      | JustSort b | CmpSorts Comparison b b
      | Guard (OutputConstraint a b) ProblemId
      | Assign b a | TypedAssign b a a | PostponedCheckArgs b [a] a a
      | IsEmptyType a
      | SizeLtSat a
      | FindInScopeOF b a [(a,a)]
      | PTSInstance b b
  deriving (Functor)

-- | A subset of 'OutputConstraint'.

data OutputConstraint' a b = OfType' { ofName :: b
                                     , ofExpr :: a
                                     }

outputFormId :: OutputForm a b -> b
outputFormId (OutputForm _ _ o) = out o
  where
    out o = case o of
      OfType i _                 -> i
      CmpInType _ _ i _          -> i
      CmpElim _ _ (i:_) _        -> i
      CmpElim _ _ [] _           -> __IMPOSSIBLE__
      JustType i                 -> i
      CmpLevels _ i _            -> i
      CmpTypes _ i _             -> i
      CmpTeles _ i _             -> i
      JustSort i                 -> i
      CmpSorts _ i _             -> i
      Guard o _                  -> out o
      Assign i _                 -> i
      TypedAssign i _ _          -> i
      PostponedCheckArgs i _ _ _ -> i
      IsEmptyType _              -> __IMPOSSIBLE__   -- Should never be used on IsEmpty constraints
      SizeLtSat{}                -> __IMPOSSIBLE__
      FindInScopeOF _ _ _        -> __IMPOSSIBLE__
      PTSInstance i _            -> i

instance Reify ProblemConstraint (Closure (OutputForm Expr Expr)) where
  reify (PConstr pids cl) = enterClosure cl $ \c -> buildClosure =<< (OutputForm (getRange c) (Set.toList pids) <$> reify c)

reifyElimToExpr :: I.Elim -> TCM Expr
reifyElimToExpr e = case e of
    I.Apply v -> appl "apply" <$> reify v
    I.Proj _o f -> appl "proj" <$> reify ((defaultArg $ I.Def f []) :: Arg Term)
  where
    appl :: String -> Arg Expr -> Expr
    appl s v = A.App defaultAppInfo_ (A.Lit (LitString noRange s)) $ fmap unnamed v

instance Reify Constraint (OutputConstraint Expr Expr) where
    reify (ValueCmp cmp t u v)   = CmpInType cmp <$> reify t <*> reify u <*> reify v
    reify (ElimCmp cmp _ t v es1 es2) =
      CmpElim cmp <$> reify t <*> mapM reifyElimToExpr es1
                              <*> mapM reifyElimToExpr es2
    reify (LevelCmp cmp t t')    = CmpLevels cmp <$> reify t <*> reify t'
    reify (TypeCmp cmp t t')     = CmpTypes cmp <$> reify t <*> reify t'
    reify (TelCmp a b cmp t t')  = CmpTeles cmp <$> (ETel <$> reify t) <*> (ETel <$> reify t')
    reify (SortCmp cmp s s')     = CmpSorts cmp <$> reify s <*> reify s'
    reify (Guarded c pid) = do
        o  <- reify c
        return $ Guard o pid
    reify (UnBlock m) = do
        mi <- mvInstantiation <$> lookupMeta m
        m' <- reify (MetaV m [])
        case mi of
          BlockedConst t -> do
            e  <- reify t
            return $ Assign m' e
          PostponedTypeCheckingProblem cl _ -> enterClosure cl $ \p -> case p of
            CheckExpr e a -> do
                a  <- reify a
                return $ TypedAssign m' e a
            CheckLambda (Arg ai (xs, mt)) body target -> do
              domType <- maybe (return underscore) reify mt
              target  <- reify target
              let bs = TypedBindings noRange $ Arg ai $
                       TBind noRange xs domType
                  e  = A.Lam Info.exprNoRange (DomainFull bs) body
              return $ TypedAssign m' e target
            CheckArgs _ _ args t0 t1 _ -> do
              t0 <- reify t0
              t1 <- reify t1
              return $ PostponedCheckArgs m' (map (namedThing . unArg) args) t0 t1
            UnquoteTactic tac _ goal -> do
              tac <- A.App defaultAppInfo_ (A.Unquote exprNoRange) . defaultNamedArg <$> reify tac
              OfType tac <$> reify goal
          Open{}  -> __IMPOSSIBLE__
          OpenIFS{}  -> __IMPOSSIBLE__
          InstV{} -> __IMPOSSIBLE__
    reify (FindInScope m _b mcands) = FindInScopeOF
      <$> (reify $ MetaV m [])
      <*> (reify =<< getMetaType m)
      <*> (forM (fromMaybe [] mcands) $ \ (Candidate tm ty eti _) -> do
            (,) <$> reify tm <*> reify ty)
    reify (IsEmpty r a) = IsEmptyType <$> reify a
    reify (CheckSizeLtSat a) = SizeLtSat  <$> reify a
    reify (CheckFunDef d i q cs) = __IMPOSSIBLE__
    reify (HasBiggerSort a) = OfType <$> reify a <*> reify (UnivSort a)
    reify (HasPTSRule a b) = do
      (a,(x,b)) <- reify (a,b)
      return $ PTSInstance a b

-- ASR TODO (28 December 2014): This function will be unnecessary when
-- using a Pretty instance for OutputConstraint instead of the Show
-- instance.
showComparison :: Comparison -> String
showComparison cmp = " " ++ prettyShow cmp ++ " "

instance (Show a,Show b) => Show (OutputForm a b) where
  show o =
    case o of
      OutputForm r []   c -> show c ++ range r
      OutputForm r pids c -> show pids ++ " " ++ show c ++ range r
    where
      range r | null s    = ""
              | otherwise = " [ at " ++ s ++ " ]"
        where s = show r

instance (Show a,Show b) => Show (OutputConstraint a b) where
    show (OfType e t)           = show e ++ " : " ++ show t
    show (JustType e)           = "Type " ++ show e
    show (JustSort e)           = "Sort " ++ show e
    show (CmpInType cmp t e e') = show e ++ showComparison cmp ++ show e' ++ " : " ++ show t
    show (CmpElim cmp t e e')   = show e ++ " == " ++ show e' ++ " : " ++ show t
    show (CmpTypes  cmp t t')   = show t ++ showComparison cmp ++ show t'
    show (CmpLevels cmp t t')   = show t ++ showComparison cmp ++ show t'
    show (CmpTeles  cmp t t')   = show t ++ showComparison cmp ++ show t'
    show (CmpSorts cmp s s')    = show s ++ showComparison cmp ++ show s'
    show (Guard o pid)          = show o ++ " [blocked by problem " ++ prettyShow pid ++ "]"
    show (Assign m e)           = show m ++ " := " ++ show e
    show (TypedAssign m e a)    = show m ++ " := " ++ show e ++ " :? " ++ show a
    show (PostponedCheckArgs m es t0 t1) = show m ++ " := (_ : " ++ show t0 ++ ") " ++ unwords (map (paren . show) es)
                                                  ++ " : " ++ show t1
      where paren s | elem ' ' s = "(" ++ s ++ ")"
                    | otherwise  = s
    show (IsEmptyType a)        = "Is empty: " ++ show a
    show (SizeLtSat a)          = "Not empty type of sizes: " ++ show a
    show (FindInScopeOF s t cs) = "Resolve instance argument " ++ showCand (s,t) ++ ".\n  Candidates:\n    [ " ++
                                    List.intercalate "\n    , " (map showCand cs) ++ " ]"
      where
      showCand (tm,ty) = indent 6 $ show tm ++ " : " ++ show ty
      indent n s = List.intercalate ("\n" ++ replicate n ' ') $ lines s
    show (PTSInstance a b)      = "PTS instance for " ++ show (a,b)

instance (ToConcrete a c, ToConcrete b d) =>
         ToConcrete (OutputForm a b) (OutputForm c d) where
    toConcrete (OutputForm r pid c) = OutputForm r pid <$> toConcrete c

instance (ToConcrete a c, ToConcrete b d) =>
         ToConcrete (OutputConstraint a b) (OutputConstraint c d) where
    toConcrete (OfType e t) = OfType <$> toConcrete e <*> toConcreteCtx TopCtx t
    toConcrete (JustType e) = JustType <$> toConcrete e
    toConcrete (JustSort e) = JustSort <$> toConcrete e
    toConcrete (CmpInType cmp t e e') =
      CmpInType cmp <$> toConcreteCtx TopCtx t <*> toConcreteCtx argumentCtx_ e
                                               <*> toConcreteCtx argumentCtx_ e'
    toConcrete (CmpElim cmp t e e') =
      CmpElim cmp <$> toConcreteCtx TopCtx t <*> toConcreteCtx TopCtx e <*> toConcreteCtx TopCtx e'
    toConcrete (CmpTypes cmp e e') = CmpTypes cmp <$> toConcreteCtx argumentCtx_ e
                                                  <*> toConcreteCtx argumentCtx_ e'
    toConcrete (CmpLevels cmp e e') = CmpLevels cmp <$> toConcreteCtx argumentCtx_ e
                                                    <*> toConcreteCtx argumentCtx_ e'
    toConcrete (CmpTeles cmp e e') = CmpTeles cmp <$> toConcrete e <*> toConcrete e'
    toConcrete (CmpSorts cmp e e') = CmpSorts cmp <$> toConcreteCtx argumentCtx_ e
                                                  <*> toConcreteCtx argumentCtx_ e'
    toConcrete (Guard o pid) = Guard <$> toConcrete o <*> pure pid
    toConcrete (Assign m e) = noTakenNames $ Assign <$> toConcrete m <*> toConcreteCtx TopCtx e
    toConcrete (TypedAssign m e a) = TypedAssign <$> toConcrete m <*> toConcreteCtx TopCtx e
                                                                  <*> toConcreteCtx TopCtx a
    toConcrete (PostponedCheckArgs m args t0 t1) =
      PostponedCheckArgs <$> toConcrete m <*> toConcrete args <*> toConcrete t0 <*> toConcrete t1
    toConcrete (IsEmptyType a) = IsEmptyType <$> toConcreteCtx TopCtx a
    toConcrete (SizeLtSat a) = SizeLtSat <$> toConcreteCtx TopCtx a
    toConcrete (FindInScopeOF s t cs) =
      FindInScopeOF <$> toConcrete s <*> toConcrete t
                    <*> mapM (\(tm,ty) -> (,) <$> toConcrete tm <*> toConcrete ty) cs
    toConcrete (PTSInstance a b) = PTSInstance <$> toConcrete a <*> toConcrete b

instance (Pretty a, Pretty b) => Pretty (OutputConstraint' a b) where
  pretty (OfType' e t) = pretty e <+> text ":" <+> pretty t

instance (ToConcrete a c, ToConcrete b d) =>
            ToConcrete (OutputConstraint' a b) (OutputConstraint' c d) where
  toConcrete (OfType' e t) = OfType' <$> toConcrete e <*> toConcreteCtx TopCtx t

getConstraints :: TCM [OutputForm C.Expr C.Expr]
getConstraints = liftTCM $ do
    cs <- M.getAllConstraints
    cs <- forM cs $ \c -> do
            cl <- reify c
            enterClosure cl abstractToConcrete_
    ss <- mapM toOutputForm =<< getSolvedInteractionPoints True AsIs -- get all
    return $ ss ++ cs
  where
    toOutputForm (ii, mi, e) = do
      mv <- getMetaInfo <$> lookupMeta mi
      withMetaInfo mv $ do
        let m = QuestionMark emptyMetaInfo{ metaNumber = Just $ fromIntegral ii } ii
        abstractToConcrete_ $ OutputForm noRange [] $ Assign m e

-- | @getSolvedInteractionPoints True@ returns all solutions,
--   even if just solved by another, non-interaction meta.
--
--   @getSolvedInteractionPoints False@ only returns metas that
--   are solved by a non-meta.

getSolvedInteractionPoints :: Bool -> Rewrite -> TCM [(InteractionId, MetaId, Expr)]
getSolvedInteractionPoints all norm = concat <$> do
  mapM solution =<< getInteractionIdsAndMetas
  where
    solution (i, m) = do
      mv <- lookupMeta m
      withMetaInfo (getMetaInfo mv) $ do
        args  <- getContextArgs
        scope <- getScope
        let sol v = do
              -- Andreas, 2014-02-17 exclude metas solved by metas
              v <- instantiate v
              let isMeta = case v of MetaV{} -> True; _ -> False
              if isMeta && not all then return [] else do
                e <- reify =<< normalForm norm v
                return [(i, m, ScopedExpr scope e)]
            unsol = return []
        case mvInstantiation mv of
          InstV{}                        -> sol (MetaV m $ map Apply args)
          Open{}                         -> unsol
          OpenIFS{}                      -> unsol
          BlockedConst{}                 -> unsol
          PostponedTypeCheckingProblem{} -> unsol

typeOfMetaMI :: Rewrite -> MetaId -> TCM (OutputConstraint Expr NamedMeta)
typeOfMetaMI norm mi =
     do mv <- lookupMeta mi
        withMetaInfo (getMetaInfo mv) $
          rewriteJudg mv (mvJudgement mv)
   where
    rewriteJudg :: MetaVariable -> Judgement MetaId ->
                   TCM (OutputConstraint Expr NamedMeta)
    rewriteJudg mv (HasType i t) = do
      ms <- getMetaNameSuggestion i
      t <- normalForm norm t
      vs <- getContextArgs
      let x = NamedMeta ms i
      reportSDoc "interactive.meta" 10 $ TP.vcat
        [ TP.text $ unwords ["permuting", show i, "with", show $ mvPermutation mv]
        , TP.nest 2 $ TP.vcat
          [ TP.text "len  =" TP.<+> TP.text (show $ length vs)
          , TP.text "args =" TP.<+> prettyTCM vs
          , TP.text "t    =" TP.<+> prettyTCM t
          , TP.text "x    =" TP.<+> TP.pretty x
          ]
        ]
      reportSDoc "interactive.meta.scope" 20 $ TP.text $ show $ getMetaScope mv
      -- Andreas, 2016-01-19, issue #1783: need piApplyM instead of just piApply
      OfType x <$> do reify =<< t `piApplyM` permute (takeP (size vs) $ mvPermutation mv) vs
    rewriteJudg mv (IsSort i t) = do
      ms <- getMetaNameSuggestion i
      return $ JustSort $ NamedMeta ms i


typeOfMeta :: Rewrite -> InteractionId -> TCM (OutputConstraint Expr InteractionId)
typeOfMeta norm ii = typeOfMeta' norm . (ii,) =<< lookupInteractionId ii

typeOfMeta' :: Rewrite -> (InteractionId, MetaId) -> TCM (OutputConstraint Expr InteractionId)
typeOfMeta' norm (ii, mi) = fmap (\_ -> ii) <$> typeOfMetaMI norm mi

typesOfVisibleMetas :: Rewrite -> TCM [OutputConstraint Expr InteractionId]
typesOfVisibleMetas norm =
  liftTCM $ mapM (typeOfMeta' norm) =<< getInteractionIdsAndMetas

typesOfHiddenMetas :: Rewrite -> TCM [OutputConstraint Expr NamedMeta]
typesOfHiddenMetas norm = liftTCM $ do
  is    <- getInteractionMetas
  store <- Map.filterWithKey (openAndImplicit is) <$> getMetaStore
  mapM (typeOfMetaMI norm) $ Map.keys store
  where
  openAndImplicit is x m =
    case mvInstantiation m of
      M.InstV{} -> False
      M.Open    -> x `notElem` is
      M.OpenIFS -> x `notElem` is  -- OR: True !?
      M.BlockedConst{} -> True
      M.PostponedTypeCheckingProblem{} -> False

metaHelperType :: Rewrite -> InteractionId -> Range -> String -> TCM (OutputConstraint' Expr Expr)
metaHelperType norm ii rng s = case words s of
  []    -> failure
  f : _ -> do
    ensureName f
    A.Application h args <- A.appView . getBody . deepUnscope <$> parseExprIn ii rng ("let " ++ f ++ " = _ in " ++ s)
    withInteractionId ii $ do
      cxtArgs  <- getContextArgs
      -- cleanupType relies on with arguments being named 'w',
      -- so we'd better rename any actual 'w's to avoid confusion.
      tel      <- runIdentity . onNamesTel unW <$> getContextTelescope
      a        <- runIdentity . onNames unW . (`piApply` cxtArgs) <$> (getMetaType =<< lookupInteractionId ii)
      (vs, as) <- unzip <$> mapM (inferExpr . namedThing . unArg) args
      -- Remember the arity of a
      TelV atel _ <- telView a
      let arity = size atel
          (delta1, delta2, _, a', as', vs') = splitTelForWith tel a (map OtherType as) vs
      a <- local (\e -> e { envPrintDomainFreePi = True }) $ do
        reify =<< cleanupType arity args =<< normalForm norm =<< fst <$> withFunctionType delta1 vs' as' delta2 a'
      reportSDoc "interaction.helper" 10 $ TP.vcat
        [ TP.text "generating helper function"
        , TP.nest 2 $ TP.text "tel    = " TP.<+> inTopContext (prettyTCM tel)
        , TP.nest 2 $ TP.text "a      = " TP.<+> prettyTCM a
        , TP.nest 2 $ TP.text "vs     = " TP.<+> prettyTCM vs
        , TP.nest 2 $ TP.text "as     = " TP.<+> prettyTCM as
        , TP.nest 2 $ TP.text "delta1 = " TP.<+> inTopContext (prettyTCM delta1)
        , TP.nest 2 $ TP.text "delta2 = " TP.<+> inTopContext (addContext delta1 $ prettyTCM delta2)
        , TP.nest 2 $ TP.text "a'     = " TP.<+> inTopContext (addContext delta1 $ addContext delta2 $ prettyTCM a')
        , TP.nest 2 $ TP.text "as'    = " TP.<+> inTopContext (addContext delta1 $ prettyTCM as')
        , TP.nest 2 $ TP.text "vs'    = " TP.<+> inTopContext (addContext delta1 $ prettyTCM vs')
        ]
      return (OfType' h a)
  where
    failure = typeError $ GenericError $ "Expected an argument of the form f e1 e2 .. en"
    ensureName f = do
      ce <- parseExpr rng f
      case ce of
        C.Ident{} -> return ()
        C.RawApp _ [C.Ident{}] -> return ()
        _ -> do
         reportSLn "interaction.helper" 10 $ "ce = " ++ show ce
         failure
    cleanupType arity args t = do
      -- Get the arity of t
      TelV ttel _ <- telView t
      -- Compute the number or pi-types subject to stripping.
      let n = size ttel - arity
      -- It cannot be negative, otherwise we would have performed a
      -- negative number of with-abstractions.
      unless (n >= 0) __IMPOSSIBLE__
      return $ evalState (renameVars $ hiding args $ stripUnused n t) args

    getBody (A.Let _ _ e)      = e
    getBody _                  = __IMPOSSIBLE__

    -- Strip the non-dependent abstractions from the first n abstractions.
    stripUnused n (El s v) = El s $ strip n v
    strip 0 v = v
    strip n v = case v of
      I.Pi a b -> case stripUnused (n-1) <$> b of
        b | absName b == "w"   -> I.Pi a b
        NoAbs _ b              -> unEl b
        Abs s b | 0 `freeIn` b -> I.Pi (hide a) (Abs s b)
                | otherwise    -> strengthen __IMPOSSIBLE__ (unEl b)
      _ -> v  -- todo: handle if goal type is a Pi

    -- renameVars = onNames (stringToArgName <.> renameVar . argNameToString)
    renameVars = onNames renameVar

    hiding args (El s v) = El s $ hidingTm args v
    hidingTm (arg:args) (I.Pi a b) | absName b == "w" =
      I.Pi (setHiding (getHiding arg) a) (hiding args <$> b)
    hidingTm args (I.Pi a b) = I.Pi a (hiding args <$> b)
    hidingTm _ a = a

    -- onNames :: Applicative m => (ArgName -> m ArgName) -> Type -> m Type
    onNames :: Applicative m => (String -> m String) -> Type -> m Type
    onNames f (El s v) = El s <$> onNamesTm f v

    -- onNamesTel :: Applicative f => (ArgName -> f ArgName) -> I.Telescope -> f I.Telescope
    onNamesTel :: Applicative f => (String -> f String) -> I.Telescope -> f I.Telescope
    onNamesTel f I.EmptyTel = pure I.EmptyTel
    onNamesTel f (I.ExtendTel a b) = I.ExtendTel <$> traverse (onNames f) a <*> onNamesAbs f onNamesTel b

    onNamesTm f v = case v of
      I.Var x es   -> I.Var x <$> onNamesElims f es
      I.Def q es   -> I.Def q <$> onNamesElims f es
      I.Con c ci args -> I.Con c ci <$> onNamesArgs f args
      I.Lam i b    -> I.Lam i <$> onNamesAbs f onNamesTm b
      I.Pi a b     -> I.Pi <$> traverse (onNames f) a <*> onNamesAbs f onNames b
      I.DontCare v -> I.DontCare <$> onNamesTm f v
      I.Lit{}      -> pure v
      I.Sort{}     -> pure v
      I.Level{}    -> pure v
      I.MetaV{}    -> pure v
    onNamesElims f = traverse $ traverse $ onNamesTm f
    onNamesArgs f  = traverse $ traverse $ onNamesTm f
    onNamesAbs f   = onNamesAbs' f (stringToArgName <.> f . argNameToString)
    onNamesAbs' f f' nd (Abs   s x) = Abs   <$> f' s <*> nd f x
    onNamesAbs' f f' nd (NoAbs s x) = NoAbs <$> f' s <*> nd f x

    unW "w" = return ".w"
    unW s   = return s

    renameVar ('.':s) = pure s
    renameVar "w"     = betterName
    renameVar s       = pure s

    betterName = do
      arg : args <- get
      put args
      return $ case arg of
        Arg _ (Named _ (A.Var x)) -> show $ A.nameConcrete x
        Arg _ (Named (Just x) _)  -> argNameToString $ rangedThing x
        _                         -> "w"


-- Gives a list of names and corresponding types.

contextOfMeta :: InteractionId -> Rewrite -> TCM [OutputConstraint' Expr Name]
contextOfMeta ii norm = do
  info <- getMetaInfo <$> (lookupMeta =<< lookupInteractionId ii)
  withMetaInfo info $ do
    cxt <- getContext
    let n         = length cxt
        localVars = zipWith raise [1..] cxt
        mkLet (x, lb) = do
          (tm, Dom c ty) <- getOpen lb
          return $ Dom c (x, ty)
    letVars <- mapM mkLet . Map.toDescList =<< asks envLetBindings
    gfilter visible . reverse <$> mapM out (letVars ++ localVars)
  where gfilter p = catMaybes . map p
        visible (OfType x y) | not (isNoName x) = Just (OfType' x y)
                             | otherwise        = Nothing
        visible _            = __IMPOSSIBLE__
        out (Dom _ (x, t)) = do
          t' <- reify =<< normalForm norm t
          return $ OfType x t'


-- | Returns the type of the expression in the current environment
--   We wake up irrelevant variables just in case the user want to
--   invoke that command in an irrelevant context.
typeInCurrent :: Rewrite -> Expr -> TCM Expr
typeInCurrent norm e =
    do  (_,t) <- wakeIrrelevantVars $ inferExpr e
        v <- normalForm norm t
        reify v



typeInMeta :: InteractionId -> Rewrite -> Expr -> TCM Expr
typeInMeta ii norm e =
   do   m <- lookupInteractionId ii
        mi <- getMetaInfo <$> lookupMeta m
        withMetaInfo mi $
            typeInCurrent norm e

withInteractionId :: InteractionId -> TCM a -> TCM a
withInteractionId i ret = do
  m <- lookupInteractionId i
  withMetaId m ret

withMetaId :: MetaId -> TCM a -> TCM a
withMetaId m ret = do
  mv <- lookupMeta m
  withMetaInfo' mv ret

-- The intro tactic

-- Returns the terms (as strings) that can be
-- used to refine the goal. Uses the coverage checker
-- to find out which constructors are possible.
introTactic :: Bool -> InteractionId -> TCM [String]
introTactic pmLambda ii = do
  mi <- lookupInteractionId ii
  mv <- lookupMeta mi
  withMetaInfo (getMetaInfo mv) $ case mvJudgement mv of
    HasType _ t -> do
        t <- reduce =<< piApplyM t =<< getContextArgs
        -- Andreas, 2013-03-05 Issue 810: skip hidden domains in introduction
        -- of constructor.
        TelV tel' t <- telViewUpTo' (-1) notVisible t
        -- if we cannot introduce a constructor, we try a lambda
        let fallback = do
              TelV tel _ <- telView t
              reportSDoc "interaction.intro" 20 $ TP.sep
                [ TP.text "introTactic/fallback"
                , TP.text "tel' = " TP.<+> prettyTCM tel'
                , TP.text "tel  = " TP.<+> prettyTCM tel
                ]
              case (tel', tel) of
                (EmptyTel, EmptyTel) -> return []
                _ -> introFun (telToList tel' ++ telToList tel)

        case unEl t of
          I.Def d _ -> do
            def <- getConstInfo d
            case theDef def of
              Datatype{}    -> addContext tel' $ introData t
              Record{ recNamedCon = name }
                | name      -> addContext tel' $ introData t
                | otherwise -> addContext tel' $ introRec d
              _ -> fallback
          _ -> fallback
     `catchError` \_ -> return []
    _ -> __IMPOSSIBLE__
  where
    conName [p] = [ c | I.ConP c _ _ <- [namedArg p] ]
    conName _   = __IMPOSSIBLE__

    showTCM v = show <$> prettyTCM v

    introFun tel = addContext tel' $ do
        reportSDoc "interaction.intro" 10 $ do TP.text "introFun" TP.<+> prettyTCM (telFromList tel)
        imp <- showImplicitArguments
        let okHiding0 h = imp || h == NotHidden
            -- if none of the vars were displayed, we would get a parse error
            -- thus, we switch to displaying all
            allHidden   = null (filter okHiding0 hs)
            okHiding    = if allHidden then const True else okHiding0
        vars <- -- setShowImplicitArguments (imp || allHidden) $
                (if allHidden then withShowAllArguments else id) $
                  mapM showTCM [ setHiding h $ defaultArg $ var i :: Arg Term
                               | (h, i) <- zip hs $ downFrom n
                               , okHiding h
                               ]
        if pmLambda
           then return [ unwords $ ["λ", "{"] ++ vars ++ ["→", "?", "}"] ]
           else return [ unwords $ ["λ"]      ++ vars ++ ["→", "?"] ]
      where
        n = size tel
        hs   = map getHiding tel
        tel' = telFromList [ fmap makeName b | b <- tel ]
        makeName ("_", t) = ("x", t)
        makeName (x, t)   = (x, t)

    introData t = do
      let tel  = telFromList [defaultDom ("_", t)]
          pat  = [defaultArg $ unnamed $ debruijnNamedVar "c" 0]
      r <- splitLast CoInductive tel pat
      case r of
        Left err -> return []
        Right cov -> mapM showTCM $ concatMap (conName . scPats) $ splitClauses cov

    introRec :: QName -> TCM [String]
    introRec d = do
      hfs <- getRecordFieldNames d
      fs <- ifM showImplicitArguments
            (return $ map unArg hfs)
            (return [ unArg a | a <- hfs, visible a ])
      let e = C.Rec noRange $ for fs $ \ f ->
            Left $ C.FieldAssignment f $ C.QuestionMark noRange Nothing
      return [ prettyShow e ]

-- | Runs the given computation as if in an anonymous goal at the end
--   of the top-level module.
--
--   Sets up current module, scope, and context.
atTopLevel :: TCM a -> TCM a
atTopLevel m = inConcreteMode $ do
  let err = typeError $ GenericError "The file has not been loaded yet."
  caseMaybeM (use stCurrentModule) err $ \ current -> do
    caseMaybeM (getVisitedModule $ toTopLevelModuleName current) __IMPOSSIBLE__ $ \ mi -> do
      let scope = iInsideScope $ miInterface mi
      tel <- lookupSection current
      -- Get the names of the local variables from @scope@
      -- and put them into the context.
      --
      -- Andreas, 2017-04-24, issue #2552:
      --
      -- Delete the let-bound ones, since they are not represented
      -- in the module telescope.
      --
      -- This is a temporary fix until a better solution is available,
      -- e.g., when the module telescope represents let-bound variables.
      --
      -- Unfortunately, referring to let-bound variables
      -- from the top level module telescope will for now result in a not-in-scope error.
      let names :: [A.Name]
          names = map localVar $ filter ((LetBound /=) . localBinder) $ map snd $ reverse $ scopeLocals scope
      -- Andreas, 2016-12-31, issue #2371
      -- The following is an unnecessary complication, as shadowed locals
      -- are not in scope anyway (they are ambiguous).
      -- -- Replace the shadowed names by fresh names (such that they do not shadow imports)
      -- let mnames :: [Maybe A.Name]
      --     mnames = map (notShadowedLocal . snd) $ reverse $ scopeLocals scope
      -- names <- mapM (maybe freshNoName_ return) mnames
      let types :: [Dom I.Type]
          types = map (snd <$>) $ telToList tel
          gamma :: ListTel' A.Name
          gamma = fromMaybe __IMPOSSIBLE__ $
                    zipWith' (\ x dom -> (x,) <$> dom) names types
      reportSDoc "interaction.top" 20 $ TP.vcat
        [ TP.text "BasicOps.atTopLevel"
        , TP.text "  names = " TP.<+> TP.sep (map prettyA   names)
        , TP.text "  types = " TP.<+> TP.sep (map prettyTCM types)
        ]
      M.withCurrentModule current $
        withScope_ scope $
          addContext gamma $ do
            -- We're going inside the top-level module, so we have to set the
            -- checkpoint for it and all its submodules to the new checkpoint.
            cp <- view eCurrentCheckpoint
            stModuleCheckpoints %= fmap (const cp)
            m

-- | Parse a name.
parseName :: Range -> String -> TCM C.QName
parseName r s = do
  m <- parseExpr r s
  case m of
    C.Ident m              -> return m
    C.RawApp _ [C.Ident m] -> return m
    _                      -> typeError $
      GenericError $ "Not an identifier: " ++ show m ++ "."

-- | Check whether an expression is a (qualified) identifier.
isQName :: C.Expr -> Maybe C.QName
isQName m = do
  case m of
    C.Ident m              -> return m
    C.RawApp _ [C.Ident m] -> return m
    _ -> Nothing

-- | Returns the contents of the given module or record.

moduleContents
  :: Rewrite
     -- ^ How should the types be presented?
  -> Range
     -- ^ The range of the next argument.
  -> String
     -- ^ The module name.
  -> TCM ([C.Name], [(C.Name, Type)])
     -- ^ Module names, names paired up with corresponding types.

moduleContents norm rng s = traceCall ModuleContents $ do
  e <- parseExpr rng s
  case isQName e of
    -- If the expression is not a single identifier, it is not a module name
    -- and treated as a record expression.
    Nothing -> getRecordContents norm e
    -- Otherwise, if it is not in scope as a module name, it is treated
    -- as a record name.
    Just x  -> do
      ms :: [AbstractModule] <- scopeLookup x <$> getScope
      if null ms then getRecordContents norm e else getModuleContents norm x

-- | Returns the contents of the given record identifier.

getRecordContents
  :: Rewrite  -- ^ Amount of normalization in types.
  -> C.Expr   -- ^ Expression presumably of record type.
  -> TCM ([C.Name], [(C.Name, Type)])
              -- ^ Module names, names paired up with corresponding types.
getRecordContents norm ce = do
  e <- toAbstract ce
  (_, t) <- inferExpr e
  let notRecordType = typeError $ ShouldBeRecordType t
  (q, vs, defn) <- fromMaybeM notRecordType $ isRecordType t
  case defn of
    Record{ recFields = fs, recTel = tel } -> do
      let xs   = map (nameConcrete . qnameName . unArg) fs
          doms = telToList $ apply tel vs
      ts <- mapM (normalForm norm) $ map (snd . unDom) doms
      return ([], zip xs ts)
    _ -> __IMPOSSIBLE__

-- | Returns the contents of the given module.

getModuleContents
  :: Rewrite  -- ^ Amount of normalization in types.
  -> C.QName  -- ^ Module name.
  -> TCM ([C.Name], [(C.Name, Type)])
              -- ^ Module names, names paired up with corresponding types.
getModuleContents norm m = do
  modScope <- getNamedScope . amodName =<< resolveModule m
  let modules :: ThingsInScope AbstractModule
      modules = exportedNamesInScope modScope
      names :: ThingsInScope AbstractName
      names = exportedNamesInScope modScope
      xns = [ (x,n) | (x, ns) <- Map.toList names, n <- ns ]
  types <- forM xns $ \(x, n) -> do
    d <- getConstInfo $ anameName n
    t <- normalForm norm =<< (defType <$> instantiateDef d)
    return (x, t)
  return (Map.keys modules, types)


whyInScope :: String -> TCM (Maybe LocalVar, [AbstractName], [AbstractModule])
whyInScope s = do
  x     <- parseName noRange s
  scope <- getScope
  return ( lookup x $ map (first C.QName) $ scopeLocals scope
         , scopeLookup x scope
         , scopeLookup x scope )
