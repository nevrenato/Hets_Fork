{- |
Module      :  $Header$
Description :  Abstract syntax of CSP-CASL processes
Copyright   :  (c) Markus Roggenbach and Till Mossakowski and Uni Bremen 2004
License     :  GPLv2 or higher, see LICENSE.txt

Maintainer  :  a.m.gimblett@swan.ac.uk
Stability   :  provisional
Portability :  portable

Abstract syntax of CSP-CASL processes.

-}

module CspCASL.AS_CspCASL_Process
  ( CHANNEL_NAME
  , CommAlpha
  , CommType (..)
  , EVENT (..)
  , EVENT_SET (..)
  , FQ_PROCESS_NAME (..)
  , PROCESS (..)
  , PROCESS_NAME
  , PROC_ARGS
  , PROC_ALPHABET (..)
  , procNameToSimpProcName
  , ProcProfile (..)
  , RenameKind (..)
  , Rename (..)
  , RENAMING (..)
  , splitCASLVar
  , TypedChanName (..)
  ) where

import CASL.AS_Basic_CASL (FORMULA, SORT, TERM (..), VAR)
import Common.Id
import qualified Data.Set as Set

-- DrIFT command
{-! global: GetRange !-}

data EVENT
    -- | @t -> p@ - Term prefix
    = TermEvent (TERM ()) Range
    -- | @[] var :: s -> p@ - External nondeterministic prefix choice
    | ExternalPrefixChoice VAR SORT Range
    -- | @|~| var :: s -> p@ - Internal nondeterministic prefix choice
    | InternalPrefixChoice VAR SORT Range
    -- | @c ! t -> p@ - Channel send
    | ChanSend CHANNEL_NAME (TERM ()) Range
    -- | @c ! var :: s -> p@ - Channel nondeterministic send
    | ChanNonDetSend CHANNEL_NAME VAR SORT Range
    -- | @c ? var :: s -> p@ - Channel recieve
    | ChanRecv CHANNEL_NAME VAR SORT Range
    -- | @t -> p@ - Fully Qualified Term prefix
    | FQTermEvent (TERM ()) Range
    -- | @[] var :: s -> p@ - Fully Qualified External nondeterministic prefix
    -- choice. The term here holds the fully qualified variable (name and sort).
    | FQExternalPrefixChoice (TERM ()) Range
    -- | @|~| var :: s -> p@ - Fully Qualified Internal nondeterministic prefix
    -- choice. The term here holds the fully qualified variable (name and sort).
    | FQInternalPrefixChoice (TERM ()) Range
    -- | @c ! t -> p@ - Fully Qualified Channel send. The term holds the fully
    -- term to send.
    | FQChanSend (CHANNEL_NAME, SORT) (TERM ()) Range
    -- | @c ! var :: s -> p@ - Fully Qualified Channel nondeterministic
    -- send. The term here holds the fully qualified variable (name and sort).
    | FQChanNonDetSend (CHANNEL_NAME, SORT) (TERM ()) Range
    -- | @c ? var :: s -> p@ - Fully Qualified Channel recieve. The term here
    -- holds the fully qualified variable (name and sort).
    | FQChanRecv (CHANNEL_NAME, SORT) (TERM ()) Range
    deriving (Show, Ord, Eq)

-- | Event sets are sets of communication types.
data EVENT_SET = EventSet [CommType] Range
                 deriving (Show, Ord, Eq)

data RenameKind = TotOp | PartOp | BinPred deriving (Show, Ord, Eq)

data Rename = Rename Id (Maybe (RenameKind, Maybe (SORT, SORT)))
    deriving (Show, Ord, Eq)

-- | CSP renamings are predicate names or op names.
data RENAMING = Renaming [Rename]
                deriving (Show, Ord, Eq)

type CHANNEL_NAME = Id

type PROCESS_NAME = Id

type PROC_ARGS = [SORT]

data PROC_ALPHABET = ProcAlphabet [CommType]
                     deriving (Show, Ord, Eq)

splitCASLVar :: TERM () -> (VAR, SORT)
splitCASLVar (Qual_var v s _ ) = (v,s)
splitCASLVar _ =
  error "CspCASL.AS_CspCASL_Process: Can not split non Qual_var CASL Term"

{- | Fully qualified process names have parameter sorts, and a communication
alphabet (a Set of sorts). The CommAlpha here should always contain the minimal
super sorts only. The communication over subsorts is implied -}
data ProcProfile = ProcProfile PROC_ARGS CommAlpha
                   deriving (Eq, Ord, Show)

{- | A process name is either a fully qualified process name or a plain process
name. -}
data FQ_PROCESS_NAME
  -- | A non-fully qualified process name
  = PROCESS_NAME PROCESS_NAME
  {- | A name with parameter sorts and communication ids from the parser.
  This is where the user has tried to specify a fully qualified process name -}
  | FQ_PROCESS_NAME PROCESS_NAME ProcProfile
                  deriving (Eq, Ord, Show)

procNameToSimpProcName :: FQ_PROCESS_NAME -> PROCESS_NAME
procNameToSimpProcName (PROCESS_NAME pn) = pn
procNameToSimpProcName (FQ_PROCESS_NAME pn _) = pn

{- | A process communication alphabet consists of a set of sort names
and typed channel names. -}
data TypedChanName = TypedChanName CHANNEL_NAME SORT
                     deriving (Eq, Ord, Show)

data CommType = CommTypeSort SORT
              | CommTypeChan TypedChanName
                deriving (Eq, Ord)

{- | Type of communication types, either a sort communication or a typed channel
communications. -}
instance Show CommType where
    show (CommTypeSort s) = show s
    show (CommTypeChan (TypedChanName c s)) = show (c, s)

-- | Type of communication alphabet
type CommAlpha = Set.Set CommType

-- | CSP-CASL process expressions.
data PROCESS
    -- | @Skip@ - Terminate immediately
    = Skip Range
    -- | @Stop@ - Do nothing
    | Stop Range
    -- | @div@ - Divergence
    | Div Range
    -- | @Run es@ - Accept any event in es, forever
    | Run EVENT_SET Range
    -- | @Chaos es@ - Accept\/refuse any event in es, forever
    | Chaos EVENT_SET Range
    -- | @event -> p@ - Prefix process
    | PrefixProcess EVENT PROCESS Range
    -- | @p ; q@ - Sequential process
    | Sequential PROCESS PROCESS Range
    -- | @p [] q@ - External choice
    | ExternalChoice PROCESS PROCESS Range
    -- | @p |~| q@ - Internal choice
    | InternalChoice PROCESS PROCESS Range
    -- | @p ||| q@ - Interleaving
    | Interleaving PROCESS PROCESS Range
    -- | @p || q @ - Synchronous parallel
    | SynchronousParallel PROCESS PROCESS Range
    -- | @p [| a |] q@ - Generalised parallel
    | GeneralisedParallel PROCESS EVENT_SET PROCESS Range
    -- | @p [ a || b ] q@ - Alphabetised parallel
    | AlphabetisedParallel PROCESS EVENT_SET EVENT_SET PROCESS Range
    -- | @p \\ es@ - Hiding
    | Hiding PROCESS EVENT_SET Range
    -- | @p [[r]]@ - Renaming
    | RenamingProcess PROCESS RENAMING Range
    -- | @if f then p else q@ - Conditional
    | ConditionalProcess (FORMULA ()) PROCESS PROCESS Range
    -- | Named process
    | NamedProcess FQ_PROCESS_NAME [TERM ()] Range
    {- | Fully qualified process. The range here shall be the same as
    in the process. -}
    | FQProcess PROCESS CommAlpha Range
    deriving (Eq, Ord, Show)

-- Generated by DrIFT, look but don't touch!

instance GetRange EVENT where
  getRange x = case x of
    TermEvent _ p -> p
    ExternalPrefixChoice _ _ p -> p
    InternalPrefixChoice _ _ p -> p
    ChanSend _ _ p -> p
    ChanNonDetSend _ _ _ p -> p
    ChanRecv _ _ _ p -> p
    FQTermEvent _ p -> p
    FQExternalPrefixChoice _ p -> p
    FQInternalPrefixChoice _ p -> p
    FQChanSend _ _ p -> p
    FQChanNonDetSend _ _ p -> p
    FQChanRecv _ _ p -> p
  rangeSpan x = case x of
    TermEvent a b -> joinRanges [rangeSpan a, rangeSpan b]
    ExternalPrefixChoice a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                              rangeSpan c]
    InternalPrefixChoice a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                              rangeSpan c]
    ChanSend a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                  rangeSpan c]
    ChanNonDetSend a b c d -> joinRanges [rangeSpan a, rangeSpan b,
                                          rangeSpan c, rangeSpan d]
    ChanRecv a b c d -> joinRanges [rangeSpan a, rangeSpan b,
                                    rangeSpan c, rangeSpan d]
    FQTermEvent a b -> joinRanges [rangeSpan a, rangeSpan b]
    FQExternalPrefixChoice a b -> joinRanges [rangeSpan a, rangeSpan b]
    FQInternalPrefixChoice a b -> joinRanges [rangeSpan a, rangeSpan b]
    FQChanSend a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                    rangeSpan c]
    FQChanNonDetSend a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                          rangeSpan c]
    FQChanRecv a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                    rangeSpan c]

instance GetRange EVENT_SET where
  getRange x = case x of
    EventSet _ p -> p
  rangeSpan x = case x of
    EventSet a b -> joinRanges [rangeSpan a, rangeSpan b]

instance GetRange RenameKind where
  getRange = const nullRange
  rangeSpan x = case x of
    TotOp -> []
    PartOp -> []
    BinPred -> []

instance GetRange Rename where
  getRange = const nullRange
  rangeSpan x = case x of
    Rename a b -> joinRanges [rangeSpan a, rangeSpan b]

instance GetRange RENAMING where
  getRange = const nullRange
  rangeSpan x = case x of
    Renaming a -> joinRanges [rangeSpan a]

instance GetRange PROC_ALPHABET where
  getRange = const nullRange
  rangeSpan x = case x of
    ProcAlphabet a -> joinRanges [rangeSpan a]

instance GetRange ProcProfile where
  getRange = const nullRange
  rangeSpan x = case x of
    ProcProfile a b -> joinRanges [rangeSpan a, rangeSpan b]

instance GetRange FQ_PROCESS_NAME where
  getRange = const nullRange
  rangeSpan x = case x of
    PROCESS_NAME a -> joinRanges [rangeSpan a]
    FQ_PROCESS_NAME a b -> joinRanges [rangeSpan a, rangeSpan b]

instance GetRange TypedChanName where
  getRange = const nullRange
  rangeSpan x = case x of
    TypedChanName a b -> joinRanges [rangeSpan a, rangeSpan b]

instance GetRange CommType where
  getRange = const nullRange
  rangeSpan x = case x of
    CommTypeSort a -> joinRanges [rangeSpan a]
    CommTypeChan a -> joinRanges [rangeSpan a]

instance GetRange PROCESS where
  getRange x = case x of
    Skip p -> p
    Stop p -> p
    Div p -> p
    Run _ p -> p
    Chaos _ p -> p
    PrefixProcess _ _ p -> p
    Sequential _ _ p -> p
    ExternalChoice _ _ p -> p
    InternalChoice _ _ p -> p
    Interleaving _ _ p -> p
    SynchronousParallel _ _ p -> p
    GeneralisedParallel _ _ _ p -> p
    AlphabetisedParallel _ _ _ _ p -> p
    Hiding _ _ p -> p
    RenamingProcess _ _ p -> p
    ConditionalProcess _ _ _ p -> p
    NamedProcess _ _ p -> p
    FQProcess _ _ p -> p
  rangeSpan x = case x of
    Skip a -> joinRanges [rangeSpan a]
    Stop a -> joinRanges [rangeSpan a]
    Div a -> joinRanges [rangeSpan a]
    Run a b -> joinRanges [rangeSpan a, rangeSpan b]
    Chaos a b -> joinRanges [rangeSpan a, rangeSpan b]
    PrefixProcess a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                       rangeSpan c]
    Sequential a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                    rangeSpan c]
    ExternalChoice a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                        rangeSpan c]
    InternalChoice a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                        rangeSpan c]
    Interleaving a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                      rangeSpan c]
    SynchronousParallel a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                             rangeSpan c]
    GeneralisedParallel a b c d -> joinRanges [rangeSpan a,
                                               rangeSpan b, rangeSpan c, rangeSpan d]
    AlphabetisedParallel a b c d e -> joinRanges [rangeSpan a,
                                                  rangeSpan b, rangeSpan c, rangeSpan d,
                                                  rangeSpan e]
    Hiding a b c -> joinRanges [rangeSpan a, rangeSpan b, rangeSpan c]
    RenamingProcess a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                         rangeSpan c]
    ConditionalProcess a b c d -> joinRanges [rangeSpan a, rangeSpan b,
                                              rangeSpan c, rangeSpan d]
    NamedProcess a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                      rangeSpan c]
    FQProcess a b c -> joinRanges [rangeSpan a, rangeSpan b,
                                   rangeSpan c]
