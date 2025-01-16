module Constants where

import Data.ByteString.Builder

-- taken from consts.ts
-- why?
-- functions must start with a lower case letter
-- I could have used a type class, but it would have required
-- a function call in front of each data constructor, so I
-- decided to just use the prefix "c"

type DsString = Builder

cDATASTAR, cDATASTAR_EVENT, cDATASTAR_REQUEST, cVERSION, cMorph        :: DsString
cInner, cOuter, cPrepend, cAppend, cBefore, cAfter, cUpsertAttributes  :: DsString
cMergeFragments, cMergeSignals, cRemoveFragments, cRemoveSignals       :: DsString
cExecuteScript, cSelector, cMerge, cSettleDuration, cFragments         :: DsString
cUseViewTransition, cSignals, cOnlyIfMissing, cPaths, cScript          :: DsString
cAttributes, cAutoRemove, cDefaultAttributes                           :: DsString
cEventId, cDefaultEventId, cRetryDuration                              :: DsString
cDefaultSelector, cDefaultMergeMode                                    :: DsString

cDefaultSettleDurationMs, cDefaultSseRetryDurationMs                   :: Int
cDefaultFragmentsUseViewTransitions                                    :: Bool
cDefaultOnlyIfMissing                                                  :: Bool
cDefaultAutoRemove                                                     :: Bool
cDefaultUseViewTransition                                              :: Bool

cDATASTAR                           = "datastar"
cDATASTAR_EVENT                     = "datastar-event"
cDATASTAR_REQUEST                   = "datastar-request"
cVERSION                            = "0.20.1"
cDefaultSettleDurationMs            = 300
cDefaultSseRetryDurationMs          = 1000
cDefaultAttributes                  = "type module"
cDefaultEventId                     = ""
cDefaultFragmentsUseViewTransitions = False
cDefaultOnlyIfMissing               = False
cDefaultAutoRemove                  = True
cDefaultUseViewTransition           = False
cDefaultSelector                    = ""
cDefaultMergeMode                   = "morph"

cMorph                              = "morph"
cInner                              = "inner"
cOuter                              = "outer"
cPrepend                            = "prepend"
cAppend                             = "append"
cBefore                             = "before"
cAfter                              = "after"
cUpsertAttributes                   = "upsertAttributes"
cMergeFragments                     = "datastar-merge-fragments"
cMergeSignals                       = "datastar-merge-signals"
cRemoveFragments                    = "datastar-remove-fragments"
cRemoveSignals                      = "datastar-remove-signals"
cExecuteScript                      = "datastar-execute-script"
cSelector                           = "selector"
cMerge                              = "merge"
cSettleDuration                     = "settleDuration"
cFragments                          = "fragments"
cUseViewTransition                  = "useViewTransition"
cSignals                            = "signals"
cOnlyIfMissing                      = "onlyIfMissing"
cPaths                              = "paths"
cScript                             = "script"
cAttributes                         = "attributes"
cEventId                            = "id"
cRetryDuration                      = "retry"
cAutoRemove                         = "autoRemove"

 
