module ServerSentEventGenerator.Constants where

import ServerSentEventGenerator.Class (StringLike)


-- taken from consts.ts
-- why?
-- functions must start with a lower case letter
-- I could have used a type class, but it would have required
-- a function call in front of each data constructor, so I
-- decided to just use the prefix "c"

cDATASTAR, cDATASTAR_EVENT, cDATASTAR_REQUEST, cVERSION, cMorph        :: StringLike a => a
cInner, cOuter, cPrepend, cAppend, cBefore, cAfter, cUpsertAttributes  :: StringLike a => a
cMergeFragments, cMergeSignals, cRemoveFragments, cRemoveSignals       :: StringLike a => a
cExecuteScript, cSelector, cMerge, cSettleDuration, cFragments         :: StringLike a => a
cUseViewTransition, cSignals, cOnlyIfMissing, cPaths, cScript          :: StringLike a => a
cAttributes, cAutoRemove, cDefaultAttributes                           :: StringLike a => a
cEventId, cDefaultEventId, cRetryDuration                              :: StringLike a => a
cDefaultSelector, cDefaultMergeMode                                    :: StringLike a => a

cDefaultSettleDurationMs, cDefaultSseRetryDurationMs                   :: Int
cDefaultFragmentsUseViewTransitions                                    :: Bool
cDefaultOnlyIfMissing                                                  :: Bool
cDefaultAutoRemove                                                     :: Bool
cDefaultUseViewTransition                                              :: Bool
cDATASTAR                           =  "datastar"
cDATASTAR_EVENT                     =  "datastar-event"
cDATASTAR_REQUEST                   =  "datastar-request"
cVERSION                            =  "0.20.1"
cDefaultSettleDurationMs            = 300
cDefaultSseRetryDurationMs          = 1000
cDefaultAttributes                  =  "type: module"
cDefaultEventId                     =  ""
cDefaultFragmentsUseViewTransitions =  False
cDefaultOnlyIfMissing               =  False
cDefaultAutoRemove                  =  True
cDefaultUseViewTransition           =  False
cDefaultSelector                    =  ""
cDefaultMergeMode                   =  "morph"

cMorph                              =  "morph"
cInner                              =  "inner"
cOuter                              =  "outer"
cPrepend                            =  "prepend"
cAppend                             =  "append"
cBefore                             =  "before"
cAfter                              =  "after"
cUpsertAttributes                   =  "upsertAttributes"
cMergeFragments                     =  "datastar-merge-fragments"
cMergeSignals                       =  "datastar-merge-signals"
cRemoveFragments                    =  "datastar-remove-fragments"
cRemoveSignals                      =  "datastar-remove-signals"
cExecuteScript                      =  "datastar-execute-script"
cSelector                           =  "selector"
cMerge                              =  "mergeMode"
cSettleDuration                     =  "settleDuration"
cFragments                          =  "fragments"
cUseViewTransition                  =  "useViewTransition"
cSignals                            =  "signals"
cOnlyIfMissing                      =  "onlyIfMissing"
cPaths                              =  "paths"
cScript                             =  "script"
cAttributes                         =  "attributes"
cEventId                            =  "id"
cRetryDuration                      =  "retry"
cAutoRemove                         =  "autoRemove"
-- added by henry

cData, cEvent, cSColon, cSpace, cType,cBlocking :: StringLike a => a
cDefaultBlocking :: Bool
cData                               =  "data"
cEvent                              =  "event"
cSColon                             =  ": "
cSpace                              =  " "
cType                               = "type"
cBlocking                           = "blocking"
cDefaultBlocking                   = False


-- DATASTAR_KEY =  "datastar"
-- VERSION                   =  "1.0.0-beta.1"
-- VERSION_CLIENT_BYTE_SIZE     =  36235
-- VERSION_CLIENT_BYTE_SIZE_GZIP =  13244
