
open SeqAST

(* pour le affectations *)

let statusRun = "StatusRun"
let statusCall = "StatusCall"
let statusWait = "StatusWait"
let statusEnded = "StatusEnded"
 
let tryEnabled = "TryEnabled"
let tryDisabled = "TryEnabled"
let tryCommit = "TryEnabled"

let makeFun name ret args =
  let name = SimpleName name in
    name, Fun (ret, args)

let awake = makeFun "Awake" Void [SchedPool; PiThread]
let canAwake = makeFun "CanAwake" PBool [PiThread; Commit]
let channelDecRefCount = makeFun "ChannelDecRefCount" Void [ Channel ]
let channelIncrRefCount = makeFun "ChannelIncrRefCount" Void [ Channel ]
let fetchInputCommitment = makeFun "FetchInputCommitment" InCommit [PiThread; Channel]
let fetchOutputCommitment = makeFun "FetchOutputCommitment" OutCommit [PiThread; Channel] 

let knowsRegister = makeFun "KnowsRegister" PBool [KnowsSet; Channel]
let knowsSetForgetAll = makeFun "KnowsSetForgetAll" Void [KnowsSet]

let knowsSetForgetToUnknown = makeFun "KnowsSetForgetToUnknown" Void [KnowsSet]
let knowsSetForget = makeFun "KnowsSetForget" (PSet Channel) [KnowsSet]
let knowsSetKnows = makeFun "KnowsSetKnows" (PSet Channel) [KnowsSet]

let registerInputCommitment = makeFun "RegisterInputCommitment" Void [PiThread; Channel; PInt; DLabel]
let registerOutputCommitment = makeFun "RegisterOutputCommitment" Void [PiThread; Channel; Fun (PValue, [PiThread]) ;DLabel ]
let setAdd = makeFun "SetAdd"

(* Thread Synchronization function *)
let waitQueuePush = makeFun "WaitQueuePush" Void [Queue; PiThread]
let readyQueueAdd = makeFun "ReadyQueueAdd" Void [Queue; PiThread]
let releaseAllChannels = makeFun "ReleaseAllChannels" Void [PSet Channel]
let acquire = makeFun "Acquire" Void [Lock]
let release = makeFun "Release" Void [Lock]
let lowLevelYield = makeFun "LowLevelYield" Void []



(* Divers *)
let emptySet = makeFun "EmptySet" (PSet Channel) []
