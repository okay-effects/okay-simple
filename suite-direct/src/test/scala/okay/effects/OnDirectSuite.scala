package okay.effects

import okay.effects.direct.OnDirect

/** the interface's suite on okay-direct, its Loom implementation: one-shot */
class OnDirectLoomSuite extends InterfaceSuite(new OnDirect(new okay.direct.loom.LoomEffects))

/** the same on its threads */
class OnDirectThreadsSuite extends InterfaceSuite(new OnDirect(new okay.direct.threads.ThreadEffects))
