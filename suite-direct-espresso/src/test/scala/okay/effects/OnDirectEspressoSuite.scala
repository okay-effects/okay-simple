package okay.effects

import okay.effects.direct.OnDirect

/** the interface's suite on okay-direct's Espresso implementation: multi-shot, every test */
class OnDirectEspressoSuite extends InterfaceSuite(new OnDirect(new okay.direct.espresso.EspressoEffects))
