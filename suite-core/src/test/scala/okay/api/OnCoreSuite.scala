package okay.api

import okay.api.core.OnCoreEffects

/** the interface's suite on okay-core's machine */
class OnCoreSuite extends InterfaceSuite(OnCoreEffects.effects)
