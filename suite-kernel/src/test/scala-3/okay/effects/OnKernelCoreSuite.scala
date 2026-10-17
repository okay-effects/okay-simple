package okay.effects

import okay.effects.kernel.OnKernel

/** the interface's suite through okay-kernel's protocol on okay-core's bridge */
class OnKernelCoreSuite extends InterfaceSuite(OnKernel.on(okay.core.kernel.OnCore))
