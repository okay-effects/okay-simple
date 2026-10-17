package okay.api

import okay.api.kernel.OnKernel

/** the interface's suite through okay-kernel's protocol on okay-core's bridge */
class OnKernelCoreSuite extends InterfaceSuite(OnKernel.on(okay.core.kernel.OnCore))
