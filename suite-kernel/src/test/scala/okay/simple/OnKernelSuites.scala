package okay.simple

import okay.simple.kernel.OnKernel

/** the interface's suite through okay-kernel's protocol: on its reference machine, and on okay-java */
class OnKernelStepsSuite extends InterfaceSuite(OnKernel.on(okay.kernel.steps.Steps))
class OnKernelJavaSuite extends InterfaceSuite(OnKernel.on(okay.core.Kernel.EFFECTS))
