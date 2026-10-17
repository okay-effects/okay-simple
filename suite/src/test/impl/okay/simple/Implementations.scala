package okay.simple

import okay.simple.free.FreeEffects
import okay.simple.machine.MachineEffects

class FreeSuite extends InterfaceSuite(FreeEffects.effects)
class MachineSuite extends InterfaceSuite(MachineEffects.effects)
