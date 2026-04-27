module params{

    // simulation of verification
    val SIMULATION = false

    // ==== MODEL SIZE PARAMETERS ====
    val UNIVERSE_SIZE        = if (SIMULATION) 5 else 2  // how many different values can be used in the protocol (per entity)
    val PARTY_SIZE           = if (SIMULATION) 5  else 3     // how many parties are there in the protocol
    val CORRUPTED_PARTY_SIZE = if (SIMULATION) 2  else 1 // how many parties can be corrupted
    val SID_SIZE             = if (SIMULATION) 3  else 1  // how many different sessions can be run
    val MAX_LOG_SIZE         = if (SIMULATION) 2000  else 7 // maximum size of the log (to prevent unbounded growth)
    val RESPONSIVE_SIMULATOR = if (SIMULATION) false else true // false = adversarial (can drop SIM control); true = cooperative
    val NONCE_INCREMENT      = if (SIMULATION) 1     else 0
}