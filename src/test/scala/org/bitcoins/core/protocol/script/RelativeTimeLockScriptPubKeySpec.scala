package org.bitcoins.core.protocol.script

import org.bitcoins.core.gen.ScriptGenerators
import org.scalacheck.{Prop, Properties}

/**
  * Created by tom on 8/23/16.
  */
class RelativeTimeLockScriptPubKeySpec extends Properties("RelativeTimeLockScriptPubKeySpec") {
  property("Serialization Symmetry") =
    Prop.forAll(ScriptGenerators.relativeTimeLockScriptPubKey) { relativeTimeLockScriptPubKey =>
      RelativeLockTimeScriptPubKey(relativeTimeLockScriptPubKey.hex) == relativeTimeLockScriptPubKey
    }
}
