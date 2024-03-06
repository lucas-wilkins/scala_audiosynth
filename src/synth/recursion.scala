package synth

import synth.base.{AtomicSynth, Synth}

object recursion {

  /* TODO: This perhaps shouldn't be atomic, but we need to
           implement a way of dealing with finding all the unique
           patches in a way that doesn't fail
   */
  class Socket extends AtomicSynth {
    private var child: Option[Synth] = None

    override def calculate(): Double =
      child.map(_.outputValue).getOrElse(0.0)


    def connect(synth: Synth): Unit = {
      child = Some(synth)
    }

    def disconnect(): Unit = {
      child = None
    }

    override def toString = s"Socket(${child.map(_.getClass.getSimpleName).getOrElse("--")})"
  }

  object Socket {
    def apply(): Socket = new Socket
  }
}
