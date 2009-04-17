package body SDL.Mixer is

  function PlayChannel
   (Channel : C.int;
    Chunk   : Chunk_Access_t;
    Loops   : C.int) return C.int is
  begin
    return PlayChannelTimed (Channel, Chunk, Loops, -1);
  end PlayChannel;

  function FadeInChannel
   (Channel : C.int;
    Chunk   : Chunk_Access_t;
    Loops   : C.int;
    Ms      : C.int) return C.int is
  begin
    return FadeInChannelTimed (Channel, Chunk, Loops, Ms, -1);
  end FadeInChannel;

end SDL.Mixer;
