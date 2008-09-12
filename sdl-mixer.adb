package body SDL.mixer is

  function PlayChannel
   (chan  : c.int;
    chunk : chunk_access_t;
    loops : c.int) return c.int is
  begin
    return PlayChannelTimed (chan, chunk, loops, -1);
  end PlayChannel;

  function FadeInChannel
   (chan  : c.int;
    chunk : chunk_access_t;
    loops : c.int;
    ms    : c.int) return c.int is
  begin
    return FadeInChannelTimed (chan, chunk, loops, ms, -1);
  end FadeInChannel;

end SDL.mixer;
