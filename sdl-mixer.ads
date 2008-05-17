with SDL.audio;

package SDL.mixer is
  package au renames SDL.audio;

  -- The internal format for an audio chunk.
  type Chunk is record
    allocated: c.int;
         abuf: uint8_ptr;
         alen: uint32;
       volume: uint8;
  end record;
  type Chunk_ptr is access all Chunk;
  pragma convention (c, Chunk);
  pragma convention (c, Chunk_ptr);

  -- The different fading types supported.
  type Fading is (
    NO_FADING,
    FADING_OUT,
    FADING_IN
  );
  for Fading use (
    NO_FADING => 0,
    FADING_OUT => 1,
    FADING_IN => 2
  );
  pragma convention (c, Fading);

  -- Type of music
  type MusicType is (
    MUSIC_NONE,
    MUSIC_CMD,
    MUSIC_WAV,
    MUSIC_MOD,
    MUSIC_MID,
    MUSIC_OGG,
    MUSIC_MP3,
    MUSIC_MP3_MAD
  );
  for MusicType use (
    MUSIC_NONE => 0,
    MUSIC_CMD => 1,
    MUSIC_WAV => 2,
    MUSIC_MOD => 3,
    MUSIC_MID => 4,
    MUSIC_OGG => 5,
    MUSIC_MP3 => 6,
    MUSIC_MP3_MAD => 7
  );
  pragma convention (c, MusicType);

  -- The internal format for a music chunk interpreted via mikmod.
  type Music is new void_ptr;
  type Music_ptr is access all void_ptr;
  pragma convention (c, Music);
  pragma convention (c, Music_ptr);

  --
  -- API functions.
  --

  -- Open the mixer with a certain audio format.
  function OpenAudio (freq: c.int; format: au.audio_format;
    channels, chunksize: c.int) return c.int;
  function open_audio (freq: c.int; format: au.audio_format;
    channels, chunksize: c.int) return c.int;
  pragma import (c, OpenAudio, "Mix_OpenAudio");
  pragma import (c, open_audio, "Mix_OpenAudio");

  -- Dynamically change the number of channels managed by the mixer.
  function AllocateChannels (chans: c.int) return c.int;
  function allocate_channels (chans: c.int) return c.int;
  pragma import (c, AllocateChannels, "Mix_AllocateChannels");
  pragma import (c, allocate_channels, "Mix_AllocateChannels");

  -- Find out what the actual audio device parameters are.
  function QuerySpec (freq: access c.int; format: access au.audio_format;
    channels: access c.int) return c.int;
  function query_spec (freq: access c.int; format: access au.audio_format;
    channels: access c.int) return c.int;
  pragma import (c, QuerySpec, "Mix_QuerySpec");
  pragma import (c, query_spec, "Mix_QuerySpec");

  -- Load a wave file or a music (.mod .s3m .it .xm)

end SDL.mixer;
