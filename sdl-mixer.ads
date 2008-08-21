with SDL.audio;
with SDL.error;
with SDL.rwops;
with interfaces.c.strings;
with interfaces.c;

package SDL.mixer is
  package au renames SDL.audio;
  package rw renames SDL.rwops;
  package c renames interfaces.c;
  package cs renames interfaces.c.strings;

  use type c.int;

  type mixer_func_t is
    access procedure (udata: void_ptr_t; stream: uint8_ptr; len: c.int);
  pragma convention (c, mixer_func_t);
  type music_finished_func_t is access procedure;
  pragma convention (c, music_finished_func_t);
  type channel_finished_func_t is access procedure (channel: c.int);
  pragma convention (c, channel_finished_func_t);

  -- The internal format for an audio chunk.
  type chunk_t is record
    allocated: c.int;
         abuf: uint8_ptr;
         alen: uint32;
       volume: uint8;
      unused1: uint8; -- XXX: C compiler padding
      unused2: uint8; -- XXX: C compiler padding
      unused3: uint8; -- XXX: C compiler padding
  end record;
  type chunk_ptr_t is access all chunk_t;
  pragma convention (c, chunk_t);
  pragma convention (c, chunk_ptr_t);

  -- The different fading types supported.
  type fading_type_t is (
    NO_FADING,
    FADING_OUT,
    FADING_IN
  );
  for fading_type_t use (
    NO_FADING => 0,
    FADING_OUT => 1,
    FADING_IN => 2
  );
  for fading_type_t'size use c.unsigned'size;
  pragma convention (c, fading_type_t);

  -- Type of music
  type music_type_t is (
    MUSIC_NONE,
    MUSIC_CMD,
    MUSIC_WAV,
    MUSIC_MOD,
    MUSIC_MID,
    MUSIC_OGG,
    MUSIC_MP3,
    MUSIC_MP3_MAD
  );
  for music_type_t use (
    MUSIC_NONE => 0,
    MUSIC_CMD => 1,
    MUSIC_WAV => 2,
    MUSIC_MOD => 3,
    MUSIC_MID => 4,
    MUSIC_OGG => 5,
    MUSIC_MP3 => 6,
    MUSIC_MP3_MAD => 7
  );
  for music_type_t'size use c.unsigned'size;
  pragma convention (c, music_type_t);

  -- The internal format for a music chunk interpreted via mikmod.
  type music_t is new void_ptr_t;
  type music_ptr_t is access all void_ptr_t;
  pragma convention (c, music_t);
  pragma convention (c, music_ptr_t);

  --
  -- API functions.
  --

  -- Open the mixer with a certain audio format.
  function OpenAudio (freq: c.int; format: au.format_t;
    channels, chunksize: c.int) return c.int;
  function open_audio (freq: c.int; format: au.format_t;
    channels, chunksize: c.int) return c.int;
  pragma import (c, OpenAudio, "Mix_OpenAudio");
  pragma import (c, open_audio, "Mix_OpenAudio");

  -- Dynamically change the number of channels managed by the mixer.
  function AllocateChannels (chans: c.int) return c.int;
  function allocate_channels (chans: c.int) return c.int;
  pragma import (c, AllocateChannels, "Mix_AllocateChannels");
  pragma import (c, allocate_channels, "Mix_AllocateChannels");

  -- Find out what the actual audio device parameters are.
  function QuerySpec (freq: access c.int; format: access au.format_t;
    channels: access c.int) return c.int;
  function query_spec (freq: access c.int; format: access au.format_t;
    channels: access c.int) return c.int;
  pragma import (c, QuerySpec, "Mix_QuerySpec");
  pragma import (c, query_spec, "Mix_QuerySpec");

  -- Load a wave file or a music (.mod .s3m .it .xm)
  function LoadWAV_RW (src: rw.rwops_ptr_t; freesrc: c.int) return chunk_ptr_t;
  function load_wav_rw (src: rw.rwops_ptr_t; freesrc: c.int) return chunk_ptr_t;
  pragma import (c, LoadWAV_RW, "Mix_LoadWAV_RW");
  pragma import (c, load_wav_rw, "Mix_LoadWAV_RW");

  function LoadWAV (file: string) return chunk_ptr_t;
  function load_wav (file: string) return chunk_ptr_t renames LoadWAV;
  pragma import (c, LoadWAV, "Mix_LoadWAV_RW");

  -- Free an audio chunk previously loaded.
  procedure FreeChunk (cptr: chunk_ptr_t);
  procedure free_chunk (cptr: chunk_ptr_t) renames FreeChunk;
  pragma import (c, FreeChunk, "Mix_FreeChunk");

  procedure FreeMusic (cptr: music_ptr_t);
  procedure free_music (cptr: music_ptr_t) renames FreeMusic;
  pragma import (c, FreeMusic, "Mix_FreeMusic");

  -- Find out the music format of a mixer music.
  function GetMusicType (mus: music_ptr_t) return music_type_t;
  function get_music_type (mus: music_ptr_t) return music_type_t renames GetMusicType;
  pragma import (c, GetMusicType, "Mix_GetMusicType");

  -- Set a function that is called after all mixing is performed.
  procedure SetPostMix (func: mixer_func_t; data: void_ptr_t);
  procedure set_post_mix (func: mixer_func_t; data: void_ptr_t) renames SetPostMix;
  pragma import (c, SetPostMix, "Mix_SetPostMix");

  -- Add your own music player or additional mixer function.
  procedure HookMusic (func: mixer_func_t; data: void_ptr_t);
  procedure hook_music (func: mixer_func_t; data: void_ptr_t) renames HookMusic; 
  pragma import (c, HookMusic, "Mix_HookMusic");

  -- Add your own callback when the music has finished playing.
  procedure HookMusicFinished (func: music_finished_func_t);
  procedure hook_music_finished (func: music_finished_func_t) renames HookMusicFinished; 
  pragma import (c, HookMusicFinished, "Mix_HookMusicFinished");

  -- Get a pointer to the user data for the current music hook.
  function GetMusicHookData return void_ptr_t;
  function get_music_hook_data return void_ptr_t renames GetMusicHookData;
  pragma import (c, GetMusicHookData, "Mix_GetMusicHookData");

  -- Add your own callback when a channel has finished playing.
  procedure ChannelFinished (func: channel_finished_func_t);
  procedure channel_finished (func: channel_finished_func_t) renames ChannelFinished;
  pragma import (c, ChannelFinished, "Mix_ChannelFinished");
  
  --
  -- Effects API.
  --

  channel_post: constant c.int := -2;

  type effect_func_t is
    access procedure (chan: c.int; stream: void_ptr_t; len: c.int; data: void_ptr_t);
  type effect_done_func_t is
    access procedure (chan: c.int; data: void_ptr_t);
  pragma convention (c, effect_func_t);
  pragma convention (c, effect_done_func_t);

  -- Register a special effect function.
  function RegisterEffect (chan: c.int; func: effect_func_t;
    func_done: effect_done_func_t; data: void_ptr_t) return c.int;
  function register_effect (chan: c.int; func: effect_func_t;
    func_done: effect_done_func_t; data: void_ptr_t) return c.int renames RegisterEffect;
  pragma import (c, RegisterEffect, "Mix_RegisterEffect");

  -- Unregister a special effect.
  function UnregisterEffect (chan: c.int; func: effect_func_t;
    func_done: effect_done_func_t) return c.int;
  function unregister_effect (chan: c.int; func: effect_func_t;
    func_done: effect_done_func_t) return c.int renames UnregisterEffect;
  pragma import (c, UnregisterEffect, "Mix_UnregisterEffect");

  -- Unregister all special effects.
  function UnregisterAllEffects (chan: c.int) return c.int;
  function unregister_all_effects (chan: c.int) return c.int
    renames UnregisterAllEffects;
  pragma import (c, UnregisterAllEffects, "Mix_UnregisterAllEffects");

  -- Set the panning of a channel.
  function SetPanning (chan: c.int; left, right: uint8) return c.int;
  function set_panning (chan: c.int; left, right: uint8) return c.int
    renames SetPanning;
  pragma import (c, SetPanning, "Mix_SetPanning");

  -- Set the position of a channel.
  subtype position_type_t is int16 range 0 .. 360;

  function SetPosition (chan: c.int; angle: position_type_t; distance: uint8)
    return c.int;
  function set_position (chan: c.int; angle: position_type_t; distance: uint8)
    return c.int renames SetPosition;
  pragma import (c, SetPosition, "Mix_SetPosition");

  -- Set the "distance" of a channel.
  function SetDistance (chan: c.int; distance: uint8) return c.int;
  function set_distance (chan: c.int; distance: uint8) return c.int
    renames SetDistance;
  pragma import (c, SetDistance, "Mix_SetDistance");

  -- Causes a channel to reverse its stereo.
  function SetReverseStereo (chan: c.int; distance: uint8) return c.int;
  function set_reverse_stereo (chan: c.int; distance: uint8) return c.int
    renames SetReverseStereo;
  pragma import (c, SetReverseStereo, "Mix_SetReverseStereo");

  -- Reserve the first channels (0 -> n-1) for the application.
  function ReserveChannels (num: c.int) return c.int;
  function reserve_channels (num: c.int) return c.int
    renames ReserveChannels;
  pragma import (c, ReserveChannels, "Mix_ReverseChannels");

  -- Attach a tag to a channel.
  function GroupChannel (which, tag: c.int) return c.int;
  function group_channel (which, tag: c.int) return c.int renames GroupChannel;
  pragma import (c, GroupChannel, "Mix_GroupChannel");

  -- Assign several consecutive channels to a group.
  function GroupChannels (from, to, tag: c.int) return c.int;
  function group_channels (from, to, tag: c.int) return c.int renames GroupChannels;
  pragma import (c, GroupChannels, "Mix_GroupChannels");

  -- Finds the first available channel in a group of channels.
  function GroupAvailable (tag: c.int) return c.int;
  function group_available (tag: c.int) return c.int renames GroupAvailable;
  pragma import (c, GroupAvailable, "Mix_GroupAvailable");

  -- Returns the number of channels in a group.
  function GroupCount (tag: c.int) return c.int;
  function group_count (tag: c.int) return c.int renames GroupCount;
  pragma import (c, GroupCount, "Mix_GroupCount");

  -- Finds the "oldest" sample playing in a group of channels.
  function GroupOldest (tag: c.int) return c.int;
  function group_oldest (tag: c.int) return c.int renames GroupOldest;
  pragma import (c, GroupOldest, "Mix_GroupOldest");

  -- Finds the "most recent" (i.e. last) sample playing in a group of channels.
  function GroupNewer (tag: c.int) return c.int;
  function group_newer (tag: c.int) return c.int renames GroupNewer;
  pragma import (c, GroupNewer, "Mix_GroupNewer");

  -- Play an audio chunk on a specific channel.
  function PlayChannelTimed (chan: c.int; chunk: chunk_ptr_t;
    loops, ticks: c.int) return c.int;
  function play_channel_timed (chan: c.int; chunk: chunk_ptr_t;
    loops, ticks: c.int) return c.int renames PlayChannelTimed;
  pragma import (c, PlayChannelTimed, "Mix_PlayChannelTimed");

  function PlayChannel (chan: c.int; chunk: chunk_ptr_t; loops: c.int) return c.int;
  function play_channel (chan: c.int; chunk: chunk_ptr_t; loops: c.int) return c.int
    renames PlayChannel;
  pragma inline (PlayChannel); 

  function PlayMusic (chan: c.int; chunk: chunk_ptr_t; loops: c.int) return c.int;
  function play_music (chan: c.int; chunk: chunk_ptr_t; loops: c.int) return c.int
    renames PlayMusic;
  pragma import (c, PlayMusic, "Mix_PlayMusic");

  -- Fade in music or a channel over "ms" milliseconds
  function FadeInMusic (music: music_ptr_t; loops, ms: c.int) return c.int;
  function fade_in_music (music: music_ptr_t; loops, ms: c.int) return c.int
    renames FadeInMusic;
  pragma import (c, FadeInMusic, "Mix_FadeInMusic");

  function FadeInMusicPos (music: music_ptr_t; loops, ms: c.int;
    position: c.double) return c.int;
  function fade_in_music_pos (music: music_ptr_t; loops, ms: c.int;
    position: c.double) return c.int renames FadeInMusicPos;
  pragma import (c, FadeInMusicPos, "Mix_FadeInMusicPos");

  function FadeInChannel (chan: c.int; chunk: chunk_ptr_t; loops, ms: c.int)
    return c.int;
  function fade_in_channel (chan: c.int; chunk: chunk_ptr_t; loops, ms: c.int)
    return c.int renames FadeInChannel;
  pragma inline (FadeInChannel);

  function FadeInChannelTimed (chan: c.int; chunk: chunk_ptr_t;
    loops, ms, ticks: c.int) return c.int;
  function fade_in_channel_timed (chan: c.int; chunk: chunk_ptr_t;
    loops, ms, ticks: c.int) return c.int renames FadeInChannelTimed;
  pragma import (c, FadeInChannelTimed, "Mix_FadeInChannelTimed"); 

  -- Set the volume in the range of 0-128 of a specific channel or chunk.
  subtype volume_type_t is c.int range 0 .. 128;
  function Volume (chan: c.int; volume: volume_type_t) return c.int;
  function VolumeChunk (chunk: chunk_ptr_t; volume: volume_type_t) return c.int;
  function VolumeMusic (volume: volume_type_t) return c.int;
  function volume_chunk (chunk: chunk_ptr_t; volume: volume_type_t) return c.int
    renames VolumeChunk;
  function volume_music (volume: volume_type_t) return c.int
    renames VolumeMusic;
  pragma import (c, Volume, "Mix_Volume");
  pragma import (c, VolumeChunk, "Mix_VolumeChunk");
  pragma import (c, VolumeMusic, "Mix_VolumeMusic");

  -- Halt playing of a particular channel.
  function HaltChannel (chan: c.int) return c.int;
  function HaltGroup (tag: c.int) return c.int;
  function HaltMusic return c.int;
  function halt_channel (chan: c.int) return c.int renames HaltChannel;
  function halt_group (tag: c.int) return c.int renames HaltGroup;
  function halt_music return c.int renames HaltMusic;
  pragma import (c, HaltChannel, "Mix_HaltChannel");
  pragma import (c, HaltGroup, "Mix_HaltGroup");
  pragma import (c, HaltMusic, "Mix_HaltMusic");

  -- Change the expiration delay for a particular channel.
  function ExpireChannel (chan, ticks: c.int) return c.int;
  function expire_channel (chan, ticks: c.int) return c.int renames ExpireChannel;
  pragma import (c, ExpireChannel, "Mix_ExpireChannel");

  -- Halt a channel, fading it out progressively until it's silent.
  function FadeOutChannel (which, ms: c.int) return c.int;
  function FadeOutGroup (tag, ms: c.int) return c.int;
  function FadeOutMusic (ms: c.int) return c.int;
  function fade_out_Channel (which, ms: c.int) return c.int renames FadeOutChannel;
  function fade_out_Group (tag, ms: c.int) return c.int renames FadeOutGroup;
  function fade_out_Music (ms: c.int) return c.int renames FadeOutMusic;
  pragma import (c, FadeOutChannel, "Mix_FadeOutChannel");
  pragma import (c, FadeOutGroup, "Mix_FadeOutGroup");
  pragma import (c, FadeOutMusic, "Mix_FadeOutMusic");

  -- Query the fading status of a channel
  function FadingMusic return fading_type_t;
  function fading_music return fading_type_t renames FadingMusic;
  function FadingChannel (chan: c.int) return fading_type_t;
  function fading_channel (chan: c.int) return fading_type_t renames FadingChannel;
  pragma import (c, FadingMusic, "Mix_FadingMusic");
  pragma import (c, FadingChannel, "Mix_FadingChannel");

  -- Pause/Resume a particular channel
  procedure pause (chan: c.int);
  procedure resume (chan: c.int);
  function paused (chan: c.int) return c.int;
  pragma import (c, pause, "Mix_Pause");
  pragma import (c, resume, "Mix_Resume");
  pragma import (c, paused, "Mix_Paused");

  -- Pause/Resume the music stream
  procedure PauseMusic;
  procedure ResumeMusic;
  procedure RewindMusic;
  function PausedMusic return c.int;
  procedure pause_music renames PauseMusic;
  procedure resume_music renames ResumeMusic;
  procedure rewind_music renames RewindMusic;
  function paused_music return c.int renames PausedMusic;
  pragma import (c, PauseMusic, "Mix_PauseMusic");
  pragma import (c, ResumeMusic, "Mix_ResumeMusic");
  pragma import (c, RewindMusic, "Mix_RewindMusic");
  pragma import (c, PausedMusic, "Mix_PausedMusic");

  -- Set the current position in the music stream.
  function SetMusicPosition (position: c.double) return c.int;
  function set_music_position (position: c.double) return c.int renames SetMusicPosition;
  pragma import (c, SetMusicPosition, "Mix_SetMusicPosition");

  -- Check the status of a specific channel.
  function SetPlaying (chan: c.int) return c.int;
  function set_playing (chan: c.int) return c.int renames SetPlaying;
  pragma import (c, SetPlaying, "Mix_SetPlaying");

  function SetPlayingMusic return c.int;
  function set_playing_music return c.int renames SetPlayingMusic;
  pragma import (c, SetPlayingMusic, "Mix_SetPlayingMusic");

  -- Synchro value is set by MikMod from modules while playing.
  function SetSynchroValue (chan: c.int) return c.int;
  function set_synchro_value (chan: c.int) return c.int renames SetSynchroValue;
  pragma import (c, SetSynchroValue, "Mix_SetSynchroValue");

  function GetSynchroValue return c.int;
  function get_synchro_value return c.int renames GetSynchroValue;
  pragma import (c, GetSynchroValue, "Mix_GetSynchroValue");
 
  -- Get the Mix_Chunk currently associated with a mixer channel.
  function GetChunk (chan: c.int) return chunk_ptr_t;
  function get_chunk (chan: c.int) return chunk_ptr_t renames GetChunk;
  pragma import (c, GetChunk, "Mix_GetChunk");

  -- Close the mixer, halting all playing audio.
  procedure CloseAudio;
  procedure close_audio renames CloseAudio;
  pragma import (c, CloseAudio, "Mix_CloseAudio");

  function GetError return cs.chars_ptr renames SDL.error.GetError;
  function GetError return string renames SDL.error.GetError;
  function get_error return cs.chars_ptr renames SDL.error.get_error;
  function get_error return string renames SDL.error.get_error;

end SDL.mixer;
