with SDL.Audio;
with SDL.Error;
with SDL.RWops;
with Interfaces.C.Strings;
with Interfaces.C;

package SDL.Mixer is
  package C renames Interfaces.C;
  package Cs renames Interfaces.C.Strings;

  use type C.int;

  type Mixer_Func_t is access procedure (User_Data : Void_Ptr_t; Stream : Uint8_Ptr_t; Length : C.int);
  pragma Convention (C, Mixer_Func_t);

  type Music_Finished_Func_t is access procedure;
  pragma Convention (C, Music_Finished_Func_t);

  type Channel_Finished_Func_t is access procedure (Channel : C.int);
  pragma Convention (C, Channel_Finished_Func_t);

  -- The internal format for an audio chunk.
  type Chunk_t is record
    Allocated : C.int;
    Abuf      : Uint8_Ptr_t;
    Alen      : Uint32_t;
    Volume    : Uint8_t;
    Unused1   : Uint8_t; -- XXX: C compiler padding
    Unused2   : Uint8_t; -- XXX: C compiler padding
    Unused3   : Uint8_t; -- XXX: C compiler padding
  end record;
  type Chunk_Access_t is access all Chunk_t;
  pragma Convention (C, Chunk_t);
  pragma Convention (C, Chunk_Access_t);

  -- The different fading types supported.
  type Fading_Type_t is (No_Fading, Fading_Out, Fading_In);
  for Fading_Type_t use (No_Fading => 0, Fading_Out => 1, Fading_In => 2);
  for Fading_Type_t'Size use C.unsigned'Size;
  pragma Convention (C, Fading_Type_t);

  -- Type of music
  type Music_Type_t is (
    Music_None,
    Music_Cmd,
    Music_WAV,
    Music_Mod,
    Music_Mid,
    Music_Ogg,
    Music_MP3,
    Music_MP3_Mad);
  for Music_Type_t use
   (Music_None    => 0,
    Music_Cmd     => 1,
    Music_WAV     => 2,
    Music_Mod     => 3,
    Music_Mid     => 4,
    Music_Ogg     => 5,
    Music_MP3     => 6,
    Music_MP3_Mad => 7);
  for Music_Type_t'Size use C.unsigned'Size;
  pragma Convention (C, Music_Type_t);

  -- The internal format for a music chunk interpreted via mikmod.
  type Music_t is new Void_Ptr_t;
  type Music_Access_t is access all Void_Ptr_t;
  pragma Convention (C, Music_t);
  pragma Convention (C, Music_Access_t);

  --
  -- API functions.
  --

-- Open the mixer with a certain audio format.
  function OpenAudio
   (Freq      : C.int;
    Format    : Audio.Format_t;
    Channels  : C.int;
    Chunksize : C.int)
    return      C.int;
  function Open_Audio
   (Freq      : C.int;
    Format    : Audio.Format_t;
    Channels  : C.int;
    Chunksize : C.int)
    return      C.int renames OpenAudio;
  pragma Import (C, OpenAudio, "Mix_OpenAudio");

  -- Dynamically change the number of channels managed by the mixer.
  function AllocateChannels (Channels : C.int) return C.int;
  function Allocate_Channels (Channels : C.int) return C.int renames AllocateChannels;
  pragma Import (C, AllocateChannels, "Mix_AllocateChannels");

  -- Find out what the actual audio device parameters are.
  function QuerySpec
   (Freq     : access C.int;
    Format   : access Audio.Format_t;
    Channels : access C.int)
    return     C.int;
  function Query_Spec
   (Freq     : access C.int;
    Format   : access Audio.Format_t;
    Channels : access C.int)
    return     C.int renames QuerySpec;
  pragma Import (C, QuerySpec, "Mix_QuerySpec");

  -- Load a wave file or a music (.mod .s3m .it .xm)
  function LoadWAV_RW (Source : RWops.RWops_Access_t; Free_Source : C.int) return Chunk_Access_t;
  function Load_WAV_RW (Source : RWops.RWops_Access_t; Free_Source : C.int) return Chunk_Access_t renames LoadWAV_RW;
  pragma Import (C, LoadWAV_RW, "Mix_LoadWAV_RW");

  function LoadWAV (File : String) return Chunk_Access_t;
  function Load_WAV (File : String) return Chunk_Access_t renames LoadWAV;
  pragma Import (C, LoadWAV, "Mix_LoadWAV_RW");

  -- Free an audio chunk previously loaded.
  procedure FreeChunk (Chunk : Chunk_Access_t);
  procedure Free_Chunk (Chunk : Chunk_Access_t) renames FreeChunk;
  pragma Import (C, FreeChunk, "Mix_FreeChunk");

  procedure FreeMusic (Chunk : Music_Access_t);
  procedure Free_Music (Chunk : Music_Access_t) renames FreeMusic;
  pragma Import (C, FreeMusic, "Mix_FreeMusic");

  -- Find out the music format of a mixer music.
  function GetMusicType (Music : Music_Access_t) return Music_Type_t;
  function Get_Music_Type (Music : Music_Access_t) return Music_Type_t renames GetMusicType;
  pragma Import (C, GetMusicType, "Mix_GetMusicType");

  -- Set a function that is called after all mixing is performed.
  procedure SetPostMix (Func : Mixer_Func_t; Data : Void_Ptr_t);
  procedure Set_Post_Mix (Func : Mixer_Func_t; Data : Void_Ptr_t) renames SetPostMix;
  pragma Import (C, SetPostMix, "Mix_SetPostMix");

  -- Add your own music player or additional mixer function.
  procedure HookMusic (Func : Mixer_Func_t; Data : Void_Ptr_t);
  procedure Hook_Music (Func : Mixer_Func_t; Data : Void_Ptr_t) renames HookMusic;
  pragma Import (C, HookMusic, "Mix_HookMusic");

  -- Add your own callback when the music has finished playing.
  procedure HookMusicFinished (Func : Music_Finished_Func_t);
  procedure Hook_Music_Finished (Func : Music_Finished_Func_t) renames HookMusicFinished;
  pragma Import (C, HookMusicFinished, "Mix_HookMusicFinished");

  -- Get a pointer to the user data for the current music hook.
  function GetMusicHookData return Void_Ptr_t;
  function Get_Music_Hook_Data return Void_Ptr_t renames GetMusicHookData;
  pragma Import (C, GetMusicHookData, "Mix_GetMusicHookData");

  -- Add your own callback when a channel has finished playing.
  procedure ChannelFinished (Func : Channel_Finished_Func_t);
  procedure Channel_Finished (Func : Channel_Finished_Func_t) renames ChannelFinished;
  pragma Import (C, ChannelFinished, "Mix_ChannelFinished");

  --
  -- Effects API.
  --

  Channel_Post : constant C.int := -2;

  type Effect_Func_t is access procedure
    (Channel : C.int;
     Stream  : Void_Ptr_t;
     Length  : C.int;
     Data    : Void_Ptr_t);
  type Effect_Done_Func_t is access procedure (Channel : C.int; Data : Void_Ptr_t);
  pragma Convention (C, Effect_Func_t);
  pragma Convention (C, Effect_Done_Func_t);

  -- Register a special effect function.
  function RegisterEffect
   (Channel   : C.int;
    Func      : Effect_Func_t;
    Func_Done : Effect_Done_Func_t;
    Data      : Void_Ptr_t)
    return      C.int;
  function Register_Effect
   (Channel   : C.int;
    Func      : Effect_Func_t;
    Func_Done : Effect_Done_Func_t;
    Data      : Void_Ptr_t)
    return      C.int renames RegisterEffect;
  pragma Import (C, RegisterEffect, "Mix_RegisterEffect");

  -- Unregister a special effect.
  function UnregisterEffect
   (Channel   : C.int;
    Func      : Effect_Func_t;
    Func_Done : Effect_Done_Func_t)
    return      C.int;
  function Unregister_Effect
   (Channel   : C.int;
    Func      : Effect_Func_t;
    Func_Done : Effect_Done_Func_t)
    return      C.int renames UnregisterEffect;
  pragma Import (C, UnregisterEffect, "Mix_UnregisterEffect");

  -- Unregister all special effects.
  function UnregisterAllEffects (Channel : C.int) return C.int;
  function Unregister_All_Effects (Channel : C.int) return C.int renames UnregisterAllEffects;
  pragma Import (C, UnregisterAllEffects, "Mix_UnregisterAllEffects");

  -- Set the panning of a channel.
  function SetPanning
   (Channel : C.int;
    Left    : Uint8_t;
    Right   : Uint8_t)
    return  C.int;
  function Set_Panning
   (Channel : C.int;
    Left    : Uint8_t;
    Right   : Uint8_t)
    return  C.int renames SetPanning;
  pragma Import (C, SetPanning, "Mix_SetPanning");

  -- Set the position of a channel.
  subtype Position_Type_t is Int16_t range 0 .. 360;

  function SetPosition
   (Channel  : C.int;
    Angle    : Position_Type_t;
    Distance : Uint8_t)
    return     C.int;
  function Set_Position
   (Channel  : C.int;
    Angle    : Position_Type_t;
    Distance : Uint8_t)
    return     C.int renames SetPosition;
  pragma Import (C, SetPosition, "Mix_SetPosition");

  -- Set the "distance" of a channel.
  function SetDistance (Channel : C.int; Distance : Uint8_t) return C.int;
  function Set_Distance (Channel : C.int; Distance : Uint8_t) return C.int renames SetDistance;
  pragma Import (C, SetDistance, "Mix_SetDistance");

  -- Causes a channel to reverse its stereo.
  function SetReverseStereo (Channel : C.int; Distance : Uint8_t) return C.int;
  function Set_Reverse_Stereo (Channel : C.int; Distance : Uint8_t) return C.int renames SetReverseStereo;
  pragma Import (C, SetReverseStereo, "Mix_SetReverseStereo");

  -- Reserve the first channels (0 -> n-1) for the application.
  function ReserveChannels (Num : C.int) return C.int;
  function Reserve_Channels (Num : C.int) return C.int renames ReserveChannels;
  pragma Import (C, ReserveChannels, "Mix_ReverseChannels");

  -- Attach a tag to a channel.
  function GroupChannel (Which : C.int; Tag : C.int) return C.int;
  function Group_Channel (Which : C.int; Tag : C.int) return C.int renames GroupChannel;
  pragma Import (C, GroupChannel, "Mix_GroupChannel");

  -- Assign several consecutive channels to a group.
  function GroupChannels
   (From : C.int;
    To   : C.int;
    Tag  : C.int)
    return C.int;
  function Group_Channels
   (From : C.int;
    To   : C.int;
    Tag  : C.int)
    return C.int renames GroupChannels;
  pragma Import (C, GroupChannels, "Mix_GroupChannels");

  -- Finds the first available channel in a group of channels.
  function GroupAvailable (Tag : C.int) return C.int;
  function Group_Available (Tag : C.int) return C.int renames GroupAvailable;
  pragma Import (C, GroupAvailable, "Mix_GroupAvailable");

  -- Returns the number of channels in a group.
  function GroupCount (Tag : C.int) return C.int;
  function Group_Count (Tag : C.int) return C.int renames GroupCount;
  pragma Import (C, GroupCount, "Mix_GroupCount");

  -- Finds the "oldest" sample playing in a group of channels.
  function GroupOldest (Tag : C.int) return C.int;
  function Group_Oldest (Tag : C.int) return C.int renames GroupOldest;
  pragma Import (C, GroupOldest, "Mix_GroupOldest");

  -- Finds the "most recent" (i.e. last) sample playing in a group of channels.
  function GroupNewer (Tag : C.int) return C.int;
  function Group_Newer (Tag : C.int) return C.int renames GroupNewer;
  pragma Import (C, GroupNewer, "Mix_GroupNewer");

  -- Play an audio chunk on a specific channel.
  function PlayChannelTimed
   (Channel : C.int;
    Chunk   : Chunk_Access_t;
    Loops   : C.int;
    Ticks   : C.int)
    return  C.int;
  function Play_Channel_Timed
   (Channel : C.int;
    Chunk   : Chunk_Access_t;
    Loops   : C.int;
    Ticks   : C.int)
    return  C.int renames PlayChannelTimed;
  pragma Import (C, PlayChannelTimed, "Mix_PlayChannelTimed");

  function PlayChannel
   (Channel : C.int;
    Chunk   : Chunk_Access_t;
    Loops   : C.int)
    return  C.int;
  function Play_Channel
   (Channel : C.int;
    Chunk   : Chunk_Access_t;
    Loops   : C.int)
    return  C.int renames PlayChannel;
  pragma Inline (PlayChannel);

  function PlayMusic
   (Channel : C.int;
    Chunk   : Chunk_Access_t;
    Loops   : C.int)
    return  C.int;
  function Play_Music
   (Channel : C.int;
    Chunk   : Chunk_Access_t;
    Loops   : C.int)
    return  C.int renames PlayMusic;
  pragma Import (C, PlayMusic, "Mix_PlayMusic");

  -- Fade in music or a channel over "ms" milliseconds
  function FadeInMusic
   (Music : Music_Access_t;
    Loops : C.int;
    Ms    : C.int)
    return  C.int;
  function Fade_In_Music
   (Music : Music_Access_t;
    Loops : C.int;
    Ms    : C.int)
    return  C.int renames FadeInMusic;
  pragma Import (C, FadeInMusic, "Mix_FadeInMusic");

  function FadeInMusicPos
   (Music    : Music_Access_t;
    Loops    : C.int;
    Ms       : C.int;
    Position : C.double)
    return     C.int;
  function Fade_In_Music_Pos
   (Music    : Music_Access_t;
    Loops    : C.int;
    Ms       : C.int;
    Position : C.double)
    return     C.int renames FadeInMusicPos;
  pragma Import (C, FadeInMusicPos, "Mix_FadeInMusicPos");

  function FadeInChannel
   (Channel : C.int;
    Chunk   : Chunk_Access_t;
    Loops   : C.int;
    Ms      : C.int)
    return  C.int;
  function Fade_In_Channel
   (Channel : C.int;
    Chunk   : Chunk_Access_t;
    Loops   : C.int;
    Ms      : C.int)
    return  C.int renames FadeInChannel;
  pragma Inline (FadeInChannel);

  function FadeInChannelTimed
   (Channel : C.int;
    Chunk   : Chunk_Access_t;
    Loops   : C.int;
    Ms      : C.int;
    Ticks   : C.int)
    return  C.int;
  function Fade_In_Channel_Timed
   (Channel : C.int;
    Chunk   : Chunk_Access_t;
    Loops   : C.int;
    Ms      : C.int;
    Ticks   : C.int)
    return  C.int renames FadeInChannelTimed;
  pragma Import (C, FadeInChannelTimed, "Mix_FadeInChannelTimed");

  -- Set the volume in the range of 0-128 of a specific channel or chunk.
  subtype Volume_Type_t is C.int range 0 .. 128;

  function Volume (Channel : C.int; Volume : Volume_Type_t) return C.int;
  pragma Import (C, Volume, "Mix_Volume");

  function VolumeChunk (Chunk : Chunk_Access_t; Volume : Volume_Type_t) return C.int;
  function Volume_Chunk (Chunk : Chunk_Access_t; Volume : Volume_Type_t) return C.int renames VolumeChunk;
  pragma Import (C, VolumeChunk, "Mix_VolumeChunk");

  function VolumeMusic (Volume : Volume_Type_t) return C.int;
  function Volume_Music (Volume : Volume_Type_t) return C.int renames VolumeMusic;
  pragma Import (C, VolumeMusic, "Mix_VolumeMusic");

  -- Halt playing of a particular channel.
  function HaltChannel (Channel : C.int) return C.int;
  function HaltGroup (Tag : C.int) return C.int;
  function HaltMusic return  C.int;
  function Halt_Channel (Channel : C.int) return C.int renames HaltChannel;
  function Halt_Group (Tag : C.int) return C.int renames HaltGroup;
  function Halt_Music return  C.int renames HaltMusic;
  pragma Import (C, HaltChannel, "Mix_HaltChannel");
  pragma Import (C, HaltGroup, "Mix_HaltGroup");
  pragma Import (C, HaltMusic, "Mix_HaltMusic");

  -- Change the expiration delay for a particular channel.
  function ExpireChannel (Channel : C.int; Ticks : C.int) return C.int;
  function Expire_Channel (Channel : C.int; Ticks : C.int) return C.int renames ExpireChannel;
  pragma Import (C, ExpireChannel, "Mix_ExpireChannel");

  -- Halt a channel, fading it out progressively until it's silent.
  function FadeOutChannel (Which : C.int; Ms : C.int) return C.int;
  function FadeOutGroup (Tag : C.int; Ms : C.int) return C.int;
  function FadeOutMusic (Ms : C.int) return C.int;
  function Fade_Out_Channel (Which : C.int; Ms : C.int) return C.int renames FadeOutChannel;
  function Fade_Out_Group (Tag : C.int; Ms : C.int) return C.int renames FadeOutGroup;
  function Fade_Out_Music (Ms : C.int) return C.int renames FadeOutMusic;
  pragma Import (C, FadeOutChannel, "Mix_FadeOutChannel");
  pragma Import (C, FadeOutGroup, "Mix_FadeOutGroup");
  pragma Import (C, FadeOutMusic, "Mix_FadeOutMusic");

  -- Query the fading status of a channel
  function FadingMusic return Fading_Type_t;
  function Fading_Music return Fading_Type_t renames FadingMusic;
  function FadingChannel (Channel : C.int) return Fading_Type_t;
  function Fading_Channel (Channel : C.int) return Fading_Type_t renames FadingChannel;
  pragma Import (C, FadingMusic, "Mix_FadingMusic");
  pragma Import (C, FadingChannel, "Mix_FadingChannel");

  -- Pause/Resume a particular channel
  procedure Pause (Channel : C.int);
  procedure Resume (Channel : C.int);
  function Paused (Channel : C.int) return C.int;
  pragma Import (C, Pause, "Mix_Pause");
  pragma Import (C, Resume, "Mix_Resume");
  pragma Import (C, Paused, "Mix_Paused");

  -- Pause/Resume the music stream
  procedure PauseMusic;
  procedure ResumeMusic;
  procedure RewindMusic;
  function PausedMusic return  C.int;
  procedure Pause_Music renames PauseMusic;
  procedure Resume_Music renames ResumeMusic;
  procedure Rewind_Music renames RewindMusic;
  function Paused_Music return  C.int renames PausedMusic;
  pragma Import (C, PauseMusic, "Mix_PauseMusic");
  pragma Import (C, ResumeMusic, "Mix_ResumeMusic");
  pragma Import (C, RewindMusic, "Mix_RewindMusic");
  pragma Import (C, PausedMusic, "Mix_PausedMusic");

  -- Set the current position in the music stream.
  function SetMusicPosition (Position : C.double) return C.int;
  function Set_Music_Position (Position : C.double) return C.int renames SetMusicPosition;
  pragma Import (C, SetMusicPosition, "Mix_SetMusicPosition");

  -- Check the status of a specific channel.
  function SetPlaying (Channel : C.int) return C.int;
  function Set_Playing (Channel : C.int) return C.int renames SetPlaying;
  pragma Import (C, SetPlaying, "Mix_SetPlaying");

  function SetPlayingMusic return  C.int;
  function Set_Playing_Music return  C.int renames SetPlayingMusic;
  pragma Import (C, SetPlayingMusic, "Mix_SetPlayingMusic");

  -- Synchro value is set by MikMod from modules while playing.
  function SetSynchroValue (Channel : C.int) return C.int;
  function Set_Synchro_Value (Channel : C.int) return C.int renames SetSynchroValue;
  pragma Import (C, SetSynchroValue, "Mix_SetSynchroValue");

  function GetSynchroValue return  C.int;
  function Get_Synchro_Value return  C.int renames GetSynchroValue;
  pragma Import (C, GetSynchroValue, "Mix_GetSynchroValue");

  -- Get the Mix_Chunk currently associated with a mixer channel.
  function GetChunk (Channel : C.int) return Chunk_Access_t;
  function Get_Chunk (Channel : C.int) return Chunk_Access_t renames GetChunk;
  pragma Import (C, GetChunk, "Mix_GetChunk");

  -- Close the mixer, halting all playing audio.
  procedure CloseAudio;
  procedure Close_Audio renames CloseAudio;
  pragma Import (C, CloseAudio, "Mix_CloseAudio");

  function Geterror return  Cs.chars_ptr renames SDL.Error.Geterror;
  function Geterror return String renames SDL.Error.Geterror;
  function Get_Error return  Cs.chars_ptr renames SDL.Error.Get_Error;
  function Get_Error return String renames SDL.Error.Get_Error;

end SDL.Mixer;
