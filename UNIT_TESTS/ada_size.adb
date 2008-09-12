with ada.text_io;
with ada.command_line;

with sdl;
with sdl.mixer;

procedure ada_size is
  package io renames ada.text_io;
  package cmdline renames ada.command_line;

  -- auto generated - do not edit
  sdl_mixer_chunk_t : aliased string := "sdl.mixer.chunk_t";
  sdl_mixer_effect_done_func_t : aliased string := "sdl.mixer.effect_done_func_t";
  sdl_mixer_effect_func_t : aliased string := "sdl.mixer.effect_func_t";
  sdl_mixer_fading_type_t : aliased string := "sdl.mixer.fading_type_t";
  sdl_mixer_music_access_t : aliased string := "sdl.mixer.music_access_t";
  sdl_mixer_music_t : aliased string := "sdl.mixer.music_t";
  sdl_mixer_music_type_t : aliased string := "sdl.mixer.music_type_t";

  type type_t is record
    name : access string;
    size : natural;
  end record;
  type type_lookup_t is array (natural range <>) of type_t;

  types : aliased constant type_lookup_t := (
    (sdl_mixer_chunk_t'access, sdl.mixer.chunk_t'size),
    (sdl_mixer_effect_done_func_t'access, sdl.mixer.effect_done_func_t'size),
    (sdl_mixer_effect_func_t'access, sdl.mixer.effect_func_t'size),
    (sdl_mixer_fading_type_t'access, sdl.mixer.fading_type_t'size),
    (sdl_mixer_music_access_t'access, sdl.mixer.music_access_t'size),
    (sdl_mixer_music_t'access, sdl.mixer.music_t'size),
    (sdl_mixer_music_type_t'access, sdl.mixer.music_type_t'size)
  );

  procedure find (name : string) is
  begin
    for index in types'range loop
      if types (index).name.all = name then
        io.put_line (natural'image (types (index).size));
        return;
      end if;
    end loop;
    raise program_error with "fatal: unknown ada type";
  end find;

begin
  if cmdline.argument_count /= 1 then
    raise program_error with "fatal: incorrect number of args";
  end if;
  find (cmdline.argument (1));
end ada_size;
