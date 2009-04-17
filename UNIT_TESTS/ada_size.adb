-- auto generated, do not edit

with Ada.Text_IO;
with Ada.Command_Line;

with SDL;
with SDL.Mixer;

procedure Ada_Size is
  package IO renames Ada.Text_IO;
  package Command_Line renames Ada.Command_Line;

  -- generic types
  -- type generic_t is new Integer;
  -- type generic_access_t is access all generic_t;

  -- package instantiations

  -- type names
  SDL_Mixer_Chunk_t : aliased String := "SDL.Mixer.Chunk_t";
  SDL_Mixer_Effect_Done_Func_t : aliased String := "SDL.Mixer.Effect_Done_Func_t";
  SDL_Mixer_Effect_Func_t : aliased String := "SDL.Mixer.Effect_Func_t";
  SDL_Mixer_Fading_Type_t : aliased String := "SDL.Mixer.Fading_Type_t";
  SDL_Mixer_Music_Access_t : aliased String := "SDL.Mixer.Music_Access_t";
  SDL_Mixer_Music_t : aliased String := "SDL.Mixer.Music_t";
  SDL_Mixer_Music_Type_t : aliased String := "SDL.Mixer.Music_Type_t";

  type Type_t is record
    Name : access String;
    Size : Natural;
  end record;
  type Type_Lookup_t is array (Natural range <>) of Type_t;

  Types : aliased constant Type_Lookup_t := (
    (SDL_Mixer_Chunk_t'Access, SDL.Mixer.Chunk_t'Size),
    (SDL_Mixer_Effect_Done_Func_t'Access, SDL.Mixer.Effect_Done_Func_t'Size),
    (SDL_Mixer_Effect_Func_t'Access, SDL.Mixer.Effect_Func_t'Size),
    (SDL_Mixer_Fading_Type_t'Access, SDL.Mixer.Fading_Type_t'Size),
    (SDL_Mixer_Music_Access_t'Access, SDL.Mixer.Music_Access_t'Size),
    (SDL_Mixer_Music_t'Access, SDL.Mixer.Music_t'Size),
    (SDL_Mixer_Music_Type_t'Access, SDL.Mixer.Music_Type_t'Size)
  );

  procedure Find (Name : String) is
  begin
    for Index in Types'Range loop
      if Types (Index).Name.all = Name then
        IO.Put_Line (Natural'Image (Types (Index).Size));
        return;
      end if;
    end loop;
    raise Program_Error with "fatal: unknown ada type";
  end Find;

begin
  if Command_Line.Argument_Count /= 1 then
    raise Program_Error with "fatal: incorrect number of args";
  end if;
  Find (Command_Line.Argument (1));
end Ada_Size;
