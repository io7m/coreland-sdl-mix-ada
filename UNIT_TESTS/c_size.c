#include <SDL.h>
#include <SDL_mixer.h>

#include <stdio.h>
#include <string.h>

struct {
  const char *type_name;
  unsigned int type_size;
} types[] = {
  /* auto generated - do not edit */
  { "Mix_Chunk", sizeof (Mix_Chunk) },
  { "struct Mix_Chunk", sizeof (struct Mix_Chunk) },
  { "Mix_EffectDone_t", sizeof (Mix_EffectDone_t) },
  { "Mix_EffectFunc_t", sizeof (Mix_EffectFunc_t) },
  { "Mix_Fading", sizeof (Mix_Fading) },
  { "Mix_Music *", sizeof (Mix_Music *) },
  { "Mix_Music *", sizeof (Mix_Music *) },
  { "Mix_MusicType", sizeof (Mix_MusicType) },
};
const unsigned int types_size = sizeof (types) / sizeof (types[0]);

void
find (const char *name)
{
  unsigned int pos;
  for (pos = 0; pos < types_size; ++pos) {
    if (strcmp (types[pos].type_name, name) == 0) {
      printf ("%u\n", types[pos].type_size * 8);
      return;
    }
  }
  fprintf (stderr, "fatal: unknown C type\n");
  exit (112);
}

int
main (int argc, char *argv[])
{
  if (argc != 2) exit (111);
  find (argv[1]);
  return 0;
}
