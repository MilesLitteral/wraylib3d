#include <unistd.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  char **my_argv = malloc(sizeof(char *) * (6 + argc));
  my_argv[0] = "deno";
  my_argv[1] = "run";
  my_argv[2] = "--allow-read";
  my_argv[3] = "--allow-write";
  my_argv[4] = "--v8-flags=--liftoff-only,--wasm-"
               "lazy-compilation,--wasm-lazy-validation";
  my_argv[5] = WASM_RUN;
  for (int i = 1; i <= argc; ++i) {
    my_argv[5 + i] = argv[i];
  }
  execvp(my_argv[0], my_argv);
}
