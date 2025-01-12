#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void expand_file(const char *input_file, const char *output_file, long new_size) {
  FILE *input = fopen(input_file, "rb");
  if (!input) {
    perror("Error opening input file");
    exit(EXIT_FAILURE);
  }

  fseek(input, 0, SEEK_END);
  long input_size = ftell(input);
  rewind(input);

  if (input_size > new_size) {
    fprintf(stderr, "Error: new_size must be greater than or equal to the input file size.\n");
    fclose(input);
    exit(EXIT_FAILURE);
  }

  char *file_content = (char *)malloc(input_size);
  if (!file_content) {
    perror("Error allocating memory");
    fclose(input);
    exit(EXIT_FAILURE);
  }

  fread(file_content, 1, input_size, input);
  fclose(input);

  FILE *output = fopen(output_file, "wb");
  if (!output) {
    perror("Error opening output file");
    free(file_content);
    exit(EXIT_FAILURE);
  }

  fwrite(file_content, 1, input_size, output);
  free(file_content);

  long bytes_to_append = new_size - input_size;
  if (bytes_to_append > 0) {
    char *empty_bytes = (char *)calloc(bytes_to_append, 1);
    if (!empty_bytes) {
      perror("Error allocating memory for empty bytes");
      fclose(output);
      exit(EXIT_FAILURE);
    }
    fwrite(empty_bytes, 1, bytes_to_append, output);
    free(empty_bytes);
  }

  printf("File expanded successfully: %s\n", output_file);
  fclose(output);
}

int main(int argc, char *argv[]) {
  if (argc != 4) {
    fprintf(stderr, "Usage: %s <input_file> <output_file> <new_size>\n", argv[0]);
    return EXIT_FAILURE;
  }

  const char *input_file = argv[1];
  const char *output_file = argv[2];
  long new_size = strtol(argv[3], NULL, 10);

  if (new_size <= 0) {
    fprintf(stderr, "Error: new_size must be a positive integer.\n");
    return EXIT_FAILURE;
  }

  expand_file(input_file, output_file, new_size);

  return EXIT_SUCCESS;
}
