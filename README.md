# BFT - Brainfuck Toolset

Optimizing Brainfuck interpreter with C and x86-64 assembly backends.

## Build

```bash
make
```

## Usage

```
bft [options] <file.b>

Options:
  -r, --run         Run the program (default)
  -c, --emit-c      Generate C code
  -s, --emit-asm    Generate NASM assembly
  -o, --output FILE Write output to FILE
  -d, --dump        Dump optimized instructions
  -h, --help        Show help
```

## Examples

Run directly:
```bash
./bft ./programs/mandelbrot.b
```

Compile to C:
```bash
./bft -c -o mandelbrot.c ./programs/mandelbrot.b
gcc -O3 -o mandelbrot mandelbrot.c
./mandelbrot
```

Compile to assembly:
```bash
./bft -s -o mandelbrot.s ./programs/mandelbrot.b
nasm -f elf64 -o mandelbrot.o mandelbrot.s
ld -o mandelbrot mandelbrot.o
./mandelbrot
```
