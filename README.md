# BFT - Brainfuck Toolset

### To "decompile" and run C version
```bash
make
./bft -c -o mandelbrot.c ./b/mandelbrot.b
gcc -O3 -o mandelbrot mandelbrot.c
./mandelbrot
```

### To transpile and run ASM version
```bash
make
./bft -s -o mandelbrot.s ./b/mandelbrot.b
nasm -f elf64 -o mandelbrot.o ./mandelbrot.s
ld -o mandelbrot ./mandelbrot.o
./mandelbrot
```

### To run interpreter
```bash
make
./bft ./b/mandelbrot.b
```