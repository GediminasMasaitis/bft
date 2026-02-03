CC = gcc
CFLAGS = -std=c11 -O3
LDFLAGS =
SOURCES = codegen_c.c codegen_nasm.c machine.c main.c optimizer.c
OBJECTS = $(SOURCES:.c=.o)
TARGET = bft
TEST_SOURCES = codegen_c.c codegen_nasm.c machine.c optimizer.c test.c
TEST_OBJECTS = $(TEST_SOURCES:.c=.o)
TEST_TARGET = test

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(OBJECTS) -o $@ $(LDFLAGS)

$(TEST_TARGET): $(TEST_OBJECTS) $(TARGET)
	$(CC) $(TEST_OBJECTS) -o $@ $(LDFLAGS)
	./$(TEST_TARGET)

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

check: $(TARGET) $(TEST_TARGET)
	./$(TEST_TARGET)

clean:
	rm -f *.o
	rm -f *.s
	rm -f ./$(TARGET)
	rm -f ./$(TEST_TARGET)

format:
	dos2unix *.c *.h Makefile
	clang-format -i *.c *.h

.PHONY: all check clean format
