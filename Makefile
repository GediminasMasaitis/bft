CC = gcc
CFLAGS = -std=c11 -O3 -I./include
LDFLAGS =

SRCDIR = src
SOURCES = $(wildcard $(SRCDIR)/*.c)
OBJECTS = $(SOURCES:.c=.o)

TARGET = bft

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(OBJECTS) -o $@ $(LDFLAGS)

$(SRCDIR)/%.o: $(SRCDIR)/%.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(SRCDIR)/*.o $(TARGET)
	rm -f bft

format:
	dos2unix $(SRCDIR)/*.c include/*.h Makefile
	clang-format -i $(SRCDIR)/*.c include/*.h
