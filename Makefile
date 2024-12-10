emulgui: emulgui.c
	x86_64-w64-mingw32-gcc -ggdb emulgui.c -o emulgui.exe -lgdi32

clean:
	@rm -f emulgui*.exe

all: clean emulgui
