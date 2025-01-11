emulgui_x86_64: emulgui.c
	x86_64-w64-mingw32-gcc -ggdb emulgui.c -o emulgui_x86_64.exe -lgdi32

emulgui_i686: emulgui.c
	i686-w64-mingw32-gcc -ggdb emulgui.c -o emulgui_i686.exe -lgdi32

forth:
	wine asem.exe forth.asm forth.mem

clean:
	@rm -f emulgui*.exe
	@rm -f forth.mem

all: emulgui_x86_64 emulgui_i686 forth
