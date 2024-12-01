emulgui_x86_64: emulgui.c
	x86_64-w64-mingw32-gcc emulgui.c -o emulgui_x86_64.exe -lgdi32

emulgui_i686: emulgui.c
	i686-w64-mingw32-gcc emulgui.c -o emulgui_i686.exe -lgdi32

clean:
	@rm -f emulgui*.exe

all: emulgui_x86_64 emulgui_i686
