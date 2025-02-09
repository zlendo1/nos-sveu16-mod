#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <windows.h>

#define freq 8000000 // Simulate 8 MHz machine
#define ROM_START 0x0
#define RAM_START 0x0100
#define VRAM_START 0x2000
#define VRAM_END 0x24B0 // VRAM + (VRAM_WIDTH + VRAM_HEIGHT)
#define VRAM_WIDTH 80
#define VRAM_HEIGHT 25
#define IO_START 0xFFF0
#define KEYBOARD_PORT 0xFFFF
#define DISK_COMMAND_PORT 0xFFFE
#define DISK_SECTOR_PORT 0xFFFD
#define DISK_DATA_PORT 0xFFFC
#define ROM_SIZE 256         // Coincidentally boot.img size in words
#define DISK_SECTOR_SIZE 256 // 128 words
#define INTERRUPT_INTERVAL_MS 20
#define MEM_SIZE 65536
#define DISK_PATH "disk.img"
#define SCREEN_WIDTH 900
#define SCREEN_HEIGHT 500

unsigned short memory[MEM_SIZE]; // Array of 16 bit words simulates 128K memory

struct instrukcije {
  void *execaddr;
  unsigned char dest;
  unsigned char src1;
  unsigned char src2;
} instrmem[MEM_SIZE], *current, *accessed;
bool regime_filling = true;

unsigned short regs[16]; // Array of 16 bit words simulates 16 registers
char asciikeyboard;      // Variable that keeps ASCII code of the last pressed key
int videochanged = 1; // Indicates that video memory was changed and the bitmap needs to be redrawn
long cyclecount;      // Counters of cycles per emulation frame

struct BitMAPINFO // Structure from Windows API
{
  BITMAPINFOHEADER bmiHeader;
  RGBQUAD bmiColors[256];
};
struct BitMAPINFO bi; // Define Bitmap

BYTE *pBits; // Pointer to bitmap memory
HDC memDC;   // Memory device context
HBITMAP hBM;
HINSTANCE hinst;
HWND hwndMain;
HDC hdc;

unsigned short disk_command;
unsigned short disk_sector;
FILE *file_handler = NULL;

// CPU initialization
void reset() {
  regs[15] = 0; // Just set program counter to zero
}

void draw_line(int pos, unsigned char x_from, unsigned char x_to, unsigned char y_from,
               unsigned char y_to) {
  for (int i = x_from; i < x_to; i++) {
    for (int j = y_from; j < x_to; j++) {
      *(pBits + pos + SCREEN_WIDTH * i + j) = 1;
    }
  }
}

void draw_diagonal_down(int pos, unsigned char from_y, unsigned char to_y) {
  for (int i = from_y; i < to_y; i++) {
    *(pBits + pos + SCREEN_WIDTH * i + i) = 1;
    *(pBits + pos + SCREEN_WIDTH * i + i + 1) = 1;
  }
}

void draw_diagonal_up(int pos, unsigned char from_y, unsigned char to_y) {
  for (int i = to_y - 1; i <= from_y; i--) {
    *(pBits + pos + SCREEN_WIDTH * i + i) = 1;
    *(pBits + pos + SCREEN_WIDTH * i + i + 1) = 1;
  }
}

// Main emulation loop analyse and execute cyclecount instructions
void mloop() {
  unsigned short ir, n;
  unsigned int temp1;
  signed int temp2;

  static void *labels[] = {&&dolod, &&doadd, &&dosub, &&doand, &&doora, &&doxor, &&doshr, &&domul,
                           &&dosto, &&domif, &&dogtu, &&dogts, &&doltu, &&dolts, &&doequ, &&domaj};

  if (regime_filling) {
    regime_filling = false;

    for (unsigned long i = 0; i < MEM_SIZE; i++) { // Fill up instrmem
      unsigned short ir;

      ir = memory[i];
      accessed = instrmem + i;
      accessed->execaddr = labels[(ir & 0xF000) >> 12]; // 4 bits for opcode
      accessed->dest = (ir & 0x0F00) >> 8;              // 4 bits for destination
      accessed->src1 = (ir & 0x00F0) >> 4;              // 4 bits for src1
      accessed->src2 = (ir & 0x000F);                   // 4 bits for src2
    }

    return;
  }

  current = instrmem + regs[15];
  goto *(current->execaddr);

dolod:
  if (cyclecount-- == 0)
    return;
  regs[15]++;

  if (regs[current->src2] < IO_START) { // For memory area belonging to ROM or RAM
    regs[current->dest] =
        memory[regs[current->src2]]; // Put to destination register 16 bit value pointed by SRC1
  } else if (regs[current->src2] ==
             KEYBOARD_PORT) {            // If memory belongs to I/O map, for each device
    regs[current->dest] = asciikeyboard; // Do special handling
    asciikeyboard = 0;                   // This keyboard controller returns ASCII code of the key!
  } else if (regs[current->src2] == DISK_DATA_PORT) {
    if (file_handler == NULL) {
      MessageBox(NULL, "File handler not open!", "Error!", MB_ICONEXCLAMATION | MB_OK);
    } else if (fseek(file_handler, sizeof(unsigned short), SEEK_CUR) != 0) {
      MessageBox(NULL, "Error seeking in disk!", "Error!", MB_ICONEXCLAMATION | MB_OK);
    } else if (fread(regs + current->dest, sizeof(unsigned short), 1, file_handler) != 1) {
      MessageBox(NULL, "Error reading from disk!", "Error!", MB_ICONEXCLAMATION | MB_OK);
    }
  }

  if (current->src2 == 15 &&
      current->dest != 15) // If addressed by program counter skip the extra word
    regs[15]++;

  current = instrmem + regs[15];
  goto *(current->execaddr);
doadd:
  if (cyclecount-- == 0)
    return;
  regs[15]++;

  regs[current->dest] = regs[current->src1] + regs[current->src2]; // Perform addition

  current = instrmem + regs[15];
  goto *(current->execaddr);
dosub:
  if (cyclecount-- == 0)
    return;
  regs[15]++;

  regs[current->dest] = regs[current->src1] - regs[current->src2]; // Perform subtraction

  current = instrmem + regs[15];
  goto *(current->execaddr);
doand:
  if (cyclecount-- == 0)
    return;
  regs[15]++;

  regs[current->dest] = regs[current->src1] & regs[current->src2]; // Perform logical AND

  current = instrmem + regs[15];
  goto *(current->execaddr);
doora:
  if (cyclecount-- == 0)
    return;
  regs[15]++;

  regs[current->dest] = regs[current->src1] | regs[current->src2]; // Perform logical OR

  current = instrmem + regs[15];
  goto *(current->execaddr);
doxor:
  if (cyclecount-- == 0)
    return;
  regs[15]++;

  regs[current->dest] = regs[current->src1] ^ regs[current->src2]; // Perform logical XOR

  current = instrmem + regs[15];
  goto *(current->execaddr);
doshr:
  if (cyclecount-- == 0)
    return;
  regs[15]++;

  n = regs[current->src2] & 0xF;               // Extract shift count from bits at position 0-3
  switch ((regs[current->src2] & 0x0030) >> 4) // Extract shift kind from bits at position 4-5
  {

  case 0:
    temp2 = regs[current->src1];        // Arithmetic shift right preserve highest (sign) bit
    if (regs[current->src1] >= 0x8000)  // For negative number
      temp2 |= 0xFFFF0000;              // Set internal 32 bit number to all high 16 bits = 1
    regs[current->dest] = temp2 >> (n); // Then shift n times right
    break;
  case 1:
    temp1 = regs[current->src1];        // Logical shift right pushes with zero
    regs[current->dest] = temp1 >> (n); // Then shift n times right
    break;
  case 2:
    regs[current->dest] = regs[current->src1] << n; // Shift left
    break;
  case 3:
    regs[current->dest] =
        (regs[current->src1] << n) |
        (regs[current->src1] >>
         (16 - n)); // Rotate right is clumsy in C, combination of left and right shift
    break;
  }

  current = instrmem + regs[15];
  goto *(current->execaddr);
domul:
  if (cyclecount-- == 0)
    return;
  regs[15]++;

  regs[current->dest] =
      regs[current->src1] * regs[current->src2]; // put lower 16 bits of the result into destination

  current = instrmem + regs[15];
  goto *(current->execaddr);
dosto:
  if (cyclecount-- == 0)
    return;
  regs[15]++;

  if (regs[current->src2] >= RAM_START &&
      regs[current->src2] < IO_START) { // Area of memory that belongs to RAM
    regs[current->dest] =               // Store value both to destination register
        memory[regs[current->src2]] =   // and memory pointed by SRC2 register
        regs[current->src1];

    ir = memory[regs[current->src2]];
    accessed = instrmem + regs[current->src2];
    accessed->execaddr = labels[(ir & 0xF000) >> 12]; // 4 bits for opcode
    accessed->dest = (ir & 0x0F00) >> 8;              // 4 bits for destination
    accessed->src1 = (ir & 0x00F0) >> 4;              // 4 bits for src1
    accessed->src2 = (ir & 0x000F);                   // 4 bits for src2

    if (regs[current->src2] >= VRAM_START && // Special case for memory belonging to video
        regs[current->src2] < VRAM_END) {
      int pos = (regs[current->src2] - VRAM_START) *
                12; // 12 pixels per word width, convert to relative position in bitmap
      unsigned short val = regs[current->src1];

      /*// Now for each bit in SRC1 register change corresponding byte in bitmap*/
      /**(pBits + pos) = (val & 0x8000) ? 1 : 0;*/
      /**(pBits + pos + 1) = (val & 0x4000) ? 1 : 0;*/
      /**(pBits + pos + 2) = (val & 0x2000) ? 1 : 0;*/
      /**(pBits + pos + 3) = (val & 0x1000) ? 1 : 0;*/
      /**(pBits + pos + 4) = (val & 0x0800) ? 1 : 0;*/
      /**(pBits + pos + 5) = (val & 0x0400) ? 1 : 0;*/
      /**(pBits + pos + 6) = (val & 0x0200) ? 1 : 0;*/
      /**(pBits + pos + 7) = (val & 0x0100) ? 1 : 0;*/
      /**(pBits + pos + 8) = (val & 0x0080) ? 1 : 0;*/
      /**(pBits + pos + 9) = (val & 0x0040) ? 1 : 0;*/
      /**(pBits + pos + 10) = (val & 0x0020) ? 1 : 0;*/
      /**(pBits + pos + 11) = (val & 0x0010) ? 1 : 0;*/
      /**(pBits + pos + 12) = (val & 0x0008) ? 1 : 0;*/
      /**(pBits + pos + 13) = (val & 0x0004) ? 1 : 0;*/
      /**(pBits + pos + 14) = (val & 0x0002) ? 1 : 0;*/
      /**(pBits + pos + 15) = (val & 0x0001) ? 1 : 0;*/

      if (val & 0x0001)
        draw_line(pos, 0, 1, 1, 5);
      if (val & 0x0002)
        draw_line(pos, 0, 1, 7, 12);
      if (val & 0x0004)
        draw_line(pos, 1, 9, 11, 12);
      if (val & 0x0008)
        draw_line(pos, 11, 19, 11, 12);
      if (val & 0x0010)
        draw_line(pos, 19, 20, 7, 11);
      if (val & 0x0020)
        draw_line(pos, 19, 20, 1, 5);
      if (val & 0x0040)
        draw_line(pos, 11, 18, 0, 1);
      if (val & 0x0080)
        draw_line(pos, 1, 9, 0, 1);
      if (val & 0x0100)
        draw_diagonal_down(pos, 1, 5);
      if (val & 0x0200)
        draw_line(pos, 1, 9, 5, 7);
      if (val & 0x0400)
        draw_diagonal_up(pos, 7, 11);
      if (val & 0x0800)
        draw_line(pos, 9, 11, 7, 11);
      if (val & 0x1000)
        draw_diagonal_down(pos, 7, 11);
      if (val & 0x2000)
        draw_line(pos, 11, 19, 5, 7);
      if (val & 0x4000)
        draw_diagonal_up(pos, 1, 5);
      if (val & 0x8000)
        draw_line(pos, 9, 11, 1, 5);

      videochanged = 1; // Video was changed
    }
  } else if (regs[current->src2] == DISK_COMMAND_PORT) {
    regs[current->dest] = disk_command = regs[current->src1];

    if (file_handler != NULL) {
      fclose(file_handler);
    }

    if (disk_command == 0) {
      disk_sector = 0;
    } else if (disk_command == 1) {
      file_handler = fopen(DISK_PATH, "rb");

      if (file_handler == NULL) {
        MessageBox(NULL, "Error opening disk handle!", "Error!", MB_ICONEXCLAMATION | MB_OK);
      }

      fseek(file_handler, disk_sector * DISK_SECTOR_SIZE, SEEK_SET);
    } else if (disk_command == 2) {
      file_handler = fopen(DISK_PATH, "rb+");

      if (file_handler == NULL) {
        MessageBox(NULL, "Error opening disk handle!", "Error!", MB_ICONEXCLAMATION | MB_OK);
      }

      fseek(file_handler, disk_sector * DISK_SECTOR_SIZE, SEEK_SET);
    }
  } else if (regs[current->src2] == DISK_SECTOR_PORT) {
    regs[current->dest] = disk_sector = regs[current->src1];
  } else if (regs[current->src2] == DISK_DATA_PORT) {
    if (file_handler == NULL) {
      MessageBox(NULL, "File handler not open", "Error!", MB_ICONEXCLAMATION | MB_OK);
    } else if (fseek(file_handler, sizeof(unsigned short), SEEK_CUR) != 0) {
      MessageBox(NULL, "Error seeking in disk!", "Error!", MB_ICONEXCLAMATION | MB_OK);
    } else if (fwrite(regs + current->src1, sizeof(unsigned short), 1, file_handler) != 1) {
      MessageBox(NULL, "Error reading from disk!", "Error!", MB_ICONEXCLAMATION | MB_OK);
    } else {
      regs[current->dest] = regs[current->src1];
    }
  } else { // Area of memory that belongs to ROM or unidirectional I/O device
    regs[current->dest] = regs[current->src1]; // Do not update memory, just destination register
  }
  if (current->src2 == 15) // If addressed by program counter skip the extra word
    regs[15]++;

  current = instrmem + regs[15];
  goto *(current->execaddr);
domif:
  if (cyclecount-- == 0)
    return;
  regs[15]++;

  regs[current->dest] = regs[current->src1] ? regs[current->src2] : regs[current->dest];

  current = instrmem + regs[15];
  goto *(current->execaddr);
dogtu:
  if (cyclecount-- == 0)
    return;
  regs[15]++;

  regs[current->dest] = regs[current->src1] > regs[current->src2] ? 1 : 0;

  current = instrmem + regs[15];
  goto *(current->execaddr);
dogts:
  if (cyclecount-- == 0)
    return;
  regs[15]++;

  regs[current->dest] =
      (signed short)regs[current->src1] > (signed short)regs[current->src2] ? 1 : 0;

  current = instrmem + regs[15];
  goto *(current->execaddr);
doltu:
  if (cyclecount-- == 0)
    return;
  regs[15]++;

  regs[current->dest] = regs[current->src1] < regs[current->src2] ? 1 : 0;

  current = instrmem + regs[15];
  goto *(current->execaddr);
dolts:
  if (cyclecount-- == 0)
    return;
  regs[15]++;

  regs[current->dest] =
      (signed short)regs[current->src1] < (signed short)regs[current->src2] ? 1 : 0;

  current = instrmem + regs[15];
  goto *(current->execaddr);
doequ:
  if (cyclecount-- == 0)
    return;
  regs[15]++;

  regs[current->dest] = regs[current->src1] == regs[current->src2] ? 1 : 0;

  current = instrmem + regs[15];
  goto *(current->execaddr);
domaj:
  if (cyclecount-- == 0)
    return;
  regs[15]++;

  regs[current->dest] = regs[current->src1]; // Put SRCq to dest
  regs[15] = regs[current->src2];            // Put SRC2 to program counter

  current = instrmem + regs[15];
  goto *(current->execaddr);
}

void bootstrap(const char *boot_img) {
  FILE *prom = fopen(boot_img, "rb");

  if (prom == NULL) {
    MessageBox(NULL, "Memory load fail!", "Error!", MB_ICONEXCLAMATION | MB_OK);
    exit(EXIT_FAILURE);
  }

  fread(memory + ROM_START, sizeof(unsigned short), ROM_SIZE, prom); // Load ROM

  fclose(prom);
}

// Now we assume that MemoryDC is created and bitmap selected
void DisplayDIB(HDC hdc) {
  BitBlt(hdc, 10, 10, 640,
         480, // Now copy bitmap to window inside client area, 10 pixels from corner
         memDC, 0, 0, SRCCOPY); // From memory DC, whole bitmap, direct copy
}

DWORD WINAPI EmulateCPU(void *arg) { // Thread function executed together with Windows main loop
#define LOWFREQ
  clock_t t1, t2, t3, delayadjust;
  int loopcount;
  double delay;
  loopcount = freq / 10; // 10 frames per second
  delay =
      loopcount * (double)CLOCKS_PER_SEC / freq; // Required time to additional wait floating point
  delayadjust = delay;                           // convert to integer

  while (TRUE) // Thread loop
  {
    t1 = clock();          // Get current time in clocks
    t2 = t1 + delayadjust; // Add waiting time
    cyclecount = loopcount;
    mloop();            // Execute cyclecount cycles i.e. instructions
    if (videochanged) { // If video memory was updated during the loop
      DisplayDIB(hdc);  // Show it
      videochanged = 0; // prevent future refreshes if video was not changed
    }
#ifdef LOWFREQ
    t3 = clock();                               // Get clock
    if (t3 < t2)                                // If time not elapsed
      Sleep((t2 - t3) * CLOCKS_PER_SEC / 1000); // Suspend, but Sleep uses millisecond as unit
#else
    while (t2 < clock()) {
    }; // Busy wait variant
#endif
  }
}

/* Called on exit */

void DeallocateGDI() {
  ReleaseDC(hwndMain, hdc); // The ReleaseDC function releases a device context (DC), freeing it for
                            // use by other applications.
  DeleteDC(memDC);          // The DeleteDC function deletes the specified device context (DC)
  free(pBits);              // Deallocate bitmap bits
  DeleteObject(hBM);        // The DeleteObject function deletes a logical pen, brush, font, bitmap,
                     // region, or palette, freeing all system resources associated with the object.
}

void CreateDIB() {
  /* The CreateDIBSection function creates a device indenpendant bitmap that applications can write
    to directly. The function gives you a pointer to the location of the bitmap bit values. You can
    supply a handle to a file-mapping object that the function will use to create the bitmap, or you
    can let the system allocate the memory for the bitmap.
*/
  hdc = GetDC(hwndMain);
  int i;
  bi.bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
  bi.bmiHeader.biWidth = SCREEN_WIDTH;    // 900 pixels width
  bi.bmiHeader.biHeight = -SCREEN_HEIGHT; // 500 pixels height negative number == top down DIB
  bi.bmiHeader.biPlanes = 1;              // Number of planes must be set to 1
  bi.bmiHeader.biBitCount = 8;            // 8 bits per pixel, easiest way to access each pixel
  bi.bmiHeader.biCompression = BI_RGB;    // No compression
  bi.bmiHeader.biClrUsed = 2;             // Just two colors 0 background, 1 foreground
  bi.bmiColors[0].rgbRed = 0xE0;          // Defines cream yellow background
  bi.bmiColors[0].rgbGreen = 0xDE;
  bi.bmiColors[0].rgbBlue = 0xBC;
  bi.bmiColors[0].rgbReserved = 0;
  bi.bmiColors[1].rgbRed = 0; // Defines black foreground
  bi.bmiColors[1].rgbGreen = 0;
  bi.bmiColors[1].rgbBlue = 0;
  bi.bmiColors[1].rgbReserved = 0;

  hBM = (HBITMAP)CreateDIBSection(hdc,               // Device context of the window
                                  (BITMAPINFO *)&bi, // Pointer to BITMAPINFO, defines structure
                                  DIB_RGB_COLORS,    // usage: palette is defined using RGB colors
                                  (VOID **)&pBits,   // Pointer to binary values of the pixels
                                  NULL,              // Do not use file mapping for DIB
                                  0);                // Offset in DIB not important

  memDC = CreateCompatibleDC(
      hdc); // creates a memory device context (memDC) that is compatible with the
  // device context (hdc) of the specified window (hwnd).
  // A device context is a data structure that defines the attributes of
  // drawing and painting operations. This memory device context is
  // typically used to manipulate a bitmap before displaying it.
  SelectObject(memDC, hBM); // Selects a bitmap (hBM) into the memory device context (memDC).
  // This means that all drawing operations performed on memDC will be
  // applied to the selected bitmap.
}

/* This is where all the input to the window goes to */
LRESULT CALLBACK WndProc(HWND hwnd, UINT Message, WPARAM wParam, LPARAM lParam) {
  PAINTSTRUCT ps;
  HDC hdc;

  switch (Message) {
  case WM_DESTROY: {    // Upon destruction, tell the main thread to stop
    PostQuitMessage(0); // Main Window loop will end
    break;
  }
  case WM_KEYDOWN: {                       // Key pressed
    asciikeyboard = (lParam >> 16) & 0xFF; // Get ASCII code of key
    break;
  }
  case WM_PAINT: {               // Main window overlapped or changed size
    hdc = BeginPaint(hwnd, &ps); // Refresh is done in thread anyway
    videochanged = 1;            // So just indicate to do it
    EndPaint(hwnd, &ps);         // End updating
    break;
  }
  /* All other messages (a lot of them) are processed using default procedures */
  default:
    return DefWindowProc(hwnd, Message, wParam, lParam);
  }
  return 0;
}

/* The 'main' function of Win32 GUI programs: this is where execution starts */

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow) {
  WNDCLASSEX wc; // Window class
  MSG msg;       // A temporary location for all messages

  /* Initialize and register the window class structure */
  memset(&wc, 0, sizeof(wc));     // Fill all bytes of the window class structure to zero
  wc.cbSize = sizeof(WNDCLASSEX); // First field must be size of the structure
  wc.lpfnWndProc = WndProc;       // This is where we will send messages to
  wc.hInstance = hInstance;       // Handle to process instance
  wc.hCursor = LoadCursor(NULL, IDC_ARROW);
  wc.hbrBackground =
      (HBRUSH)(COLOR_WINDOW +
               1); // Set the background color of the window to a system color (COLOR_WINDOW)
  wc.lpszClassName = "WindowClass";             // Set the name of the window class
  wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);   // Load a standard icon for the window
  wc.hIconSm = LoadIcon(NULL, IDI_APPLICATION); // Set a small icon (if available) for the window

  if (!RegisterClassEx(&wc)) // Register the window class with Windows
  {
    MessageBox(NULL, "Window Registration Failed!", "Error!",
               MB_ICONEXCLAMATION | MB_OK); // Display an error message if registration fails
    return 0;                               // Return 0 to indicate an error
  }
  hwndMain = CreateWindowEx( // Create the main application window
      WS_EX_CLIENTEDGE, "WindowClass", "SVEU16 emulator", WS_VISIBLE | WS_OVERLAPPEDWINDOW, 0, 0,
      680, 540, NULL, NULL, hInstance, NULL);

  if (hwndMain == NULL) {
    MessageBox(NULL, "Window Creation Failed!", "Error!",
               MB_ICONEXCLAMATION | MB_OK); // Display an error message if window creation fails
    return 0;                               // Return 0 to indicate an error
  }

  RECT rc;
  GetWindowRect(hwndMain, &rc); // Rect contains rc current window rectangle in screen coordinates
  int xPos = (GetSystemMetrics(SM_CXSCREEN) - rc.right) /
             2; // Half between screen width and right window edge
  int yPos = (GetSystemMetrics(SM_CYSCREEN) - rc.bottom) /
             2; // Half between screen heighg and bottom window edge
  SetWindowPos(hwndMain, 0, xPos, yPos, 0, 0,
               SWP_NOZORDER | SWP_NOSIZE); // Center the window on the screen
  UpdateWindow(hwndMain);                  // Update and display the window

  CreateDIB(); // Create the DIB (Device-Independent Bitmap) for display simulated video memory

  bootstrap("boot.img");

  /*FILE *prom = fopen("forth.mem", "r"); // Open a file with simulated RAM content*/
  /*if (prom == NULL) {*/
  /*  MessageBox(NULL, "Memory load fail!", "Error!",*/
  /*             MB_ICONEXCLAMATION | MB_OK); // Display an error message if file not found*/
  /*  return 0;                               // Return 0 to indicate an error*/
  /*}*/
  /*fread(memory, 1, 0x20000, prom); // Read content*/
  /*fclose(prom);                    // Close memory image*/

  reset(); // Reset the emulated CPU

  mloop(); // call for intstrmem filling
  HANDLE thread = CreateThread(NULL, 0, EmulateCPU, NULL, 0,
                               NULL); // Create a separate thread for CPU emulation

  /* Enter the message loop to process messages and input */
  while (GetMessage(&msg, NULL, 0, 0) > 0) { // If new message received...
    TranslateMessage(&msg);                  // Translate key codes to chars if present
    DispatchMessage(&msg);                   // Send the message to the window procedure (WndProc)
  }
  TerminateThread(thread, 0); // Stop the emulation thread
  DeallocateGDI();            // Deallocate GDI resources and return the exit code
  return msg.wParam;          // Exit WinMain
}
