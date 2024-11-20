#include <windows.h>
#include <stdio.h>
#include <time.h>
unsigned short memory [65536];                     // Array of 16 bit words simulates 128K memory
struct instrukcije {
   void * execaddr;
   unsigned char dest;
   unsigned char src1;
   unsigned char src2;
} instrmem[65536],*current,*accessed ;

unsigned short regs [16];                          // Array of 16 bit words simulates 16 registers
char asciikeyboard;                                // Variable that keeps ASCII code of the last pressed key
int videochanged=1;                                // Indicates that video memory was changed and the bitmap needs to be redrawn
long cyclecount;                                   // Counters of cycles per emulation frame
struct BitMAPINFO                                  // Structure from Windows API
{ 
    BITMAPINFOHEADER bmiHeader; 
    RGBQUAD         bmiColors[256]; 
} ;
struct BitMAPINFO bi;                              // Define Bitmap  
BYTE* pBits;                                       // Pointer to bitmap memory
HDC memDC;                                         // Memory device context 
HBITMAP hBM ; 
HINSTANCE hinst; 
HWND hwndMain; 
HDC hdc;
// CPU initialization
void reset() {                                  
   regs[15]=0;                                      // Just set program counter to zero
}

//Main emulation loop analyse and execute cyclecount instructions
void mloop() {
   unsigned short ir,op,dest,src1,src2,n;
   unsigned int temp1;
   signed int temp2;

   do {
     ir=memory[regs[15]];                          // Get instruction from address pointed by program counter
 
//     printf("Executing %4x: %4x R0=%4x R1=%4x R2=%4x R3=%4x R4=%4x R5=%4x R6=%4x  R7=%4x  R8=%4x  R13=%4x\n",
//        regs[15],ir,regs[0],regs[1],regs[2],regs[3],regs[4],regs[5],regs[6],regs[7], regs[8], regs[13]  );
     op=(ir & 0xF000)>>12;                         // 4 bits for opcode
     dest=(ir & 0x0F00)>>8;                        // 4 bits for destination
     src1=(ir & 0x00F0)>>4;                        // 4 bits for src1
     src2=(ir & 0x000F);                           // 4 bits for src2
     regs[15]++;                                   // Program counter before execution will point to next instruction
     switch (op) {
        case 0x0:                                  // LOD Rd,Rs1,Rs2
           if (regs[src2]<0xFFF0) {                // For memory area belonging to ROM or RAM
             regs[dest]=memory[regs[src2]];        // Put to destination register 16 bit value pointed by SRC1
           }
           else 
           if (regs[src2]==0xFFF1) {               // If memory belongs to I/O map, for each device
             regs[dest]=asciikeyboard;             // Do special handling
             asciikeyboard=0;                      // This keyboard controller returns ASCII code of the key!
           }
           if (src2==15 && dest !=15)              // If addressed by program counter skip the extra word
             regs[15]++;
 
           break;
        case 0x1:                                  // ADD Rd,Rs1,Rs2
           regs[dest]=regs[src1]+regs[src2];       // Perform addition
           break;
        case 0x2:                                  // SUB Rd,Rs1,Rs2
           regs[dest]=regs[src1]-regs[src2];       // Perform subtraction
           break;
        case 0x3:                                  // AND Rd,Rs1,Rs2
           regs[dest]=regs[src1]&regs[src2];       // Perform logical AND
           break;
        case 0x4:                                  // ORA Rd,Rs1,Rs2
           regs[dest]=regs[src1]|regs[src2];       // Perform logical OR
           break;
        case 0x5:                                  // XOR Rd,Rs1,Rs2
           regs[dest]=regs[src1]^regs[src2];       // Perform logical XOR
           break;
        case 0x6:                                  // SHR Rd,Rs1,Rs2
           n=regs[src2] & 0xF;                     // Extract shift count from bits at position 0-3
           switch((regs[src2] & 0x0030) >> 4)      // Extract shift kind from bits at position 4-5
           {
                
                case 0:
                    temp2=regs[src1];              // Arithmetic shift right preserve highest (sign) bit
                    if (regs[src1]>=0x8000)        // For negative number
                       temp2 |= 0xFFFF0000;        // Set internal 32 bit number to all high 16 bits = 1
                    regs[dest] = temp2 >> (n);     // Then shift n times right
                    break;
                case 1:
                    temp1=regs[src1];              // Logical shift right pushes with zero
                    regs[dest] = temp1 >> (n);     // Then shift n times right
                    break;
                case 2: 
                    regs[dest] = regs[src1] << n;  // Shift left
                    break;
                case 3:
                    regs[dest] = (regs[src1] << n) | 
                      (regs[src1] >> (16 - n));    // Rotate right is clumsy in C, combination of left and right shift
                    break;
            }
           break;
        case 0x7:                                   // MUL Rd,Rs1,Rs2
           regs[dest]=regs[src1]*regs[src2];        // put lower 16 bits of the result into destination
           break;
        case 0x8:                                   // STO Rd,Rs1,Rs2  - stores Rs1 both to Rd and address pointed by Rs2 
                                                    
           if (regs[src2]<0xFFF0) {                 // Area of memory that belongs to RAM 
              regs[dest]=                           // Store value both to destination register
                 memory[regs[src2]]=                // and memory pointed by SRC2 register 
                 regs[src1];
              if (regs[src2]>=0xB000 &&             // Special case for memory belonging to video
                  regs[src2]<0xFB00 ) {
                  int pos=(regs[src2]-0xB000)*16;   // 16 pixels per word, convert to relative position in bitmap
                  unsigned short val=regs[src1];
                  *(pBits+pos)  = (val & 0x8000)?1:0;// Now for each bit in SRC1 register change corresponding byte in bitmap
                  *(pBits+pos+1)= (val & 0x4000)?1:0;
                  *(pBits+pos+2)= (val & 0x2000)?1:0;
                  *(pBits+pos+3)= (val & 0x1000)?1:0;
                  *(pBits+pos+4)= (val & 0x0800)?1:0;
                  *(pBits+pos+5)= (val & 0x0400)?1:0;
                  *(pBits+pos+6)= (val & 0x0200)?1:0;
                  *(pBits+pos+7)= (val & 0x0100)?1:0;
                  *(pBits+pos+8)= (val & 0x0080)?1:0;
                  *(pBits+pos+9)= (val & 0x0040)?1:0;
                  *(pBits+pos+10)=(val & 0x0020)?1:0;
                  *(pBits+pos+11)=(val & 0x0010)?1:0;
                  *(pBits+pos+12)=(val & 0x0008)?1:0;
                  *(pBits+pos+13)=(val & 0x0004)?1:0;
                  *(pBits+pos+14)=(val & 0x0002)?1:0;
                  *(pBits+pos+15)=(val & 0x0001)?1:0;
                  videochanged=1;                     // Video was changed
              }           
           }
           else                                    // Area of memory that belongs to ROM or unidirectional I/O device
           {
              regs[dest]=regs[src1];               // Do not update memory, just destination register
           }
           if (src2==15)                           // If addressed by program counter skip the extra word
             regs[15]++;
           break;
        case 0x9:                                  // MIF Rd,Rs1,Rs2  if Rs1 != 0 then Rd=Rs2
           regs[dest]=regs[src1]?regs[src2]:regs[dest];
           break;
        case 0xA:                                  // GTU Rd,Rs1,Rs2  Greater than unsigned
           regs[dest]=regs[src1]>regs[src2]  ? 1: 0;
           break;
        case 0xB:                                  // GTS Rd,Rs1,Rs2   Greater than signed
           regs[dest]=(signed short) regs[src1]>(signed short) regs[src2]  ? 1: 0;
           break;
        case 0xC:                                  // LTU Rd,Rs1,Rs2   Less than unsigned 
           regs[dest]=regs[src1]<regs[src2]  ? 1: 0;
           break;
        case 0xD:                                  // LTS Rd,Rs1,Rs2   Less than signed 
           regs[dest]=(signed short) regs[src1]<(signed short) regs[src2]  ? 1: 0;
           break;
        case 0xE:                                  // EQU Rd,Rs1,Rs2   Equal
           regs[dest]=regs[src1]==regs[src2]  ? 1: 0;
           break;
        case 0xF:                                  // MAJ Rd,Rs1,Rs2   Move and jump , if src1=R15, useful for subroutines
           regs[dest]=regs[src1];                  // Put SRCq to dest
           regs[15]=regs[src2];                    // Put SRC2 to program counter
           break;
     }
     cyclecount -- ;                               // Reduce cycle count
  } while (cyclecount>0);                          // Exit loop after several instructions
} ;
// Now we assume that MemoryDC is created and bitmap selected
void DisplayDIB(HDC hdc)
{
    BitBlt( hdc,10, 10, 640, 480,                  // Now copy bitmap to window inside client area, 10 pixels from corner
      memDC, 0, 0, SRCCOPY );                      // From memory DC, whole bitmap, direct copy
}

DWORD WINAPI EmulateCPU(void* arg) {               // Thread function executed together with Windows main loop
#define freq 10000000                              // Simulate 10 MHz machine
#define LOWFREQ
  clock_t t1,t2,t3,delayadjust;
  int loopcount;
  double delay;
  loopcount=freq/10;                               // 10 frames per second
  delay=loopcount*(double)CLOCKS_PER_SEC/freq;     // Required time to additional wait floating point
  delayadjust=delay;                               // convert to integer

  while(TRUE)                                      // Thread loop
  {
      t1=clock();                                  // Get current time in clocks
      t2=t1+delayadjust;                           // Add waiting time
      cyclecount=loopcount;
      mloop();                                     // Execute cyclecount cycles i.e. instructions
      if (videochanged) {                          // If video memory was updated during the loop         
         DisplayDIB(hdc);                          // Show it
         videochanged=0;                           // prevent future refreshes if video was not changed
      }
#ifdef LOWFREQ
      t3=clock();                                  // Get clock
      if (t3<t2)                                   // If time not elapsed
         Sleep((t2-t3)*CLOCKS_PER_SEC/1000);       // Suspend, but Sleep uses millisecond as unit
#else
       while (t2 <clock()) {} ;                    // Busy wait variant
#endif
  } 
}

/* Called on exit */ 

void DeallocateGDI() 
{ 
    ReleaseDC(hwndMain,hdc);                       // The ReleaseDC function releases a device context (DC), freeing it for use by other applications.
    DeleteDC(memDC);                               // The DeleteDC function deletes the specified device context (DC)     
    free(pBits);                                   // Deallocate bitmap bits
    DeleteObject(hBM);                             // The DeleteObject function deletes a logical pen, brush, font, bitmap, region, or palette, freeing all system resources associated with the object.
}

void CreateDIB() {
/* The CreateDIBSection function creates a device indenpendant bitmap that applications can write to directly. 
    The function gives you a pointer to the location of the bitmap bit values. 
      You can supply a handle to a file-mapping object that the function will use to create the bitmap, 
     or you can let the system allocate the memory for the bitmap.
*/
    hdc=GetDC(hwndMain);
    int i; 
    bi.bmiHeader.biSize         = sizeof(BITMAPINFOHEADER);
    bi.bmiHeader.biWidth        = 640;              // 640 pixels width
    bi.bmiHeader.biHeight       = -480;             // 480 pixels height negative number == top down DIB
    bi.bmiHeader.biPlanes       = 1;                // Number of planes must be set to 1
    bi.bmiHeader.biBitCount     = 8;                // 8 bits per pixel, easiest way to access each pixel
    bi.bmiHeader.biCompression  = BI_RGB;           // No compression
    bi.bmiHeader.biClrUsed      = 2;                // Just two colors 0 background, 1 foreground
    bi.bmiColors[0].rgbRed      = 0xE0;             // Defines cream yellow background
    bi.bmiColors[0].rgbGreen    = 0xDE;
    bi.bmiColors[0].rgbBlue     = 0xBC;
    bi.bmiColors[0].rgbReserved = 0; 
    bi.bmiColors[1].rgbRed      = 0;                // Defines black foreground 
    bi.bmiColors[1].rgbGreen    = 0;
    bi.bmiColors[1].rgbBlue     = 0;
    bi.bmiColors[1].rgbReserved = 0; 

    hBM = (HBITMAP)CreateDIBSection(
       hdc,                                         // Device context of the window
       (BITMAPINFO*)&bi,                            // Pointer to BITMAPINFO, defines structure 
       DIB_RGB_COLORS,                              // usage: palette is defined using RGB colors
       (VOID**)&pBits,                              // Pointer to binary values of the pixels
       NULL,                                        // Do not use file mapping for DIB
       0 );                                         // Offset in DIB not important

    memDC = CreateCompatibleDC( hdc );              // creates a memory device context (memDC) that is compatible with the 
                                                    // device context (hdc) of the specified window (hwnd). 
                                                    // A device context is a data structure that defines the attributes of 
                                                    // drawing and painting operations. This memory device context is 
                                                    // typically used to manipulate a bitmap before displaying it.
    SelectObject( memDC,hBM );                      // Selects a bitmap (hBM) into the memory device context (memDC). 
                                                    // This means that all drawing operations performed on memDC will be 
                                                    // applied to the selected bitmap. 

}


/* This is where all the input to the window goes to */
LRESULT CALLBACK WndProc(HWND hwnd, UINT Message, WPARAM wParam, LPARAM lParam) {
PAINTSTRUCT ps; 
HDC hdc;

    switch(Message) {
        case WM_DESTROY: {                         // Upon destruction, tell the main thread to stop
            PostQuitMessage(0);                    // Main Window loop will end
            break;
        }
        case WM_CHAR: {                            // Key pressed
            asciikeyboard=wParam;                  // Get ASCII code of key
            break;    
        }    
        case WM_PAINT: {                           // Main window overlapped or changed size
            hdc = BeginPaint(hwnd, &ps);           // Refresh is done in thread anyway
            videochanged=1;                        // So just indicate to do it
            EndPaint(hwnd, &ps);                   // End updating
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
    WNDCLASSEX wc;                                 // Window class
    MSG msg;                                       // A temporary location for all messages

    /* Initialize and register the window class structure */
    memset(&wc, 0, sizeof(wc));                    // Fill all bytes of the window class structure to zero
    wc.cbSize = sizeof(WNDCLASSEX);                // First field must be size of the structure 
    wc.lpfnWndProc = WndProc;                      // This is where we will send messages to
    wc.hInstance = hInstance;                      // Handle to process instance
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground = (HBRUSH)(COLOR_WINDOW + 1); // Set the background color of the window to a system color (COLOR_WINDOW)
    wc.lpszClassName = "WindowClass";              // Set the name of the window class
    wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);    // Load a standard icon for the window
    wc.hIconSm = LoadIcon(NULL, IDI_APPLICATION);  // Set a small icon (if available) for the window

    
    if (!RegisterClassEx(&wc))                     // Register the window class with Windows
    {
        MessageBox(NULL, 
          "Window Registration Failed!", "Error!", 
          MB_ICONEXCLAMATION | MB_OK);             // Display an error message if registration fails
        return 0;                                  // Return 0 to indicate an error
    }    
    hwndMain = CreateWindowEx(                     // Create the main application window
        WS_EX_CLIENTEDGE, 
        "WindowClass", "SVEU16 emulator",
        WS_VISIBLE | WS_OVERLAPPEDWINDOW,
        0, 0, 680, 540, 
        NULL, NULL, hInstance, NULL); 

    if (hwndMain == NULL) {      
        MessageBox(NULL, 
        "Window Creation Failed!", "Error!", 
        MB_ICONEXCLAMATION | MB_OK);               // Display an error message if window creation fails
        return 0;                                  // Return 0 to indicate an error 
    }

    
    RECT rc;
    GetWindowRect(hwndMain, &rc);                  // Rect contains rc current window rectangle in screen coordinates
    int xPos = (GetSystemMetrics(SM_CXSCREEN) - rc.right) / 2;  // Half between screen width and right window edge
    int yPos = (GetSystemMetrics(SM_CYSCREEN) - rc.bottom) / 2; // Half between screen heighg and bottom window edge
    SetWindowPos(hwndMain, 0, xPos, yPos, 0, 0, SWP_NOZORDER | SWP_NOSIZE);    // Center the window on the screen 
    UpdateWindow(hwndMain);                        // Update and display the window
  
    CreateDIB();                                   // Create the DIB (Device-Independent Bitmap) for display simulated video memory       
    HANDLE thread = CreateThread(NULL, 0,
        EmulateCPU, NULL, 0, NULL);                // Create a separate thread for CPU emulation   
    FILE* prom = fopen("forth.mem", "r");          // Open a file with simulated RAM content
    if (prom == NULL) {
        MessageBox(NULL, "Memory load fail!", "Error!",
        MB_ICONEXCLAMATION | MB_OK);               // Display an error message if file not found
        return 0;                                  // Return 0 to indicate an error
    }
    fread(memory, 1, 0x20000, prom);               // Read content
    fclose(prom);                                  // Close memory image
    reset();                                       // Reset the emulated CPU

    /* Enter the message loop to process messages and input */
    while (GetMessage(&msg, NULL, 0, 0) > 0) {     // If new message received... 
        TranslateMessage(&msg);                    // Translate key codes to chars if present
        DispatchMessage(&msg);                     // Send the message to the window procedure (WndProc)
    }    
    TerminateThread(thread,0);                     // Stop the emulation thread 
    DeallocateGDI();                               // Deallocate GDI resources and return the exit code
    return msg.wParam;                             // Exit WinMain 
}