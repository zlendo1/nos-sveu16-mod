# nos-sveu16-mod

## Overview

This project was done as part of our Advanced Operating Systems coursework,
based on a fork of the existing work of our teacher - the SVEU16 CPU emulator
and accompanying operating system ForthOS. The goal of this fork was to
implement a myriad of new features, as well as improving the existing
performance of the system and ensuring a better development experience.

## Features

### New Emulator Features

1. **Improved Interpretation Method**: Exchanged basic interpretation for
   directly threaded interpretation, sacrificing a bit of startup time for
   massive improvements in runtime performance.
2. **Modified Instruction Set Behavior**: Modified existing instruction set to
   accommodate for I/O virtualization and the new video display system.
3. **16-Segment Display Adaptation**: Adapted the existing emulator GUI display
   for the new 16-segment display video system.
4. **Peripheral I/O Virtualization**: Added support for virtualizing peripheral
   I/O systems, allowing for deeper integration with hardware.

### New Operating System Features

1. **Bootstrap Loader**: Implemented a bootstrap sequence using the new
   interface provided by a previously mentioned modification to the instruction
   set, such that the OS can load itself into memory instead of directly loading
   the entire OS into memory through the emulator.
2. **16-Segment Display System**: Changed previous ASCII raster-based display to
   a 16-segment display system, reducing memory usage and simplifying character
   encoding.

## Getting Started

This implementation of the SVEU16 emulator is designed using the Windows API,
requiring cross-compilation for developers using a POSIX environment. We
recommend reading up on the basics of Windows API before getting into this
codebase. The operating system itself is written in the 16-word instruction set
of the SVEU16 CPU. A general understanding of RISC is enough to understand this
component.

### Prerequisites

- **C Compiler**: GCC, Clang, or MinGW for cross-compilation.
- **Build System**: Make.
- **Host OS**: Windows or a POSIX system with compatibility layer (we recommend
  [WINE](https://www.winehq.org/)).

### Installation:

1. **Clone the Repository**:
   ```bash
   git clone git@github.com:zlendo1/nos-sveu16-mod.git
   cd nos-sveu16-mod
   ```
2. **Build the Project**:
   ```bash
   make all
   ```
3. **Run the Emulator**:
   ```bash
   ./emulgui.exe
   ```
   Or if you use a POSIX-compliant operating system:
   ```bash
   wine emulgui.exe
   ```

### Usage

- **Emulator**: Use the emulator to run the OS and interact with the OS and its
  systems.
- **Operating System**: Boot the OS through the emulator and explore its new
  features.

## Contributing

We welcome contributions from the community! If you'd like to contribute, please
follow these steps:

1. Fork the repository.
2. Create a new branch for your feature or bug fix.
3. Submit a pull request with a detailed description of your changes.

## License

This project is licensed under the GNU General Public License v3.0.

## Acknowledgments

We'd like to give big thanks to our teacher Dr. Samir Ribić for starting this
project, guiding us through this course assignment and giving us the incredible
opportunity to learn more about this topic.

## Contact

For any questions, issues, or feedback, please open an issue on GitHub or
contact the project host at [zlendo1@etf.unsa.ba](mailto:zlendo1@etf.unsa.ba).
