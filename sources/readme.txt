
FPGA64
Reconfigurable hardware based commodore64 emulator.
Copyright 2005-2007 Peter Wendrich (pwsoft@syntiac.com)
http://www.syntiac.com


Version: 0.18
- Added other FPGA platform files.
- Changed reset moment of an internal sprite flag. Now sprites can be displayed in right side border.
- CIA has flag_n input (cassette, rs232 and SRQ line).


Version: 0.17
- Implemented "SBX imm" (aka AXS imm) undocumented opcode.
- Optimised CIA Timer to save around 100 logic-elements in FPGA.


Version: 0.16
- Fixed cpu IO registers at $0000/$0001 and moved them from buslogic to separate vhdl file.
- Fixed a CIA TOD bug preventing starting of TOD in some cases.
- Fixed IRQ/NMI timing of the branch instructions. Interrupts are delayed one cycle if branch is taken, but there is no page crossing.
- Created build and release scripts, so it is easier and less error prone to make release versions.


version: Alpha15
- Fixed F7 key (was wrongly mapped to F4)
- Implemented 'pulseRd' signal so CIA/VIC-II special readonly registers are only triggered on read not write.
- Did a lot of CIA timer fixes. The crX_inmode flags work correctly now. (So timerB can be slaved to timerA)
- Added one cycle delay while writing to the VIC-II registers. The side-border can be opened now!
- Added one cycle delay to NMI.
- Fixed the cycle count of indexed read-modify-write opcodes.


Version: Alpha14
- Splitted 65xx ALU in two for implementation of undoc-opcodes.
- Implemented undoc-opcodes: SLO(ASO) RLA SRE RRA SAX DCP ISC ANC


Version: Alpha13
- Fixed readback of VIC-II sprite expansion registers (fixes random sprite corruption in Paradroid).
- Fixed reading of memory location 3FFF/39FF in VIC-II idle mode.
- Eliminated extra character that appeared on the left side after poke53270,15
- Fixed 2 pixel offset of sprite when it was expanded in X direction. (allows 'Impossible Mission' to be played)
- Changed IRQ timing in cpu (necessary for certain VIC-II tricks to work).


Version: Alpha12
- Increased blanking area to remove artifacts in the first lines of the screen.
- Changed I flag behaviour for 'RTI'. No interrupts are possible just after a RTI.
- Fixed VIC-II emulation so number of cycles/line is 63.
- Fixed bug in generation of (virtual) BA signal.
- Moved cycle of the sprite calculations to enable sprite multiplexing.


Version: Alpha11
- Fixed 8 pixel left shift of bitmap graphics. Lots of games had distored graphics
- Optimised some of the bus-logic, somewhat easier for the vhdl compiler.


Version: Alpha10
- Fixed video sync issue introduced in Alpha9
- Because reading from real SID isn't yet possible, a dummy register $1B is created for games that need it.
- Partial implementation of CIA TOD (time of day)


Version: Alpha9
- Enabled NMI interrupt (did fix in alpha8 but forgot to enable it)
- Fixed crash when "BRK" was encountered after a branch across page boundary.
- Implemented various undocumented "NOP" opcodes.


Version: Alpha8
- Implemented Sprite priority
- Optimised VIC-II graphic modes for less FPGA resources use.
- Fixed NMI interrupts
- Fixed "CMP abs,y" instruction
- Fixed "ORA (zp,x)" instruction
- Fixed "BIT xxx" instructions
- Fixed crash when TXS instruction was executed while interrupt pending.
- Fixed zp,x so it wraps around staying in page 0.
- Fixed "STX zp,x"


Version: Alpha7
- Note: Alpha7 has introduced alot of CPU bugs as it contained a complete new 6510 core
        All of these problems are rectified in Alpha8 and later.

- Implemented Sprite X expansion.
- Implemented Sprite Y expansion.
- Added support for multi-color sprites.
- Added sprite-background collision detection.
- New 6510 emulation for less FPGA resources use.


version: Alpha6

Added sprite-sprite collision detection + interrupt.
VIC-II emulation should be cycle exact now.


Version: Alpha5

After a long delay and some very usefull help from Tobias.
There is another alpha release of the "PAL" core for your c-one.

- Contains preliminary sprite support in VIC-II
- Improved flicker-fixer (hopefully all monitors support the 50Hz mode now...)
- FPGA64 specific MMU/bootload disabled in this version, it uses
  currently the same rom/memory layout as Jerry's ntsc core.

Use F12 to switch between PAL (50Hz) or NTSC (60Hz) mode.

Have fun,
Peter
