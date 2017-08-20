-- -----------------------------------------------------------------------
--
--                                 FPGA 64
--
--     A fully functional commodore 64 implementation in a single FPGA
--
-- -----------------------------------------------------------------------
-- Peter Wendrich (pwsoft@syntiac.com)
-- http://www.syntiac.com/fpga64.html
-- -----------------------------------------------------------------------
--
-- VIC-II - Video Interface Chip no 2
--
-- -----------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_unsigned.ALL;
use IEEE.numeric_std.ALL;

-- -----------------------------------------------------------------------

entity vicii_6567_6569 is
	generic (
		graphicsEnable : std_logic := '1'
	);
	port (
		clk: in std_logic;
		enable : in std_logic;
		endOfCycle : in std_logic;
		phi0_cpu : in std_logic;
		phi0_vic : in std_logic;
		mode6569 : in std_logic; -- 63 cycles / line
		mode6567old : in std_logic; -- 64 cycles / line
		mode6567R8 : in std_logic; -- 65 cycles / line
				
		cs : in std_logic;
		we : in std_logic;
		rd : in std_logic;
		lp_n : in std_logic;

		addr: in unsigned(5 downto 0);
		di: in unsigned(7 downto 0);
		diColor: in unsigned(3 downto 0);
		do: out unsigned(7 downto 0);

		ba: out std_logic;

		vicAddr: out unsigned(13 downto 0);
		irq_n: out std_logic;

		-- Video output
		hsync : out std_logic;
		vsync : out std_logic;
		
		colorIndex : out unsigned(3 downto 0)
	);
end vicii_6567_6569;

-- -----------------------------------------------------------------------

architecture rtl of vicii_6567_6569 is
type vicCycles is (
		cycleRefresh1, -- Memory refresh cycles
		cycleBa1, cycleBa2, cycleBa3, cycleChar,  -- claim bus from cpu
		cycleCalcSprites, cycleSpriteBa1, cycleSpriteBa2, cycleSpriteBa3,
		cycleSpriteA, cycleSpriteB
	);
	type MFlags is array(0 to 7) of boolean;
	type MTwoBits is array(0 to 7) of unsigned(0 to 1);
	type MXdef is array(0 to 7) of unsigned(8 downto 0);
	type MYdef is array(0 to 7) of unsigned(7 downto 0);


-- User registers
	signal MX : MXdef; -- Sprite X
	signal MY : MYdef; -- Sprite Y
	signal MXE : unsigned(7 downto 0); -- Sprite X expansion
	signal MYE : unsigned(7 downto 0); -- Sprite Y expansion
	signal MPRIO : unsigned(7 downto 0); -- Sprite priority
	signal MC : unsigned(7 downto 0); -- sprite multi color

	signal EC : unsigned(3 downto 0); -- border color
	signal B0C : unsigned(3 downto 0); -- background color 0
	signal B1C : unsigned(3 downto 0); -- background color 1
	signal B2C : unsigned(3 downto 0); -- background color 2
	signal B3C : unsigned(3 downto 0); -- background color 3
	signal MM0 : unsigned(3 downto 0); -- sprite multicolor 0
	signal MM1 : unsigned(3 downto 0); -- sprite multicolor 1

-- Internal registers
	signal Mrun : MFlags; -- Sprite is active
	signal Mshift : MFlags; -- Sprite is shifting
	signal McurrentPixels : MTwoBits; -- Output of sprite shift register
	signal Moddeven : unsigned(7 downto 0); -- Lowest bit toggle for shift register
	signal MXE_ff : unsigned(7 downto 0); -- Sprite X expansion flipflop
	signal MYE_ff : unsigned(7 downto 0); -- Sprite Y expansion flipflop

--	signal Boddeven : std_logic;

-- State machine
	signal vicCycle : vicCycles := cycleRefresh1;
	signal currentSprite : unsigned(2 downto 0) := "000";
	signal shiftChars : boolean;
	signal shiftCharsReg : boolean;



	type mob is record
		e: std_logic;  -- enabled
		xe: std_logic; -- x size doubling
		ye: std_logic; -- y size doubling
		dp: std_logic;

		-- pointer to memory
		ptr: unsigned(7 downto 0);

		-- byte counter
		cntBase: unsigned(5 downto 0);

		-- working registers
		xe_ff : std_logic;
		ye_ff : std_logic;
		pixels: unsigned(0 to 23);
	end record;
	type mobs is array(integer range 0 to 7) of mob;

	signal m: mobs := (
		others => (
			e => '0',
			xe => '0',
			ye => '0',
			dp => '0',

			ptr => (others => '0'),
			cntBase => (others => '1'),

--			isOn => '0',
			xe_ff => '0',
			ye_ff => '0',
			pixels => (others => '0')
		)
	);



-- Bus access
signal theVicAddr : unsigned(13 downto 0);

signal baChars : std_logic;
signal baSprite04 : std_logic;
signal baSprite15 : std_logic;
signal baSprite26 : std_logic;
signal baSprite37 : std_logic;

-- Raster counters
signal rasterX : unsigned(9 downto 0) := (others => '0');
signal rasterY : unsigned(8 downto 0) := (others => '0');
signal lpX : unsigned(7 downto 0);
signal lpY : unsigned(7 downto 0);

-- borders and blanking
signal rasterEnable: std_logic;
signal LRBorder: std_logic;
signal TBBorder: std_logic;
signal hBlack: std_logic;
signal vBlanking : std_logic;
signal hBlanking : std_logic;

-- Light pen
signal lightPenHit: std_logic;



signal xscroll: unsigned(2 downto 0);
signal yscroll: unsigned(2 downto 0);
signal rasterCmp : unsigned(8 downto 0);

	signal ECM: std_logic;
	signal BMM: std_logic;
	signal DEN: std_logic;
	signal RSEL: std_logic;

	signal RES: std_logic;
	signal MCM: std_logic;
	signal CSEL: std_logic;

	signal VM: unsigned(13 downto 10);
	signal CB: unsigned(13 downto 11);

	-- IRQ
	signal IRST: std_logic := '0';
	signal ERST: std_logic := '0';
	signal IMBC: std_logic := '0';
	signal EMBC: std_logic := '0';
	signal IMMC: std_logic := '0';
	signal EMMC: std_logic := '0';
	signal ILP: std_logic := '0';
	signal ELP: std_logic := '0';
	signal IRQ: std_logic;

signal rasterHit : std_logic; -- Only one interrupt each rasterLine
signal resetLightPenIrq: std_logic;
signal resetIMMC : std_logic;
signal resetIMBC : std_logic;
signal resetRasterIrq : std_logic;

signal M2M: unsigned(7 downto 0); -- Sprite to sprite collision
signal M2D: unsigned(7 downto 0); -- Sprite to character collision
signal M2Mhit : std_logic;
signal M2Dhit : std_logic;

type spriteColorsDef is array(7 downto 0) of unsigned(3 downto 0);
signal spriteColors: spriteColorsDef;

	signal ColCounter: unsigned(9 downto 0) := (others => '0');
	signal ColRestart: unsigned(9 downto 0) := (others => '0');
	signal RowCounter: unsigned(2 downto 0) := (others => '0');
	signal lastColCounter: unsigned(9 downto 0) := (others => '0');

	signal idle: std_logic := '1';

	-- characters
	type charStoreDef is array(38 downto 0) of unsigned(11 downto 0);
	signal charStore: charStoreDef;
	signal nextChar: unsigned(11 downto 0);

	signal storeChar : unsigned(11 downto 0);
	signal storePixels : unsigned(7 downto 0);
	signal currentChar : unsigned(11 downto 0);
	signal currentPixels : unsigned(0 to 7);

	type pixelColorStoreDef is array(9 downto 0) of unsigned(3 downto 0);
	signal pixelColorStore: pixelColorStoreDef;
	signal foregroundBits: unsigned(9 downto 0); -- For collision detection

begin

vicStateMachine: process(clk)
	begin
		if rising_edge(clk) then
			if (endOfCycle = '1') then
				case vicCycle is
				when cycleRefresh1 => vicCycle <= cycleBa1;		
				when cycleBa1 => vicCycle <= cycleBa2;
				when cycleBa2 => vicCycle <= cycleBa3;
				when cycleBa3 => vicCycle <= cycleChar; -- X=0..7 on this cycle
				when cycleChar =>
					if ((mode6569  = '1') and rasterX(9 downto 3) = "0100111") -- PAL
					or ((mode6567old  = '1') and rasterX(9 downto 3) = "0101000") -- Old NTSC
					or ((mode6567R8  = '1') and rasterX(9 downto 3) = "0101001") then -- New NTSC
						vicCycle <= cycleCalcSprites;
					end if;
				when cycleCalcSprites => vicCycle <= cycleSpriteBa1;
				when cycleSpriteBa1 => vicCycle <= cycleSpriteBa2;
				when cycleSpriteBa2 => vicCycle <= cycleSpriteBa3;
				when cycleSpriteBa3 => vicCycle <= cycleSpriteA;
				when cycleSpriteA =>
					vicCycle <= cycleSpriteB;
				when cycleSpriteB =>
					vicCycle <= cycleSpriteA;
					if currentSprite = "111" then
						vicCycle <= cycleRefresh1;
					end if;
				end case;												
				if vicCycle = cycleSpriteB then
					currentSprite <= currentSprite + 1;
				end if;
			end if;
		end if;			
	end process;

rasterCounters: process(clk, mode6569, mode6567old, mode6567R8)
		variable rasterEnd : integer range 0 to 312;
	begin
		rasterEnd := 255; -- PAL 312
		if mode6567old = '1' then
			rasterEnd := 256; -- NTSC (R7 and earlier have 262 lines)
		end if;
		if mode6567R8 = '1' then
			rasterEnd := 257; -- NTSC (R8 and newer have 263 lines)
		end if;
		if rising_edge(clk) then
			if enable = '1' then
				rasterX <= rasterX + 1;
			end if;				
			if endOfCycle = '1' then
				if vicCycle = cycleBa2 then
					rasterX <= (others => '0');
				end if;
				-- In line 0 the RasterIRQ is one cycle later
				-- So I suspect the reset is done when rasterY reaches
				-- lastLine + 1 instead of lastLine. This would cause
				-- one cycle delay. We should test if rasterIRQ can be
				-- done on lastLine + 1 on real VIC-II. As this is
				-- possible in current implementation here.
				if rasterY = rasterEnd then
					rasterY <= (others => '0');
				end if;
				if (vicCycle = cycleSpriteB)
				and (currentSprite = 2) then
					rasterY <= rasterY + 1;
					rasterHit <= '0';
				end if;					
			end if;
			if resetRasterIrq = '1' then
				IRST <= '0';
			end if;

			if (rasterHit = '0')
			and (rasterY = rasterCmp) then
				rasterHit <= '1';
				IRST <= '1';
			end if;
		end if;
	end process;


doVBlanking: process(clk)
	begin
		if rising_edge(clk) then
			vBlanking <= '0';
			if mode6569 = '1' then
				if rasterY = 300 then
					vBlanking <= '1';
				end if;
			end if;
			if (mode6567old or mode6567R8) = '1' then
				if rasterY = 12 then
					vBlanking <= '1';
				end if;
			end if;
		end if;
	end process;
	
	
-- On a negative edge on the LP input, the current position of the raster beam
-- is latched in the registers LPX ($d013) and LPY ($d014). LPX contains the
-- upper 8 bits (of 9) of the X position and LPY the lower 8 bits (likewise of
-- 9) of the Y position. So the horizontal resolution of the light pen is
-- limited to 2 pixels.

-- Only one negative edge on LP is recognized per frame. If multiple edges
-- occur on LP, all following ones are ignored. The trigger is not released
-- until the next vertical blanking interval.
lightPen: process(clk)
	begin
		if rising_edge(clk) then
			if resetLightPenIrq = '1' then
				-- Reset light pen interrupt
				ILP <= '0';
			end if;			
			if vBlanking = '1' then
				-- Reset lightpen state at beginning of frame
				lightPenHit <= '0';
			elsif (lightPenHit = '0') and (lp_n = '0') then
				-- One hit/frame
				lightPenHit <= '1'; 
				-- Toggle Interrupt
				ILP <= '1'; 
				-- Store position of beam
				lpx <= rasterX(8 downto 1);
				lpy <= rasterY(7 downto 0);
			end if;
		end if;
	end process;

addressing: process(vicCycle, VM, CB, phi0_vic, phi0_cpu, currentSprite, m, idle)
	begin
--		if rising_edge(clk) then
		theVicAddr <= (others => '-');


		if phi0_vic = '1' then
			case vicCycle is
			when cycleSpriteA =>
				theVicAddr <= VM & "1111111" & currentSprite;
			when cycleSpriteB =>
				theVicAddr <= m(to_integer(currentSprite)).ptr & (m(to_integer(currentSprite)).cntBase + 1);
			when others =>
				theVicAddr <= (others => '1');
				if (idle = '0')
				and shiftCharsReg then
					if BMM = '1' then
						theVicAddr <= CB(13) & lastColCounter & rowCounter;
					else
						theVicAddr <= CB & nextChar(7 downto 0) & rowCounter;
					end if;
				end if;
				if ECM = '1' then
					theVicAddr(10 downto 9) <= "00";
				end if;					
			end case;
		elsif phi0_cpu = '1' then
			case vicCycle is
			when cycleSpriteA =>
				theVicAddr <= m(to_integer(currentSprite)).ptr & m(to_integer(currentSprite)).cntBase;
 			when cycleSpriteB =>
				theVicAddr <= m(to_integer(currentSprite)).ptr & (m(to_integer(currentSprite)).cntBase + 2);
			when others =>
				theVicAddr <= VM & colCounter;
			end case;
		end if;
--		end if;
	end process;
	vicAddr <= theVicAddr;

borderLogic: process(clk)
		variable newTBBorder: std_logic;
	begin
		if rising_edge(clk) then
			if enable = '1' then
				--
				-- Calc top/bottom border
				newTBBorder := TBBorder;
				if (rasterY = 55) and (RSEL = '0') and (rasterEnable = '1') then
					newTBBorder := '0';
				end if;
				if (rasterY = 51) and (RSEL = '1') and (rasterEnable = '1') then
					newTBBorder := '0';
				end if;
				if (rasterY = 247) and (RSEL = '0') then
					newTBBorder := '1';
				end if;
				if (rasterY = 251) and (RSEL = '1') then
					newTBBorder := '1';
				end if;

				--
				-- Calc left/right border
				if (rasterX = 33) and (CSEL = '0') then
					LRBorder <= newTBBorder;
					TBBorder <= newTBBorder;
				end if;
				if (rasterX = 26) and (CSEL = '1') then
					LRBorder <= newTBBorder;
					TBBorder <= newTBBorder;
				end if;
				if (rasterX = 337) and (CSEL = '0') then
					LRBorder <= '1';
				end if;
				if (rasterX = 346) and (CSEL = '1') then
					LRBorder <= '1';
				end if;
			end if;
			if endOfCycle = '1' then
				if (currentSprite = 2) then
					hBlack <= '1';
				end if;						
				if vicCycle = cycleBa1 then
					hBlack <= '0';
				end if;
				if (currentSprite = 4) then
					hBlanking <= '1';
				else					
					hBlanking <= '0';
				end if;
			end if;
		end if;
	end process;

calcRasterEnable: process(clk)
	begin
		-- Enable screen and character display.
		-- This is only possible in line 48 on the VIC-II.
		-- On other lines any DEN changes are ignored.
		if rising_edge(clk) then
			if (rasterY = 48) and (DEN = '1') then
				rasterEnable <= '1';
			end if;
			if (rasterY = 248) then
				rasterEnable <= '0';
			end if;
		end if;
	end process;

calcSpritesRunning: process(m)
	begin
		for i in 0 to 7 loop
			Mrun(i) <= false;
			if m(i).cntBase /= 63 then
				Mrun(i) <= true;
			end if;
		end loop;
	end process;

calcSprites: process(clk)
	begin
		if rising_edge(clk) then
			if endOfCycle = '1' then
				case vicCycle is
				when cycleBa3 =>
					for i in 0 to 7 loop
						MYE_ff(i) <= (not MYE_ff(i)) and MYE(i);
						if Mrun(i) then
							if MYE_ff(i) = '0' then
								m(i).cntBase <= m(i).cntBase + 3;
							end if;
						end if;
					end loop;
				when cycleCalcSprites =>
					for i in 0 to 7 loop
						if (not Mrun(i))
						and (m(i).e = '1')
						and (rasterY = MY(i)) then
							m(i).cntBase <= (others => '0');
							MYE_ff(i) <= MYE(i);
						end if;
					end loop;						
				when others =>
					null;
				end case;								
			end if;
				
			if enable = '1' then
				-- Enable sprites on the correct X position
				for i in 0 to 7 loop
					if rasterX = MX(i) then
						Mshift(i) <= true;
					end if;
				end loop;
				
				-- Shift one pixel of the sprite from the shift register.
				for i in 0 to 7 loop
					if Mshift(i) then
						MXE_ff(i) <= (not MXE_ff(i)) and MXE(i);
						if MXE_ff(i) = '0' then
							Moddeven(i) <= not Moddeven(i);
							if Moddeven(i) = '1' then
								m(i).pixels <= m(i).pixels(2 to 23) & "00";
								McurrentPixels(i) <= m(i).pixels(0 to 1);
							end if;
						end if;							
					else
						MXE_ff(i) <= MXE(i);
						Moddeven(i) <= MXE(i); -- oddeven set to X expansion to force output in second cycle
						McurrentPixels(i) <= "00";
					end if;
				end loop;
			end if;

			if enable = '1' then
				if Mrun(to_integer(currentSprite)) then
					if phi0_vic = '1' then
						case vicCycle is
						when cycleSpriteA => m(to_integer(currentSprite)).ptr <= di;
						                     Mshift(to_integer(currentSprite)) <= false;
						when cycleSpriteB => m(to_integer(currentSprite)).pixels <= m(to_integer(currentSprite)).pixels(8 to 23) & di;
						when others => null;
						end case;			
					end if;
					if phi0_cpu = '1' then
						case vicCycle is
						when cycleSpriteA
						   | cycleSpriteB => m(to_integer(currentSprite)).pixels <= m(to_integer(currentSprite)).pixels(8 to 23) & di;
						when others => null;
						end case;
					end if;
				end if;
			end if;
		end if;
	end process;

pixelShifter: process(clk)
		variable multiColor : std_logic;
	begin
		if rising_edge(clk) then
			if enable = '1' then
				-- Multicolor mode is active with MCM, but for character
				-- mode it depends on bit3 of color ram (currentChar(11))
				multiColor := MCM and (BMM or ECM or currentChar(11));
				
				if multiColor = '0' then
					currentPixels <= currentPixels(1 to 7) & "0";
				else
					-- Shift two pixels when in multicolor mode
					if rasterX(0) = '1' then
						currentPixels <= currentPixels(2 to 7) & "00";
					end if;
				end if;
				
				-- Calculate if pixel is in foreground or background
				foregroundBits <= foregroundBits(8 downto 0) & currentPixels(0);

				-- Calculate color of next pixel				
				pixelColorStore <= pixelColorStore(8 downto 0) & B0C;
				if (BMM = '0') and (ECM='0') then
					if (MCM = '0') or (currentChar(11) = '0') then
						-- normal character mode
						if currentPixels(0) = '1' then
							pixelColorStore(0) <= currentChar(11 downto 8);
						end if;							
					else
						-- multi-color character mode
						case currentPixels(0 to 1) is
						when "01" => pixelColorStore(0) <= B1C;
						when "10" => pixelColorStore(0) <= B2C;
						when "11" => pixelColorStore(0) <= '0' & currentChar(10 downto 8);
						when others => null;
						end case;
					end if;
				elsif (MCM = '0') and (BMM = '0') and (ECM='1') then
					-- extended-color character mode
					-- multiple background colors but only 64 characters
					if currentPixels(0) = '1' then
						pixelColorStore(0) <= currentChar(11 downto 8);
					else
						case currentChar(7 downto 6) is
						when "01" => pixelColorStore(0) <= B1C;
						when "10" => pixelColorStore(0) <= B2C;
						when "11" => pixelColorStore(0) <= B3C;
						when others	=> null;
						end case;
					end if;
				elsif (graphicsEnable = '1') and (MCM = '0') and (BMM = '1') and (ECM='0') then
					-- highres bitmap mode
					if currentPixels(0) = '1' then
						pixelColorStore(0) <= currentChar(7 downto 4);
					else
						pixelColorStore(0) <= currentChar(3 downto 0);
					end if;
				elsif (graphicsEnable = '1') and (MCM = '1') and (BMM = '1') and (ECM='0') then
					-- Multi-color bitmap mode
					case currentPixels(0 to 1) is
					when "01" => pixelColorStore(0) <= currentChar(7 downto 4);
					when "10" => pixelColorStore(0) <= currentChar(3 downto 0);
					when "11" => pixelColorStore(0) <= currentChar(11 downto 8);
					when others => null;
					end case;						
				else
					-- illegal display mode, force output to black
					pixelColorStore(0) <= "0000";
				end if;
				
				-- Process data read from memory
				if phi0_cpu = '1' then
					if shiftChars then
						if baChars = '0' then
							nextChar <= diColor & di;
						else
							nextChar <= charStore(38);
						end if;
						charStore(38 downto 0) <= charStore(37 downto 0) & nextChar;
					end if;		
				end if;
				if phi0_vic = '1' then
					storeChar <= nextChar;
					storePixels <= di;
				end if;
			end if;
			if endOfCycle = '1' then
				currentChar <= (others => '0');
				if shiftCharsReg then
					currentPixels <= storePixels;
					if idle = '0' then
						currentChar <= storeChar;
					end if;
				end if;
				shiftCharsReg <= shiftChars;
			end if;				
		end if;
	end process;

calcShiftChars: process(clk)
	begin
		if rising_edge(clk) then
			if endOfCycle = '1' then
				if rasterX(9 downto 3) = "0000000" then
					shiftChars <= true;
				end if;
				if rasterX(9 downto 3) = "0101000" then
					shiftChars <= false;
				end if;
			end if;
		end if;
	end process;

	-- Generate BA signal.
	-- When BA is held low the processor is stopped so the VIC-II can
	-- use its cycles. In this implementation we combine sprite 0 with 4,
	-- sprite 1 with 5, sprite 2 with 6 and sprite 3 with 7 to save
	-- hardware resources.
	-- Due to a bug in the 65xx processors, BA goes low 3 cycles before
	-- the VIC needs the bus.
calcBa: process(clk)
	begin
		if rising_edge(clk) then
			baChars <= '1';
			if (vicCycle = cycleBa1)
			or (vicCycle = cycleBa2)
			or (vicCycle = cycleBa3)
			or (rasterX(9 downto 3) < "0101001") then
				if (rasterY(2 downto 0) = yscroll)
				and (rasterEnable = '1') then
					baChars <= '0';
				end if;
			end if;
			
			if currentSprite = 1 then
				baSprite04 <= '1';
			end if;
			if currentSprite = 2 then
				baSprite15 <= '1';
			end if;
			if currentSprite = 3 then
				baSprite26 <= '1';
			end if;
			if currentSprite = 4 then
				baSprite37 <= '1';
			end if;
			if currentSprite = 5 then
				baSprite04 <= '1';
			end if;
			if currentSprite = 6 then
				baSprite15 <= '1';
			end if;
			if currentSprite = 7 then
				baSprite26 <= '1';
			end if;
			if vicCycle = cycleRefresh1 then
				baSprite37 <= '1';
			end if;
			
			if Mrun(0) and (vicCycle = cycleSpriteBa1) then
				baSprite04 <= '0';
			end if;
			if Mrun(1) and (vicCycle = cycleSpriteBa3) then
				baSprite15 <= '0';
			end if;
			if Mrun(2) and (vicCycle = cycleSpriteB) and (currentSprite = 0) then
				baSprite26 <= '0';
			end if;
			if Mrun(3) and (vicCycle = cycleSpriteB) and (currentSprite = 1) then
				baSprite37 <= '0';
			end if;
			if Mrun(4) and (vicCycle = cycleSpriteB) and (currentSprite = 2) then
				baSprite04 <= '0';
			end if;
			if Mrun(5) and (vicCycle = cycleSpriteB) and (currentSprite = 3) then
				baSprite15 <= '0';
			end if;
			if Mrun(6) and (vicCycle = cycleSpriteB) and (currentSprite = 4) then
				baSprite26 <= '0';
			end if;
			if Mrun(7) and (vicCycle = cycleSpriteB) and (currentSprite = 5) then
				baSprite37 <= '0';
			end if;
		end if;
	end process;
	ba <= baChars and baSprite04 and baSprite15 and baSprite26 and baSprite37;

	rasterLogic: process(clk)
	begin
		if rising_edge(clk) then
			if endOfCycle = '1' then
				if shiftChars then
					colCounter <= colCounter + 1;
					lastColCounter <= colCounter;
				end if;
				case vicCycle is
				when cycleBa3 =>
					colCounter <= colRestart;
					if (rasterY(2 downto 0) = yscroll)
					and (48 <= rasterY)
					and (rasterY < 248) then
						rowCounter <= (others => '0');
						idle <= '0';
					end if;
				when cycleSpriteBa3 =>
					if rowCounter = 7 then
						if idle = '0' then
							colRestart <= colCounter;
						end if;
						idle <= '1';
					else
						rowCounter <= rowCounter + 1;
					end if;
				when others =>
					null;					
				end case;
				if rasterY = 0 then
					colRestart <= (others => '0');
				end if;
			end if;				
		end if;
	end process;

spriteSpriteCollision: process(clk)
		variable collision : unsigned(7 downto 0);
	begin
		if rising_edge(clk) then			
			if resetIMMC = '1' then
				IMMC <= '0';
			end if;

			if (cs = '1')
			and (rd = '1')
			and	(addr = "011110") then
				M2M <= (others => '0');
				M2Mhit <= '0';
			end if;

			for i in 0 to 7 loop
				collision(i) := McurrentPixels(i)(0);
			end loop;
			if (collision /= "00000000")
			and (collision /= "00000001")
			and (collision /= "00000010")
			and (collision /= "00000100")
			and (collision /= "00001000")
			and (collision /= "00010000")
			and (collision /= "00100000")
			and (collision /= "01000000")
			and (collision /= "10000000")
			and (TBBorder = '0') then
				M2M <= M2M or collision;
				
				-- Give collision interrupt but only once until clear of register
				if M2Mhit = '0' then
					IMMC <= '1';
					M2Mhit <= '1';
				end if;
			end if;
		end if;
	end process;

spriteBackgroundCollision: process(clk)
	begin
		if rising_edge(clk) then			
			if resetIMBC = '1' then
				IMBC <= '0';
			end if;

			if (cs = '1')
			and (rd = '1')
			and	(addr = "011111") then
				M2D <= (others => '0');
				M2Dhit <= '0';
			end if;

			for i in 0 to 7 loop
				if McurrentPixels(i)(0) = '1'
				and foregroundBits(2+to_integer(xscroll)) = '1'
				and (TBBorder = '0') then
					M2D(i) <= '1';
					
					-- Give collision interrupt but only once until clear of register
					if M2Dhit = '0' then
						IMBC <= '1';
						M2Dhit <= '1';
					end if;
				end if;
			end loop;
		end if;
	end process;

colorVideoGenerator: process(clk)
		variable myColor: unsigned(3 downto 0);
	begin
		if rising_edge(clk) then			
			myColor := pixelColorStore(2+to_integer(xscroll));
			for i in 7 downto 0 loop
				if (MPRIO(i) = '0') or (foregroundBits(2+to_integer(xscroll)) = '0') then
					if MC(i) = '1' then
						case McurrentPixels(i) is
						when "01" => myColor := MM0;
						when "10" => myColor := spriteColors(i);
						when "11" => myColor := MM1;
						when others => null;
						end case;					
					else
						if (Moddeven(i) = '0' and McurrentPixels(i)(0) = '1')
						or (Moddeven(i) = '1' and McurrentPixels(i)(1) = '1') then
							myColor := spriteColors(i);
						end if;
					end if;					
				end if;
			end loop;
			
			if (LRBorder = '1') or (TBBorder = '1') then
				myColor := EC;
			end if;
			if (hBlack = '1') then
				myColor := (others => '0');
			end if;

			colorIndex <= myColor;
			hsync <= hBlanking;
			vsync <= vBlanking;
		
		end if;
	end process;

	registers: process(clk)
	begin
		if rising_edge(clk) then
			resetLightPenIrq <= '0';
			resetIMMC <= '0';
			resetIMBC <= '0';
			resetRasterIrq <= '0';
		
			--
			-- write to registers
			if (cs = '1') and (we = '1') then
				case addr is
				when "000000" => MX(0)(7 downto 0) <= di;
				when "000001" => MY(0) <= di;
				when "000010" => MX(1)(7 downto 0) <= di;
				when "000011" => MY(1) <= di;
				when "000100" => MX(2)(7 downto 0) <= di;
				when "000101" => MY(2) <= di;
				when "000110" => MX(3)(7 downto 0) <= di;
				when "000111" => MY(3) <= di;
				when "001000" => MX(4)(7 downto 0) <= di;
				when "001001" => MY(4) <= di;
				when "001010" => MX(5)(7 downto 0) <= di;
				when "001011" => MY(5) <= di;
				when "001100" => MX(6)(7 downto 0) <= di;
				when "001101" => MY(6) <= di;
				when "001110" => MX(7)(7 downto 0) <= di;
				when "001111" => MY(7) <= di;
				when "010000" =>
					MX(0)(8) <= di(0);
					MX(1)(8) <= di(1);
					MX(2)(8) <= di(2);
					MX(3)(8) <= di(3);
					MX(4)(8) <= di(4);
					MX(5)(8) <= di(5);
					MX(6)(8) <= di(6);
					MX(7)(8) <= di(7);
				when "010001" =>
					rasterCmp(8) <= di(7);
					ECM <= di(6);
					BMM <= di(5);
					DEN <= di(4);
					RSEL <= di(3);
					yscroll <= di(2 downto 0);
				when "010010" =>
					rasterCmp(7 downto 0) <= di;
				when "010101" =>
					m(0).e <= di(0);
					m(1).e <= di(1);
					m(2).e <= di(2);
					m(3).e <= di(3);
					m(4).e <= di(4);
					m(5).e <= di(5);
					m(6).e <= di(6);
					m(7).e <= di(7);
				when "010110" =>
					RES <= di(5);
					MCM <= di(4);
					CSEL <= di(3);
					xscroll <= di(2 downto 0);

				when "010111" => MYE <= di;
				when "011000" =>
					VM <= di(7 downto 4);
					CB <= di(3 downto 1);
				when "011001" =>
					if di(3) = '1' then
						resetLightPenIrq <= '1';
					end if;
					if di(2) = '1' then
						resetIMMC <= '1';
					end if;
					if di(1) = '1' then
						resetIMBC <= '1';
					end if;
					if di(0) = '1' then
						resetRasterIrq <= '1';
					end if;
				when "011010" =>
					ELP <= di(3);
					EMMC <= di(2);
					EMBC <= di(1);
					ERST <= di(0);
				when "011011" => MPRIO <= di;
				when "011100" => MC <= di;
				when "011101" => MXE <= di;
				when "100000" => EC <= di(3 downto 0);
				when "100001" => B0C <= di(3 downto 0);
				when "100010" => B1C <= di(3 downto 0);
				when "100011" => B2C <= di(3 downto 0);
				when "100100" => B3C <= di(3 downto 0);
				when "100101" => MM0 <= di(3 downto 0);
				when "100110" => MM1 <= di(3 downto 0);
				when "100111" => spriteColors(0) <= di(3 downto 0);
				when "101000" => spriteColors(1) <= di(3 downto 0);
				when "101001" => spriteColors(2) <= di(3 downto 0);
				when "101010" => spriteColors(3) <= di(3 downto 0);
				when "101011" => spriteColors(4) <= di(3 downto 0);
				when "101100" => spriteColors(5) <= di(3 downto 0);
				when "101101" => spriteColors(6) <= di(3 downto 0);
				when "101110" => spriteColors(7) <= di(3 downto 0);
				when others => null;
				end case;
			end if;
		end if;
	end process;

calcDo: process(clk)
	begin
		if rising_edge(clk) then
			case addr is
			when "000000" => do <= MX(0)(7 downto 0);
			when "000001" => do <= MY(0);
			when "000010" => do <= MX(1)(7 downto 0);
			when "000011" => do <= MY(1);
			when "000100" => do <= MX(2)(7 downto 0);
			when "000101" => do <= MY(2);
			when "000110" => do <= MX(3)(7 downto 0);
			when "000111" => do <= MY(3);
			when "001000" => do <= MX(4)(7 downto 0);
			when "001001" => do <= MY(4);
			when "001010" => do <= MX(5)(7 downto 0);
			when "001011" => do <= MY(5);
			when "001100" => do <= MX(6)(7 downto 0);
			when "001101" => do <= MY(6);
			when "001110" => do <= MX(7)(7 downto 0);
			when "001111" => do <= MY(7);
			when "010000" =>
				do <= MX(7)(8) & MX(6)(8) & MX(5)(8) & MX(4)(8)
					& MX(3)(8) & MX(2)(8) & MX(1)(8) & MX(0)(8);
			when "010001" => do <= rasterY(8) & ECM & BMM & DEN & RSEL & yscroll;
			when "010010" => do <= rasterY(7 downto 0);
			when "010011" => do <= lpX;
			when "010100" => do <= lpY;
			when "010101" => do <= m(7).e & m(6).e & m(5).e & m(4).e & m(3).e & m(2).e & m(1).e & m(0).e;
			when "010110" => do <= "11" & RES & MCM & CSEL & xscroll;
			when "010111" => do <= MYE;
			when "011000" => do <= VM & CB & '1';
			when "011001" => do <= IRQ & "111" & ILP & IMMC & IMBC & IRST;
			when "011010" => do <= "1111" & ELP & EMMC & EMBC & ERST;
			when "011011" => do <= MPRIO;
			when "011100" => do <= MC;
			when "011101" => do <= MXE;
			when "011110" => do <= M2M;
			when "011111" => do <= M2D;
			when "100000" => do <= "1111" & EC;
			when "100001" => do <= "1111" & B0C;
			when "100010" => do <= "1111" & B1C;
			when "100011" => do <= "1111" & B2C;
			when "100100" => do <= "1111" & B3C;
			when "100101" => do <= "1111" & MM0;
			when "100110" => do <= "1111" & MM1;
			when "100111" => do <= "1111" & spriteColors(0);
			when "101000" => do <= "1111" & spriteColors(1);
			when "101001" => do <= "1111" & spriteColors(2);
			when "101010" => do <= "1111" & spriteColors(3);
			when "101011" => do <= "1111" & spriteColors(4);
			when "101100" => do <= "1111" & spriteColors(5);
			when "101101" => do <= "1111" & spriteColors(6);
			when "101110" => do <= "1111" & spriteColors(7);
			when others => do <= (others => '1');
			end case;
		end if;
	end process;

	IRQ <= (ILP and ELP) or (IMMC and EMMC) or (IMBC and EMBC) or (IRST and ERST);
	irq_n <= not IRQ;
end rtl;

