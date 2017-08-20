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
-- -----------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_unsigned.ALL;
use IEEE.numeric_std.all;

-- -----------------------------------------------------------------------

entity fpga64_pipistrello is
	generic (
		resetCycles: integer := 15
	);
	port (
		clka : in std_logic;

		ps2_clk : in std_logic;
		ps2_data : in std_logic;

		vga_blue : out unsigned(2 downto 0);
		vga_green : out unsigned(2 downto 0);
		vga_red : out unsigned(2 downto 0);
		vga_hsync_n : out std_logic;
		vga_vsync_n : out std_logic;

		i_rst : in std_logic;
		tmds : out std_logic_vector(3 downto 0);
		tmdsb : out std_logic_vector(3 downto 0)
		--sw : in unsigned(3 downto 2);

		--proto : inout unsigned(84 downto 0)

	);
end entity;

architecture Behavioral of fpga64_pipistrello is

signal clk32 : std_logic;
signal endOfCycle : std_logic;
signal busCycle: unsigned(5 downto 0);
signal lastCycleVic : std_logic;
signal phi0_cpu : std_logic;
signal phi0_vic : std_logic;

	signal ba: std_logic;
	signal irq_n: std_logic;
	signal nmi_n: std_logic;

	signal cpuGetsBus : std_logic;
	signal enableCpu: std_logic;
	signal enableVic : std_logic;
	signal enableBus : std_logic;

	signal irq_cia1: std_logic;
	signal irq_cia2: std_logic;
	signal irq_vic: std_logic;

	signal systemWe: std_logic;
	signal pulseWrRam: std_logic;
	signal pulseWrIo: std_logic;
	signal pulseRd: std_logic;
	signal colorWe : std_logic;
	signal systemAddr: unsigned(15 downto 0);

	signal cs_vic: std_logic;
	signal cs_sid: std_logic;
	signal cs_color: std_logic;
	signal cs_cia1: std_logic;
	signal cs_cia2: std_logic;
	signal cs_ram: std_logic;
	signal cselWe: std_logic;

	signal reset: std_logic := '1';
	signal reset_cnt: integer range 0 to resetCycles := 0;

-- CIA signals
signal enableCia : std_logic;
signal cia1Do: unsigned(7 downto 0);
signal cia2Do: unsigned(7 downto 0);

-- keyboard
signal newScanCode: std_logic;
signal theScanCode: unsigned(7 downto 0);

-- I/O
signal cia1_pai: unsigned(7 downto 0);
signal cia1_pao: unsigned(7 downto 0);
signal cia1_pad: unsigned(7 downto 0);
signal cia1_pbi: unsigned(7 downto 0);
signal cia1_pbo: unsigned(7 downto 0);
signal cia1_pbd: unsigned(7 downto 0);
signal cia2_pai: unsigned(7 downto 0);
signal cia2_pao: unsigned(7 downto 0);
signal cia2_pad: unsigned(7 downto 0);
signal cia2_pbi: unsigned(7 downto 0);
signal cia2_pbo: unsigned(7 downto 0);
signal cia2_pbd: unsigned(7 downto 0);

	signal debugWE: std_logic := '0';
	signal debugData: unsigned(7 downto 0) := (others => '0');
	signal debugAddr: integer range 2047 downto 0 := 0;

	signal cpuWe: std_logic;
	signal cpuAddr: unsigned(15 downto 0);
	signal cpuDi: unsigned(7 downto 0);
	signal cpuDo: unsigned(7 downto 0);
	signal cpuIO: unsigned(7 downto 0);

	signal vicDi: unsigned(7 downto 0);
	signal vicAddr: unsigned(15 downto 0);
	signal vicData: unsigned(7 downto 0);
	signal lastVicDi : unsigned(7 downto 0);

	signal sidWriteData: unsigned(7 downto 0);
	signal sidData: unsigned(7 downto 0);
	signal sidRandom : unsigned(7 downto 0);
	
	signal colorData : unsigned(3 downto 0);

	signal cpuDebugOpcode: unsigned(7 downto 0);
	signal cpuDebugPc: unsigned(15 downto 0);
	signal cpuDebugA: unsigned(7 downto 0);
	signal cpuDebugX: unsigned(7 downto 0);
	signal cpuDebugY: unsigned(7 downto 0);
	signal cpuDebugS: unsigned(7 downto 0);
	signal cpuStep : std_logic;
	signal traceKey : std_logic;
	signal trace2Key : std_logic;

-- video
signal vicColorIndex : unsigned(3 downto 0);
signal vicHSync : std_logic;
signal vicVSync : std_logic;
signal blank : std_logic;

signal vgaColorIndex : unsigned(3 downto 0);
signal vgaR : unsigned(7 downto 0);
signal vgaG : unsigned(7 downto 0);
signal vgaB : unsigned(7 downto 0);
signal vgaVSync : std_logic;
signal vgaHSync : std_logic;
signal vgaDebug : std_logic;
signal vgaDebugDim : std_logic;
signal debuggerOn : std_logic;	
signal traceStep : std_logic;
	
-- config
signal videoKey : std_logic;
signal ntscMode : std_logic;
signal hSyncPolarity : std_logic;
signal vSyncPolarity : std_logic;

signal videoConfigVideo : std_logic;
signal videoConfigDim : std_logic;
signal videoConfigShow : std_logic;
signal videoConfigTimeout : unsigned(19 downto 0);

type testMainRamDef is array(16383 downto 0) of unsigned(7 downto 0);
signal testMainRam: testMainRamDef;
signal ramData : unsigned(7 downto 0);
	
begin

	myDcm: entity work.fpga64_dcm100mhz
		port map (
			xtal => clkA,
			clk100out => open,
			clk32out => clk32
		);

	 dvi_video : entity work.vga_to_dvi
		port map (
			I_RESET    => i_rst,
			I_CLK      => clka,
			I_VGA_R    => std_logic_vector(vgaR),
			I_VGA_G    => std_logic_vector(vgaG),
			I_VGA_B    => std_logic_vector(vgaB),
			I_HSYNC    => vgaHSync,
			I_VSYNC    => vgaVSync,
			I_BLANK    => blank,
			O_TMDS     => tmds,
			O_TMDSB    => tmdsb
		);
		
	fpga64_busTiming_instance : entity work.fpga64_busTiming
		generic map (
			resetCycles => 15,
			noofBusCycles => 32
--			noofBusCycles => 34
		)
		port map (
			clkIn => clk32,
			rstIn => '0',
			rstOut => open,
			endOfCycle => endOfCycle,
			busCycle => busCycle
		);

	myScanDoubler: entity work.fpga64_scandoubler
		generic map (
			videoWidth => 4
		)
		port map (
			clk => clk32,
			hSyncPolarity => hSyncPolarity,
			vSyncPolarity => vSyncPolarity,
			video_in => vicColorIndex,
			hsync_in => vicHSync,
			vsync_in => vicVSync,
			video_out => vgaColorIndex,
			hsync_out => vgaHSync,
			vsync_out => vgaVSync
		);
	
	c64colors: entity work.fpga64_rgbcolor
		port map (
			index => vgaColorIndex,
			r => vgaR,
			g => vgaG,
			b => vgaB
		);

	colorram: entity work.fpga64_singleram
		generic map (
			dWidth => 4,
			aWidth => 10
		)
		port map (
			clk => clk32,
			we => colorWe,
			addr => systemAddr(9 downto 0),
			d => cpuDo(3 downto 0),
			q => colorData
		);
	colorWe <= (cs_color and pulseWrRam);

	buslogic: entity work.fpga64_buslogic_roms
		port map (
			clk => clk32,
			reset => reset,
			ena => enableBus,
			busCycle => busCycle,
			cpuGetsBus => cpuGetsBus,

			bankSwitch => cpuIO(2 downto 0),

			ramData => ramData,

			cpuWe => cpuWe,
			cpuAddr => cpuAddr,
			cpuData => cpuDo,
			vicAddr => vicAddr,
			vicData => vicData,
			sidData => sidData,
			colorData => colorData,
			cia1Data => cia1Do,
			cia2Data => cia2Do,
			lastVicData => lastVicDi,

			systemWe => systemWe,
			pulseWr => pulseWrRam,
			systemAddr => systemAddr,
			dataToCpu => cpuDi,
			dataToVic => vicDi,

			cs_vic => cs_vic,
			cs_sid => cs_sid,
			cs_color => cs_color,
			cs_cia1 => cs_cia1,
			cs_cia2 => cs_cia2,
			cs_ram => cs_ram
		);

	vic: entity work.vicii_6567_6569
		generic map (
			graphicsEnable => '1'
		)
		port map (
			clk => clk32,
			enable => enableVic,
			endOfCycle => lastCycleVic,
			phi0_cpu => phi0_cpu,
			phi0_vic => phi0_vic,			
			mode6569 => (not ntscMode),
			mode6567old => '0',
			mode6567R8 => ntscMode,
			
			cs => cs_vic,
			we => pulseWrIo,
			rd => pulseRd,
			lp_n => cia1_pbi(4),

			addr => cpuAddr(5 downto 0),
			di => vicDi,
			diColor => colorData,
			do => vicData,

			ba => ba,
			vicAddr => vicAddr(13 downto 0),
--			vid_di => (others => '0'),
			
			hsync => vicHSync,
			vsync => vicVSync,
			colorIndex => vicColorIndex,

			irq_n => irq_vic
		);

	cia1: entity work.cia6526
		port map (
			clk => clk32,
			todClk => vicVSync,
			reset => reset,
			enable => enableCia,
			cs => cs_cia1,
			we => cpuWe,
			wr => pulseWrIo,
			rd => pulseRd,

			addr => cpuAddr(3 downto 0),
			di => cpuDo,
			do => cia1Do,

			ppai => cia1_pai,
			ppao => cia1_pao,
			ppbi => cia1_pbi,
			ppbo => cia1_pbo,

			flag_n => '1',

			irq_n => irq_cia1
		);

	cia2: entity work.cia6526
		port map (
			clk => clk32,
			todClk => vicVSync,
			reset => reset,
			enable => enableCia,
			cs => cs_cia2,
			we => cpuWe,
			wr => pulseWrIo,
			rd => pulseRd,

			addr => cpuAddr(3 downto 0),
			di => cpuDo,
			do => cia2Do,

			ppai => cia2_pai,
			ppao => cia2_pao,
			ppbi => cia2_pbi,
			ppbo => cia2_pbo,

			flag_n => '1',

			irq_n => irq_cia2
		);

	cpu: entity work.cpu_6510
		port map (
			clk => clk32,
			reset => reset,
			enable => enableCpu,
			nmi_n => nmi_n,
			irq_n => irq_n,

			di => cpuDi,
			addr => cpuAddr,
			do => cpuDo,
			we => cpuWe,
			
			diIO => "00010111",
			doIO => cpuIO,

			debugOpcode => cpuDebugOpcode,
			debugPc => cpuDebugPc,
			debugA => cpuDebugA,			
			debugX => cpuDebugX,			
			debugY => cpuDebugY,			
			debugS => cpuDebugS
		);

	myKeyboard: entity work.io_ps2_keyboard
		port map (
			clk => clk32,
			kbd_clk => ps2_clk,
			kbd_dat => ps2_data,
			interrupt => newScanCode,
			scanCode => theScanCode
		);

	myKeyboardMatrix: entity work.fpga64_keyboard_matrix
		port map (
			clk => clk32,
			theScanCode => theScanCode,
			newScanCode => newScanCode,

			joyA => "11111", -- (not joyA(4 downto 0)),
			joyB => "11111", -- (not joyB(4 downto 0)),
			pai => cia1_pao,
			pbi => cia1_pbo,
			pao => cia1_pai,
			pbo => cia1_pbi,
			
			videoKey => videoKey,
			traceKey => traceKey,
			trace2Key => trace2Key,

			backwardsReadingEnabled => '0'
		);

	-- Reset, clock and bus timing
	process(clk32)
	begin
		if rising_edge(clk32) then
			if endOfCycle = '1' then
				if reset_cnt = resetCycles then
					reset <= '0';
				else
					reset <= '1';
					reset_cnt <= reset_cnt + 1;
				end if;
			end if;
			if i_rst = '1' then
				reset_cnt <= 0;
			end if;
		end if;
	end process;


	-- Video modes
	process(clk32)
	begin
		if rising_edge(clk32) then
			if videoKey = '1' then
				ntscMode <= not ntscMode;
				if ntscMode = '1' then
					hSyncPolarity <= not hSyncPolarity;
					if hSyncPolarity = '1' then
						vSyncPolarity <= not vSyncPolarity;
					end if;
				end if;
				videoConfigTimeout <= (others => '1');
			end if;
			if endOfCycle = '1' then
				videoConfigShow <= '0';
				if videoConfigTimeout /= 0 then
					videoConfigTimeout <= videoConfigTimeout - 1;
					videoConfigShow <= '1';
				end if;
			end if;				
		end if;
	end process;
	
	displayVideoConfig: entity work.fpga64_hexy_vmode
		generic map (
			xoffset => 200
		)
		port map (
			clk => clk32,
			vSync => vgaVSync,
			hSync => vgaHSync,
			video => videoConfigVideo,
			dim => videoConfigDim,
			ntscMode => ntscMode,
			hSyncPolarity => hSyncPolarity,
			vSyncPolarity => vSyncPolarity
		);

	mainMemoryBus: process(clk32)
	begin
		if rising_edge(clk32) then
			if pulseWrRam = '1' and cs_ram = '1' then
				testMainRam(to_integer(systemAddr(13 downto 0))) <= cpuDo;
			end if;
			ramData <= testMainRam(to_integer(systemAddr(13 downto 0)));
		end if;
	end process;

	-- Emulate reading SID register $1B
	process(clk32)
	begin
		if rising_edge(clk32) then
			if cpuAddr(4 downto 0) = "11011" then
				sidData <= sidRandom;
			else
				sidData <= (others => '0');
			end if;
			if enableCpu = '1' then
				sidRandom <= sidRandom + 17; -- Emulate RND number
			end if;
		end if;
	end process;

	process(clk32)
	begin
		if rising_edge(clk32) then
			if phi0_vic = '1' then
				lastVicDi <= vicDi;
			end if;
		end if;
	end process;

	process(clk32)
	begin
		if rising_edge(clk32) then
			lastCycleVic <= '0';
			enableVic <= '0';
			enableCpu <= '0';
			enableCia<= '0';
			enableBus <= '0';
			pulseWrIo <= '0';
			pulseRd <= '0';
			if busCycle = "011110" then
				lastCycleVic <= '1';
			end if;
			if busCycle(5) = '0' then
				if busCycle(1 downto 0) = "10" then
					enableVic <= '1';
					enableBus <= '1';
					if phi0_cpu = '1' then
						enableCia <= '1';
						if (ba = '1') or (cpuWe = '1') then
							enableCpu <= '1';
						end if;
						if cpuWe = '1' then
							pulseWrIo <= '1';
						else
							pulseRd <= '1';
						end if;
					end if;
				end if;
			end if;				
			if busCycle(1 downto 0) = "11" then
				phi0_vic <= '0';
				phi0_cpu <= '0';
				if phi0_vic = '1' then
					phi0_cpu <= '1';
				end if;
			end if;
			if endOfCycle = '1' then
				phi0_vic <= '1';
			end if;
		end if;
	end process;
	
	process(clk32)
	begin
		if rising_edge(clk32) then
			if trace2Key = '1' then
				debuggerOn <= not debuggerOn;
			end if;
		end if;
	end process;

	process(clk32)
	begin
		if rising_edge(clk32) then
			vga_red <= vgaR(7 downto 5);
			vga_green <= vgaG(7 downto 5);
			vga_blue <= vgaB(7 downto 5);
--			if videoConfigShow = '1' and videoConfigDim = '1' then
--			if videoConfigDim = '1' then
--				r <= videoConfigVideo & vgaR(7 downto 1);
--				g <= videoConfigVideo & vgaG(7 downto 1);
--				b <= videoConfigVideo & vgaB(7 downto 1);
--			end if;
			if vgaDebugDim = '1' then
				vga_red <= vgaDebug & vgaR(7 downto 6);
				vga_green <= vgaDebug & vgaG(7 downto 6);
				vga_blue <= vgaDebug & vgaB(7 downto 6);
			end if;				
		end if;
		if (vgaR = "00000000" and vgaG = "00000000" and vgaB = "00000000") then
	     blank <= '1';
	   else
	     blank <= '0';
	   end if;
	end process;
	vga_hsync_n <= vgaHSync;
	vga_vsync_n <= vgaVSync;
	

	
	hexyInstance : entity work.fpga64_hexy
		generic map (
			xoffset => 200,
			yoffset => 110
		)
		port map (
			clk => clk32,
			vSync => vgaVSync,
			hSync => vgaHSync,
			video => vgaDebug,
			dim => vgaDebugDim,
			
			spyAddr => cpuAddr,
			spyPc => cpuDebugPc,
			spyDo => cpuDo,
			spyOpcode => cpuDebugOpcode,
			spyA => cpuDebugA,
			spyX => cpuDebugX,
			spyY => cpuDebugY,
			spyS => cpuDebugS
		);

	cia2_pai(5 downto 0) <= cia2_pao(5 downto 0);
	cia2_pbi(7 downto 0) <= cia2_pbo;

	vicAddr(14) <= (not cia2_pao(0));
	vicAddr(15) <= (not cia2_pao(1));

	cpuGetsBus <= phi0_vic and (ba or cpuWe);

	irq_n <= irq_cia1 and irq_vic;
	nmi_n <= irq_cia2;
end architecture;
