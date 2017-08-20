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
library IEEE;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

entity fpga64_buslogic_roms is
	port (
		clk: in std_logic;
		reset: in std_logic;
		ena : in std_logic;
		busCycle: in unsigned(5 downto 0);
		cpuGetsBus : in std_logic;

		ramData: in unsigned(7 downto 0);

		-- 2 CHAREN
		-- 1 HIRAM
		-- 0 LORAM
		bankSwitch: in unsigned(2 downto 0);

		cpuWe: in std_logic;
		cpuAddr: in unsigned(15 downto 0);
		cpuData: in unsigned(7 downto 0);
		vicAddr: in unsigned(15 downto 0);
		vicData: in unsigned(7 downto 0);
		sidData: in unsigned(7 downto 0);
		colorData: in unsigned(3 downto 0);
		cia1Data: in unsigned(7 downto 0);
		cia2Data: in unsigned(7 downto 0);
		lastVicData : in unsigned(7 downto 0);

		systemWe: out std_logic;
		pulseWr: out std_logic;
		systemAddr: out unsigned(15 downto 0);
		dataToCpu : out unsigned(7 downto 0);
		dataToVic : out unsigned(7 downto 0);

		cs_vic: out std_logic;
		cs_sid: out std_logic;
		cs_color : out std_logic;
		cs_cia1: out std_logic;
		cs_cia2: out std_logic;
		cs_ram: out std_logic
	);
end fpga64_buslogic_roms;

-- -----------------------------------------------------------------------

architecture Behavioral of fpga64_buslogic_roms is
	component fpga64_colorram is
		port (
			clk: in std_logic;
			cs: in std_logic;
			we: in std_logic;

			addr: in unsigned(9 downto 0);
			di: in unsigned(3 downto 0);
			do: out unsigned(3 downto 0)
		);
	end component;

	signal charData: unsigned(7 downto 0);
	signal basicData: unsigned(7 downto 0);
	signal kernalData: unsigned(7 downto 0);

	signal cs_CharReg : std_logic;
	signal cs_BasicReg : std_logic;
	signal cs_KernalReg : std_logic;
	signal vicCharReg : std_logic;

	signal cs_ramReg : std_logic;
	signal cs_vicReg : std_logic;
	signal cs_sidReg : std_logic;
	signal cs_colorReg : std_logic;
	signal cs_cia1Reg : std_logic;
	signal cs_cia2Reg : std_logic;

	signal currentAddr: unsigned(15 downto 0);
	signal cpuHasBus : std_logic; -- Hacky hack
begin
	charrom: entity work.c64rom_chargen
		port map (
			clk => clk,
			addr => currentAddr(11 downto 0),
			do => charData
		);

	basicrom: entity work.c64rom_basic
		port map (
			clk => clk,
			addr => cpuAddr(12 downto 0),
			do => basicData
		);

	kernelrom: entity work.c64rom_kernal
		port map (
			clk => clk,
			addr => cpuAddr(12 downto 0),
			do => kernalData
		);
	
	--
	-- write pulse runs during subCycle xxx10. 
	generatePulseWe: process(clk, busCycle, cpuWe)
	begin
		if rising_edge(clk) then
			pulseWr <= '0';
-- !!! Need to go into toplevel not buslogic
			if (busCycle = "00101") and (cpuWe = '1') then
				-- Pulse WR halfway CPU access
				pulseWr <= '1';
			end if;
		end if;
	end process;
	
	process(ramData, vicData, sidData, colorData, cia1Data, cia2Data, cs_ramReg, cs_vicReg, cs_sidReg, cs_colorReg, cs_cia1Reg, cs_cia2Reg, lastVicData)
	begin
		-- If no hardware is addressed the bus is floating.
		-- It will contain the last data read by the VIC. (if a C64 is shielded correctly)
		dataToCpu <= lastVicData;
		if cs_CharReg = '1' then	
			dataToCpu <= charData;
		elsif cs_BasicReg = '1' then	
			dataToCpu <= basicData;
		elsif cs_KernalReg = '1' then	
			dataToCpu <= kernalData;	
		elsif cs_ramReg = '1' then
			dataToCpu <= ramData;
		elsif cs_vicReg = '1' then
			dataToCpu <= vicData;
		elsif cs_sidReg = '1' then
			dataToCpu <= sidData;
		elsif cs_colorReg = '1' then
			dataToCpu <= "1111" & colorData;
		elsif cs_cia1Reg = '1' then
			dataToCpu <= cia1Data;
		elsif cs_cia2Reg = '1' then
			dataToCpu <= cia2Data;
		end if;
	end process;

	-- !!! hacky hack
	process(clk)
	begin
		if rising_edge(clk) then
			if ena = '1' then
				cpuHasBus <= cpuGetsBus;
			end if;
		end if;
	end process;

	process(clk)
	begin
		if rising_edge(clk) then
			currentAddr <= (others => '1'); -- Prevent generation of a latch when neither vic or cpu is using the bus.
			systemWe <= '0';
			vicCharReg <= '0';
			cs_charReg <= '0';
			cs_basicReg <= '0';
			cs_kernalReg <= '0';
			cs_ramReg <= '0';
			cs_vicReg <= '0';
			cs_sidReg <= '0';
			cs_colorReg <= '0';
			cs_cia1Reg <= '0';
			cs_cia2Reg <= '0';

			if (cpuHasBus = '1') then
				-- The 6502 CPU has the bus.
				currentAddr <= cpuAddr;
				case cpuAddr(15 downto 12) is
				when X"E" | X"F" =>
					if (cpuWe = '0') and (bankSwitch(1) = '1') then
						-- Read kernal
						cs_kernalReg <= '1';
					else
						-- 64Kbyte RAM layout
						cs_ramReg <= '1';
					end if;
				when X"D" =>
					if (bankSwitch(1) = '0') and (bankSwitch(0) = '0') then
						-- 64Kbyte RAM layout
						cs_ramReg <= '1';
					elsif bankSwitch(2) = '1' then
						case cpuAddr(11 downto 8) is
							when X"0" | X"1" | X"2" | X"3" =>
								cs_vicReg <= '1';
							when X"4" | X"5" | X"6" | X"7" =>
								cs_sidReg <= '1';
							when X"8" | X"9" | X"A" | X"B" =>
								cs_colorReg <= '1';
							when X"C" =>
								cs_cia1Reg <= '1';
							when X"D" =>
								cs_cia2Reg <= '1';
							when others =>
								null;
						end case;						
					else
						-- I/O space turned off. Read from charrom or write to RAM.
						if cpuWe = '0' then
							cs_charReg <= '1';
						else
							cs_ramReg <= '1';
						end if;
					end if;
				when X"A" | X"B" =>
					if (cpuWe = '0') and (bankSwitch(1) = '1') and (bankSwitch(0) = '1') then
						cs_basicReg <= '1';
					else
						cs_ramReg <= '1';
					end if;
				when others =>
					cs_ramReg <= '1';
				end case;

				systemWe <= cpuWe;
			else
				-- The VIC-II has the bus.
				currentAddr <= vicAddr;
				if vicAddr(14 downto 12)="001" then
					vicCharReg <= '1';
				else
					cs_ramReg <= '1';
				end if;
			end if;
		end if;
	end process;
	
	cs_ram <= cs_ramReg;
	cs_vic <= cs_vicReg;
	cs_sid <= cs_sidReg;
	cs_color <= cs_colorReg;
	cs_cia1 <= cs_cia1Reg;
	cs_cia2 <= cs_cia2Reg;

	process(ramData, charData, vicCharReg)
	begin
		if (cpuWe = '1') and (cpuHasBus = '1') then
			dataToVic <= cpuData;
		elsif vicCharReg = '1' then
			dataToVic <= charData;
		else
			dataToVic <= ramData;
		end if;
	end process;

	systemAddr <= currentAddr;
end Behavioral;

