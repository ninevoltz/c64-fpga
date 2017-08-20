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
-- 6526 Complex Interface Adapter
--
-- -----------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.ALL;

-- -----------------------------------------------------------------------

entity cia6526 is
	port (
		clk: in std_logic;
		todClk: in std_logic;
		reset: in std_logic;
		enable: in std_logic;
		cs: in std_logic;
		we: in std_logic; -- Write enable
		wr: in std_logic; -- Write strobe
		rd: in std_logic; -- Read strobe

		addr: in unsigned(3 downto 0);
		di: in unsigned(7 downto 0);
		do: out unsigned(7 downto 0);

		ppai: in unsigned(7 downto 0);
		ppao: out unsigned(7 downto 0);
		ppad: out unsigned(7 downto 0);

		ppbi: in unsigned(7 downto 0);
		ppbo: out unsigned(7 downto 0);
		ppbd: out unsigned(7 downto 0);

		flag_n: in std_logic;

		irq_n: out std_logic
	);
end cia6526;

-- -----------------------------------------------------------------------

architecture Behavioral of cia6526 is
	signal pra: unsigned(7 downto 0);
	signal prb: unsigned(7 downto 0);
	signal ddra: unsigned(7 downto 0);
	signal ddrb: unsigned(7 downto 0);

	signal talo: unsigned(7 downto 0) := (others => '1');
	signal tahi: unsigned(7 downto 0) := (others => '1');
	signal tblo: unsigned(7 downto 0) := (others => '1');
	signal tbhi: unsigned(7 downto 0) := (others => '1');

	signal timerA : unsigned(15 downto 0);
	signal timerADecr : unsigned(15 downto 0);
	signal forceTimerA : std_logic;
	signal loadTimerA : std_logic;
	signal clkTimerA : std_logic; -- 1 clock before timer input
	signal timerB: unsigned(15 downto 0);
	signal timerBDecr : unsigned(15 downto 0);
	signal forceTimerB : std_logic;
	signal loadTimerB : std_logic;
	signal clkTimerB : std_logic; -- 1 clock before timer input

	signal cra_start : std_logic;
	signal cra_pbon : std_logic;
	signal cra_outmode : std_logic;
	signal cra_runmode : std_logic;
	signal cra_runmodeLatch : std_logic;
	signal cra_inmode : std_logic;
	signal cra_spmode : std_logic;
	signal cra_todin : std_logic;
	
	signal crb_start : std_logic;
	signal crb_pbon : std_logic;
	signal crb_outmode : std_logic;
	signal crb_runmode : std_logic;
	signal crb_runmodeLatch : std_logic;
	signal crb_inmode5 : std_logic;
	signal crb_inmode6 : std_logic;
	signal crb_alarm : std_logic;

	-- TOD 50/60 hz clock
	signal oldTodClk : std_logic;
	signal tod_clkcnt: unsigned(2 downto 0);

	-- TOD counters
	signal tod_running: std_logic;
	signal tod_10ths: unsigned(3 downto 0);
	signal tod_secs: unsigned(6 downto 0);
	signal tod_mins: unsigned(6 downto 0);
	signal tod_hrs: unsigned(5 downto 0);
	
	-- TOD latches
	signal tod_latched: std_logic;
	signal tod_latch_10ths: unsigned(3 downto 0);
	signal tod_latch_secs: unsigned(6 downto 0);
	signal tod_latch_mins: unsigned(6 downto 0);
	signal tod_latch_hrs: unsigned(5 downto 0);
	signal tod_latch_pm: std_logic;

	-- Interrupt processing
	signal resetIrq : std_logic;
	signal intr_flags: unsigned(4 downto 0);
	signal intr_mask: unsigned(4 downto 0);
	signal ir: std_logic;

	signal prevFlag_n: std_logic;

begin
	-- I/O ports
	process(pra, prb, ddra, ddrb)
	begin
		ppad <= ddra;
		ppao <= pra or (not ddra);
		ppbd <= ddrb;
		ppbo <= prb or (not ddrb);
	end process;
	
	-- TOD - time of day
	process(clk)
		variable new_10ths : unsigned(3 downto 0);
		variable new_secs : unsigned(6 downto 0);
		variable new_mins : unsigned(6 downto 0);
		variable new_hrs : unsigned(5 downto 0);
	begin
		if rising_edge(clk) then
			new_10ths := tod_10ths;
			new_secs := tod_secs;
			new_mins := tod_mins;
			new_hrs := tod_hrs;

			if (todClk = '1')
			and (oldTodClk = '0') then
				if tod_clkcnt /= "000" then
					tod_clkcnt <= tod_clkcnt - 1;
				elsif tod_running = '1' then
					if cra_todin = '1' then
						tod_clkcnt <= "100"; -- 50 Hz
					else
						tod_clkcnt <= "101"; -- 60 Hz
					end if;
					if new_10ths /= "1001" then
						new_10ths := new_10ths + 1;
					else
						new_10ths := "0000";
						if new_secs /= "111011" then
							new_secs := new_secs + 1;
						else
							new_secs := "0000000";
							if new_mins /= "111011" then
								new_mins := new_mins + 1;
							else
								new_mins := "0000000";
							end if;
						end if;
					end if;
				end if;
			end if;

			if (cs = '1')
			and (wr = '1')
			and (crb_alarm = '0') then
				case addr is
				when X"8" =>
					new_10ths := di(3 downto 0);
					tod_running <= '1';
				when X"9" =>
					new_secs := di(6 downto 0);
				when X"A" =>
					new_mins := di(6 downto 0);
				when X"B" =>
					new_hrs := di(5 downto 0);
					tod_running <= '0';
				when others =>
					null;
				end case;
			end if;

			tod_10ths <= new_10ths;
			tod_secs <= new_secs;
			tod_mins <= new_mins;
			tod_hrs <= new_hrs;
			if tod_latched = '0' then
				tod_latch_10ths <= new_10ths;
				tod_latch_secs <= new_secs;
				tod_latch_mins <= new_mins;
				tod_latch_hrs <= new_hrs;
			end if;
			oldTodClk <= todClk;
			
			if reset = '1' then
				tod_running <= '0';
			end if;
		end if;		
	end process;

	-- timers runmode
	-- Using the runmode is tricky as in a real CIA there is both a
	-- latch and a register in use followed by a OR port.
	-- Although we use CLK here, from emulation point of view it is 
	-- the direct value of the runmode latch. As we have 3 CLK cycles
	-- to prepare it.
	process(clk)
	begin
		if rising_edge(clk) then
			cra_runmodeLatch <= cra_runmode;
			if we = '1'
			and cs = '1'
			and addr = X"E" then
				cra_runmodeLatch <= di(3);
			end if;

			crb_runmodeLatch <= crb_runmode;
			if we = '1'
			and cs = '1'
			and addr = X"F" then
				crb_runmodeLatch <= di(3);
			end if;
		end if;
	end process;

	process(clk)
		variable ciaInput : std_logic;
		variable underflowA : std_logic;
	begin		
		if rising_edge(clk) then
			loadTimerA <= '0';
			loadTimerB <= '0';
			if reset = '1' then
				cra_start <= '0';
				crb_start <= '0';
				intr_flags <= (others => '0');
				intr_mask <= (others => '0');				
				ir <= '0';
			else

				if enable = '1' then
					--
					-- process timer A
					--
					underflowA := '0';
					-- CNT is not emulated so don't count when inmode = 1
					clkTimerA <= cra_start and (not cra_inmode);
					if clkTimerA = '1' then
						if timera = "00000000" then
							intr_flags(0) <= '1';
							loadTimerA <= '1';
							cra_start <= not (cra_runmode or cra_runmodeLatch);
							underflowA := '1';
						else
							timera <= timera - 1;
						end if;
					end if;
					if forceTimerA = '1' then
						loadTimerA <= '1';
						clkTimerA <= '0';
					end if;

					--
					-- process timer B
					--
					if crb_inmode6 = '1' then
						-- count timerA underflows
						ciaInput := underflowA;
					elsif crb_inmode5 = '0' then
						-- count clock pulses
						ciaInput := '1'; 
					else
						-- CNT is not emulated so don't count
						ciaInput := '0';
					end if;
					clkTimerB <= ciaInput and crb_start;
					if clkTimerB = '1' then
						if timerb = "00000000" then
							intr_flags(1) <= '1';
							loadTimerB <= '1';
							crb_start <= not (crb_runmode or crb_runmodeLatch);
						else
							timerb <= timerb - 1;
						end if;
					end if;
					
					if forceTimerB = '1' then
						loadTimerB <= '1';
						clkTimerB <= '0';
					end if;
				end if;
				if loadTimerA = '1' then
					timerA <= tahi & talo;
				end if;
				if loadTimerB = '1' then
					timerB <= tbhi & tblo;
				end if;

				if cs = '1' and wr = '1' then
					case addr is
					when X"5" =>
						if cra_start = '0' then
							loadTimerA <= '1';
						end if;
					when X"7" =>
						if crb_start = '0' then
							loadTimerB <= '1';
						end if;
					when X"D" =>
						if di(7) ='0' then
							intr_mask <= intr_mask and (not di(4 downto 0));
						else
							intr_mask <= intr_mask or di(4 downto 0);
						end if;
					when X"E" =>
						cra_start <= di(0);
					when X"F" =>
						crb_start <= di(0);
					when others => null;
					end case;
				end if;

				if (flag_n = '0') and (prevFlag_n = '1') then
					intr_flags(4) <= '1';
				end if;

				
				ir <= ir
				or (intr_flags(4) and intr_mask(4))
				or (intr_flags(3) and intr_mask(3))
				or (intr_flags(2) and intr_mask(2))
				or (intr_flags(1) and intr_mask(1))
				or (intr_flags(0) and intr_mask(0));
			end if;
			if resetIrq = '1' then
				intr_flags <= (others => '0');
				ir <= '0';
			end if;

			prevFlag_n <= flag_n;
		end if;
	end process;

-- -----------------------------------------------------------------------
-- Write registers
-- -----------------------------------------------------------------------
	process(clk)
	begin
		if rising_edge(clk) then
			resetIrq <= '0';
			if enable = '1' then
				forceTimerA <= '0';
				forceTimerB <= '0';
			end if;
			if cs = '1' then
				if wr = '1' then
					case addr is
					when X"0" => pra <= di;
					when X"1" => prb <= di;
					when X"2" => ddra <= di;
					when X"3" => ddrb <= di;
					when X"4" => talo <= di;
					when X"5" => tahi <= di;
					when X"6" => tblo <= di;
					when X"7" => tbhi <= di;
					when X"E" =>
						cra_pbon <= di(1);
						cra_outmode <= di(2);
						cra_runmode <= di(3);
						forceTimerA <= di(4);
						cra_inmode <= di(5);
						cra_spmode <= di(6);
						cra_todin <= di(7);
					when X"F" =>
						crb_pbon <= di(1);
						crb_outmode <= di(2);
						crb_runmode <= di(3);
						forceTimerB <= di(4);
						crb_inmode5 <= di(5);
						crb_inmode6 <= di(6);
						crb_alarm <= di(7);
					when others => null;
					end case;
				end if;
				if rd = '1' then
					case addr is
					when X"8" => tod_latched <= '0';
					when X"B" => tod_latched <= '1';
					when X"D" => resetIrq <= '1';
					when others => null;
					end case;
				end if;
			end if;
			if reset = '1' then
				pra <= (others => '0');
				prb <= (others => '0');
				ddra <= (others => '0');
				ddrb <= (others => '0');
				cra_pbon <= '0';
				cra_outmode <= '0';
				cra_runmode <= '0';
				cra_inmode <= '0';
				cra_spmode <= '0';
				cra_todin <= '0';
				crb_pbon <= '0';
				crb_outmode <= '0';
				crb_runmode <= '0';
				crb_inmode5 <= '0';
				crb_inmode6 <= '0';
				crb_alarm <= '0';
				tod_latched <= '0';
			end if;
		end if;
	end process;

-- -----------------------------------------------------------------------
-- Read registers
-- -----------------------------------------------------------------------
	process(clk)
	begin
		if rising_edge(clk) then
			case addr is
			when X"0" => do <= ppai;
			when X"1" => do <= ppbi;
			when X"2" => do <= DDRA;
			when X"3" => do <= DDRB;
			when X"4" => do <= timera(7 downto 0);
			when X"5" => do <= timera(15 downto 8);
			when X"6" => do <= timerb(7 downto 0);
			when X"7" => do <= timerb(15 downto 8);
			when X"8" => do <= "0000" & tod_latch_10ths;
			when X"9" => do <= "0" & tod_latch_secs;
			when X"A" => do <= "0" & tod_latch_mins;
			when X"B" => do <= (others => '1');
			when X"C" => do <= (others => '1');
			when X"D" => do <= ir & '0' & '0' & intr_flags;
			when X"E" => do <= cra_todin & cra_spmode & cra_inmode & '0' & cra_runmode & cra_outmode & cra_pbon & cra_start;
			when X"F" => do <= crb_alarm & crb_inmode6 & crb_inmode5 & '0' & crb_runmode & crb_outmode & crb_pbon & crb_start;
			when others => do <= (others => '-');
			end case;
		end if;
	end process;

	irq_n <= not(ir);
end Behavioral;
