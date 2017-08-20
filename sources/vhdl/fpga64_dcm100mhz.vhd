-- -----------------------------------------------------------------------
--
--                                 FPGA 64
--
--     A fully functional commodore 64 implementation in a single FPGA
--
-- -----------------------------------------------------------------------
-- Copyright 2005-2008 by Peter Wendrich (pwsoft@syntiac.com)
-- http://www.syntiac.com/fpga64.html
-- -----------------------------------------------------------------------
--
-- 100 Mhz clock divider for Xilinx FPGAs
-- 
-- -----------------------------------------------------------------------

library IEEE;
library UNISIM;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;
use UNISIM.VComponents.all;

-- -----------------------------------------------------------------------

entity fpga64_dcm100mhz is
	port (
		xtal: in std_logic;
		clk100out: out std_logic;
		clk32out: out std_logic
	);
end fpga64_dcm100mhz;

-- -----------------------------------------------------------------------

architecture rtl of fpga64_dcm100mhz is
	signal clk32: std_logic;
	signal clk100: std_logic;
	signal clk200: std_logic;
	signal clk32_dcm: std_logic;
	signal clk100_dcm: std_logic;
begin
	dcm_module : DCM
		generic map(
			CLKIN_PERIOD => 20.0,
			CLK_FEEDBACK => "1x",
			CLKFX_MULTIPLY => 12,
			CLKFX_DIVIDE => 19,
			DFS_FREQUENCY_MODE => "LOW",
			DLL_FREQUENCY_MODE => "LOW",
			STARTUP_WAIT => FALSE,
			DUTY_CYCLE_CORRECTION => TRUE
		)
		port map(
			CLKIN => xtal,
			CLK0 => clk100_dcm,
--			CLKFB => clk100,
			CLKFB => clk100_dcm,
			CLK2X => clk200,
			CLKFX => clk32_dcm
		);
	
	clk32buffer: BUFG
		port map(
			I => clk32_dcm,
			O => clk32
		);

	clk50buffer: BUFG
		port map(
			I => clk100_dcm,
			O => clk100
		);

	clk100out <= clk100;
	clk32out <= clk32;
end rtl;

