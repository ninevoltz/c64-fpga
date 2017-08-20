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
-- Simple dual port ram: One read and one write port
--
-- -----------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_unsigned.ALL;
use IEEE.numeric_std.all;

entity fpga64_singleram is
	generic (
		dWidth : integer := 1;
		aWidth : integer := 10
	);
	port (
		clk : in std_logic;
		we : in std_logic;
		addr : in unsigned((aWidth-1) downto 0);
		d : in unsigned((dWidth-1) downto 0);
		q : out unsigned((dWidth-1) downto 0)
	);
end fpga64_singleram;

architecture rtl of fpga64_singleram is
	subtype addressRange is integer range 0 to ((2**aWidth)-1);
	type ramDef is array(addressRange) of unsigned((dWidth-1) downto 0);
	signal ram: ramDef;
	
	signal myAddr : addressRange;
	signal myAddrReg : addressRange;
	signal ramDo : unsigned((dWidth-1) downto 0);
begin
	-- Ram with one read and one write port
	-- Not write-through
	process(clk)
	begin
		if rising_edge(clk) then
			if we = '1' then
				ram(myAddr) <= d;
			end if;
			ramDo <= ram(myAddrReg);
			myAddrReg <= myAddr;
		end if;
	end process;

	-- Interface
	myAddr <= to_integer(addr);
	q <= ramDo;
end architecture;

		
