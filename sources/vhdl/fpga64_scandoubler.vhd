library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.std_logic_unsigned.ALL;
use IEEE.numeric_std.all;

entity fpga64_scandoubler is
	generic (
		videoWidth : integer := 1
	);
	port (
		clk				: in std_logic;
		hSyncPolarity	: in std_logic := '0';
		vSyncPolarity	: in std_logic := '0';
		video_in		: in unsigned((videoWidth-1) downto 0);
		vsync_in		: in std_logic;
		hsync_in		: in std_logic;
		video_out		: out unsigned((videoWidth-1) downto 0);
		vsync_out		: out std_logic;
		hsync_out		: out std_logic
	);
end fpga64_scandoubler;

architecture rtl of fpga64_scandoubler is
signal reReadIndex : unsigned(9 downto 0) := (others => '0');
signal readIndex : unsigned(9 downto 0) := (others => '0');
signal writeIndex : unsigned(10 downto 0) := (others => '0');
signal oldHSync : std_logic := '0';
signal oldVSync : std_logic := '0';
signal vSyncCount : integer range 0 to 31;
signal divTwo : std_logic;
signal ramIn : unsigned(videoWidth downto 0);
signal ramOut : unsigned(videoWidth downto 0);
begin
	lineRam: entity work.fpga64_rwram
		generic map (
			dWidth => videoWidth+1,
			aWidth => 10
		)
		port map (
			clk => clk,
			we => '1',
			rAddr => readIndex,
			wAddr => writeIndex(10 downto 1),
			d => ramIn,
			q => ramOut
		);
	
	ramIn <= hsync_in & video_in;
	
	process(clk)
	begin
		if rising_edge(clk) then
			if divTwo = '0' then
				writeIndex <= writeIndex + 1;
				readIndex <= readIndex + 1;
				oldHSync <= hsync_in;

				if (oldHSync = '1') and (hsync_in = '0') then
					readIndex <= reReadIndex;
					reReadIndex <= writeIndex(10 downto 1);
					--writeIndex <= (others => '0');

					if (vsync_in = '1') and (oldVSync = '0') then
						vSyncCount <= 31;
					elsif vSyncCount /= 0 then
						vSyncCount <= vSyncCount - 1;
					end if;
					oldVSync <= vsync_in;
				end if;

				video_out <= ramOut((videoWidth-1) downto 0);
				hsync_out <= ramOut(videoWidth) xor (not hSyncPolarity);
				-- Video blank
				if (vSyncCount /= 0) and (vSyncCount < 16) then
					video_out <= "0000";
				end if;
			end if;
			if (vSyncCount = 9) or (vSyncCount = 10) then
				vsync_out <= vSyncPolarity;
			else
				vsync_out <= not vSyncPolarity;
			end if;
			divTwo <= not divTwo;
		end if;
	end process;
end architecture;
