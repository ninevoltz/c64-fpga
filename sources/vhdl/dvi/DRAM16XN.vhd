--
-- Module:      DRAM16XN
--
-- Description: Distributed SelectRAM example
--              Dual Port 16 x N-bit
--
-- Device:      Spartan-3 Family
-----------------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;

library UNISIM;
use UNISIM.Vcomponents.all;

entity DRAM16XN is
  generic (
    data_width : integer := 20
    );
  port (
    DATA_IN       : in  std_logic_vector((data_width - 1) downto 0);
    ADDRESS       : in  std_logic_vector(3 downto 0);
    ADDRESS_DP    : in  std_logic_vector(3 downto 0);
    WRITE_EN      : in  std_logic;
    CLK           : in  std_logic;
    O_DATA_OUT_DP : out std_logic_vector((data_width - 1) downto 0);
    O_DATA_OUT    : out std_logic_vector((data_width - 1) downto 0)
    );
end entity;

architecture rtl of DRAM16XN is
begin

  dram16s : for i in 0 to (data_width - 1) generate
  begin
    i_RAM16X1D_U : RAM16X1D
      port map (
        A0    => ADDRESS(0),            -- insert Address 0 signal port SPO
        A1    => ADDRESS(1),            -- insert Address 1 signal port SPO
        A2    => ADDRESS(2),            -- insert Address 2 signal port SPO
        A3    => ADDRESS(3),            -- insert Address 3 signal port SPO
        D     => DATA_IN(i),            -- insert input signal
        DPO   => O_DATA_OUT_DP(i),      -- insert output signal DPO
        DPRA0 => ADDRESS_DP(0),     -- insert Address 0 signal dual port DPO
        DPRA1 => ADDRESS_DP(1),     -- insert Address 1 signal dual port DPO
        DPRA2 => ADDRESS_DP(2),     -- insert Address 2 signal dual port DPO
        DPRA3 => ADDRESS_DP(3),     -- insert Address 3 signal dual port DPO
        SPO   => O_DATA_OUT(i),         -- insert output signal SPO
        WCLK  => CLK,                   -- insert Write Clock signal
        WE    => WRITE_EN               -- insert Write Enable signal
        );
  end generate dram16s;

end;
