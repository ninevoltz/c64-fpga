library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;

library UNISIM;
use UNISIM.Vcomponents.all;

entity convert_30to15_fifo is
  port (
    rst     : in  std_logic;            -- reset
    clk     : in  std_logic;            -- clock input
    clkx2   : in  std_logic;            -- 2x clock input
    datain  : in  std_logic_vector(29 downto 0);  -- input data for 2:1 serialisation
    dataout : out std_logic_vector(14 downto 0)   -- 5-bit data out
    );
end entity;

architecture rtl of convert_30to15_fifo is

  attribute ASYNC_REG            : string;
  attribute ASYNC_REG of fdp_rst : label is "TRUE";

  ----------------------------------------------------
  -- Here we instantiate a 16x10 Dual Port RAM
  -- and fill first it with data aligned to
  -- clk domain
  ---------------------------------------------------- 
  signal wa      : std_logic_vector(3 downto 0);   -- RAM write address
  signal wa_d    : std_logic_vector(3 downto 0);   -- RAM write address
  signal ra      : std_logic_vector(3 downto 0);   -- RAM read address
  signal ra_d    : std_logic_vector(3 downto 0);   -- RAM read address
  signal dataint : std_logic_vector(29 downto 0);  -- RAM output

  signal rstsync   : std_logic;
  signal rstsync_q : std_logic;
  signal rstp      : std_logic;
  signal mux       : std_logic_vector(14 downto 0);
  signal sync      : std_logic;
  signal db        : std_logic_vector(29 downto 0);

begin

  process
  begin
    wait on wa;
    case (wa) is
      when "0000" =>
        wa_d <= "0001";
      when "0001" =>
        wa_d <= "0010";
      when "0010" =>
        wa_d <= "0011";
      when "0011" =>
        wa_d <= "0100";
      when "0100" =>
        wa_d <= "0101";
      when "0101" =>
        wa_d <= "0110";
      when "0110" =>
        wa_d <= "0111";
      when "0111" =>
        wa_d <= "1000";
      when "1000" =>
        wa_d <= "1001";
      when "1001" =>
        wa_d <= "1010";
      when "1010" =>
        wa_d <= "1011";
      when "1011" =>
        wa_d <= "1100";
      when "1100" =>
        wa_d <= "1101";
      when "1101" =>
        wa_d <= "1110";
      when "1110" =>
        wa_d <= "1111";
      when others =>
        wa_d <= "0000";
    end case;
  end process;

  fdc_wa0 : FDC port map (C => clk, CLR => rst, D => wa_d(0), Q => wa(0));
  fdc_wa1 : FDC port map (C => clk, CLR => rst, D => wa_d(1), Q => wa(1));
  fdc_wa2 : FDC port map (C => clk, CLR => rst, D => wa_d(2), Q => wa(2));
  fdc_wa3 : FDC port map (C => clk, CLR => rst, D => wa_d(3), Q => wa(3));

  -- Dual Port fifo to bridge data from clk to clkx2
  fifo_u : entity work.DRAM16XN
    generic map (
      data_width => 30
      )
    port map (
      ADDRESS       => wa,
      ADDRESS_DP    => ra,
      CLK           => clk,
      DATA_IN       => datain,
      O_DATA_OUT    => open,
      O_DATA_OUT_DP => dataint,
      WRITE_EN      => '1'
      );

  -------------------------------------------------------------
  -- Here starts clk2x domain for fifo read out 
  -- FIFO read is set to be once every 2 cycles of clk2x in order
  -- to keep up pace with the fifo write speed
  -- Also FIFO read reset is delayed a bit in order to avoid
  -- underflow
  -----------------------------------------------------------------             
  process
  begin
    wait on ra;
    case (ra) is
      when "0000" =>
        ra_d <= "0001";
      when "0001" =>
        ra_d <= "0010";
      when "0010" =>
        ra_d <= "0011";
      when "0011" =>
        ra_d <= "0100";
      when "0100" =>
        ra_d <= "0101";
      when "0101" =>
        ra_d <= "0110";
      when "0110" =>
        ra_d <= "0111";
      when "0111" =>
        ra_d <= "1000";
      when "1000" =>
        ra_d <= "1001";
      when "1001" =>
        ra_d <= "1010";
      when "1010" =>
        ra_d <= "1011";
      when "1011" =>
        ra_d <= "1100";
      when "1100" =>
        ra_d <= "1101";
      when "1101" =>
        ra_d <= "1110";
      when "1110" =>
        ra_d <= "1111";
      when others =>
        ra_d <= "0000";
    end case;
  end process;

  fdp_rst : FDP port map (C => clkx2, D => rst, PRE => rst, Q => rstsync);

  fd_rstsync : FD port map (C => clkx2, D => rstsync, Q => rstsync_q);
  fd_rstp    : FD port map (C => clkx2, D => rstsync_q, Q => rstp);

  sync_gen : FDR port map (Q => sync, C => clkx2, R => rstp, D => (not sync));

  fdc_ra0 : FDRE port map (C => clkx2, D => ra_d(0), R => rstp, CE => sync, Q => ra(0));
  fdc_ra1 : FDRE port map (C => clkx2, D => ra_d(1), R => rstp, CE => sync, Q => ra(1));
  fdc_ra2 : FDRE port map (C => clkx2, D => ra_d(2), R => rstp, CE => sync, Q => ra(2));
  fdc_ra3 : FDRE port map (C => clkx2, D => ra_d(3), R => rstp, CE => sync, Q => ra(3));

  fd_db0  : FDE port map (C => clkx2, D => dataint(0), CE => sync, Q => db(0));
  fd_db1  : FDE port map (C => clkx2, D => dataint(1), CE => sync, Q => db(1));
  fd_db2  : FDE port map (C => clkx2, D => dataint(2), CE => sync, Q => db(2));
  fd_db3  : FDE port map (C => clkx2, D => dataint(3), CE => sync, Q => db(3));
  fd_db4  : FDE port map (C => clkx2, D => dataint(4), CE => sync, Q => db(4));
  fd_db5  : FDE port map (C => clkx2, D => dataint(5), CE => sync, Q => db(5));
  fd_db6  : FDE port map (C => clkx2, D => dataint(6), CE => sync, Q => db(6));
  fd_db7  : FDE port map (C => clkx2, D => dataint(7), CE => sync, Q => db(7));
  fd_db8  : FDE port map (C => clkx2, D => dataint(8), CE => sync, Q => db(8));
  fd_db9  : FDE port map (C => clkx2, D => dataint(9), CE => sync, Q => db(9));
  fd_db10 : FDE port map (C => clkx2, D => dataint(10), CE => sync, Q => db(10));
  fd_db11 : FDE port map (C => clkx2, D => dataint(11), CE => sync, Q => db(11));
  fd_db12 : FDE port map (C => clkx2, D => dataint(12), CE => sync, Q => db(12));
  fd_db13 : FDE port map (C => clkx2, D => dataint(13), CE => sync, Q => db(13));
  fd_db14 : FDE port map (C => clkx2, D => dataint(14), CE => sync, Q => db(14));
  fd_db15 : FDE port map (C => clkx2, D => dataint(15), CE => sync, Q => db(15));
  fd_db16 : FDE port map (C => clkx2, D => dataint(16), CE => sync, Q => db(16));
  fd_db17 : FDE port map (C => clkx2, D => dataint(17), CE => sync, Q => db(17));
  fd_db18 : FDE port map (C => clkx2, D => dataint(18), CE => sync, Q => db(18));
  fd_db19 : FDE port map (C => clkx2, D => dataint(19), CE => sync, Q => db(19));
  fd_db20 : FDE port map (C => clkx2, D => dataint(20), CE => sync, Q => db(20));
  fd_db21 : FDE port map (C => clkx2, D => dataint(21), CE => sync, Q => db(21));
  fd_db22 : FDE port map (C => clkx2, D => dataint(22), CE => sync, Q => db(22));
  fd_db23 : FDE port map (C => clkx2, D => dataint(23), CE => sync, Q => db(23));
  fd_db24 : FDE port map (C => clkx2, D => dataint(24), CE => sync, Q => db(24));
  fd_db25 : FDE port map (C => clkx2, D => dataint(25), CE => sync, Q => db(25));
  fd_db26 : FDE port map (C => clkx2, D => dataint(26), CE => sync, Q => db(26));
  fd_db27 : FDE port map (C => clkx2, D => dataint(27), CE => sync, Q => db(27));
  fd_db28 : FDE port map (C => clkx2, D => dataint(28), CE => sync, Q => db(28));
  fd_db29 : FDE port map (C => clkx2, D => dataint(29), CE => sync, Q => db(29));

  process (sync, db)
  begin
    if (sync = '0') then
      mux <= db(14 downto 0);
    else
      mux <= db(29 downto 15);
    end if;
  end process;

  fd_out0  : FD port map (C => clkx2, D => mux(0), Q => dataout(0));
  fd_out1  : FD port map (C => clkx2, D => mux(1), Q => dataout(1));
  fd_out2  : FD port map (C => clkx2, D => mux(2), Q => dataout(2));
  fd_out3  : FD port map (C => clkx2, D => mux(3), Q => dataout(3));
  fd_out4  : FD port map (C => clkx2, D => mux(4), Q => dataout(4));
  fd_out5  : FD port map (C => clkx2, D => mux(5), Q => dataout(5));
  fd_out6  : FD port map (C => clkx2, D => mux(6), Q => dataout(6));
  fd_out7  : FD port map (C => clkx2, D => mux(7), Q => dataout(7));
  fd_out8  : FD port map (C => clkx2, D => mux(8), Q => dataout(8));
  fd_out9  : FD port map (C => clkx2, D => mux(9), Q => dataout(9));
  fd_out10 : FD port map (C => clkx2, D => mux(10), Q => dataout(10));
  fd_out11 : FD port map (C => clkx2, D => mux(11), Q => dataout(11));
  fd_out12 : FD port map (C => clkx2, D => mux(12), Q => dataout(12));
  fd_out13 : FD port map (C => clkx2, D => mux(13), Q => dataout(13));
  fd_out14 : FD port map (C => clkx2, D => mux(14), Q => dataout(14));

end;
