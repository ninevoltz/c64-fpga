library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;

library UNISIM;
use UNISIM.Vcomponents.all;

entity vga_to_dvi is
  port (
    I_RESET : in  std_logic;
    I_CLK   : in  std_logic;
    I_VGA_R : in  std_logic_vector(7 downto 0);
    I_VGA_G : in  std_logic_vector(7 downto 0);
    I_VGA_B : in  std_logic_vector(7 downto 0);
    I_HSYNC : in  std_logic;
    I_VSYNC : in  std_logic;
    I_BLANK : in  std_logic;
    O_TMDS  : out std_logic_vector(3 downto 0);
    O_TMDSB : out std_logic_vector(3 downto 0)
    );
end entity;

architecture rtl of vga_to_dvi is
  signal pclk         : std_logic;
  signal pllclk0      : std_logic;
  signal pllclk1      : std_logic;
  signal pllclk2      : std_logic;
  signal pclkx2       : std_logic;
  signal pclkx10      : std_logic;
  signal pll_lckd     : std_logic;
  signal clkfbout     : std_logic;
  signal reset        : std_logic;
  signal serdesstrobe : std_logic;
  signal bufpll_lock  : std_logic;
  signal active       : std_logic;
  signal active_q     : std_logic;
  signal vsync        : std_logic;
  signal hsync        : std_logic;
  signal VGA_HSYNC    : std_logic;
  signal VGA_VSYNC    : std_logic;
  signal de           : std_logic;
  signal tmds_data0   : std_logic_vector(4 downto 0);
  signal tmds_data1   : std_logic_vector(4 downto 0);
  signal tmds_data2   : std_logic_vector(4 downto 0);
  signal tmdsint      : std_logic_vector(2 downto 0);
  signal serdes_rst   : std_logic := (I_RESET or (not bufpll_lock));
  signal tmdsclkint   : std_logic_vector(4 downto 0);
  signal toggle       : std_logic;
  signal tmdsclk      : std_logic;

begin
  pclkbufg   : BUFG port map (I => pllclk1, O => pclk);
  pclkx2bufg : BUFG port map (I => pllclk2, O => pclkx2);

  PLL_OSERDES : PLL_BASE
    generic map (
      CLKIN_PERIOD   => 20.0,
      CLKFBOUT_MULT  => 10,             -- set VCO to 10x of CLKIN
      CLKOUT0_DIVIDE => 2,
      CLKOUT1_DIVIDE => 20,
      CLKOUT2_DIVIDE => 10,
      COMPENSATION   => "INTERNAL"
      )
    port map (
      CLKFBIN  => clkfbout,
      CLKFBOUT => clkfbout,
      CLKIN    => I_CLK,
      CLKOUT0  => pllclk0,
      CLKOUT1  => pllclk1,
      CLKOUT2  => pllclk2,
      CLKOUT3  => open,
      CLKOUT4  => open,
      CLKOUT5  => open,
      LOCKED   => pll_lckd,
      RST      => '0'
      );

  ioclk_buf : BUFPLL generic map (DIVIDE                        => 5) port map (GCLK => pclkx2, IOCLK => pclkx10, LOCK => bufpll_lock,
                                                         LOCKED => pll_lckd, PLLIN => pllclk0, SERDESSTROBE => serdesstrobe);

  synchro_reset : entity work.synchro
    port map (
      async => (not pll_lckd),
      clk   => pclk,
      sync  => reset
      );

  active     <= (not I_BLANK);
  serdes_rst <= I_RESET or (not bufpll_lock);

  process
  begin
    wait until rising_edge(pclk);
    hsync     <= I_HSYNC;
    vsync     <= I_VSYNC;
    VGA_HSYNC <= hsync;
    VGA_VSYNC <= vsync;
    active_q  <= active;
    de        <= active_q;
  end process;

  enc0 : entity work.dvi_encoder
    port map (
      blue_din   => I_VGA_B,
      clkin      => pclk,
      clkx2in    => pclkx2,
      de         => de,
      green_din  => I_VGA_G,
      hsync      => VGA_HSYNC,
      red_din    => I_VGA_R,
      rstin      => reset,
      tmds_data0 => tmds_data0,
      tmds_data1 => tmds_data1,
      tmds_data2 => tmds_data2,
      vsync      => VGA_VSYNC
      );

  oserdes_0 : entity work.serdes_n_to_1
    generic map (
      SF => 5
      )
    port map (
      datain       => tmds_data0,
      gclk         => pclkx2,
      iob_data_out => tmdsint(0),
      ioclk        => pclkx10,
      reset        => serdes_rst,
      serdesstrobe => serdesstrobe
      );

  oserdes_1 : entity work.serdes_n_to_1
    generic map (
      SF => 5
      )
    port map (
      datain       => tmds_data1,
      gclk         => pclkx2,
      iob_data_out => tmdsint(1),
      ioclk        => pclkx10,
      reset        => serdes_rst,
      serdesstrobe => serdesstrobe
      );

  oserdes_2 : entity work.serdes_n_to_1
    generic map (
      SF => 5
      )
    port map (
      datain       => tmds_data2,
      gclk         => pclkx2,
      iob_data_out => tmdsint(2),
      ioclk        => pclkx10,
      reset        => serdes_rst,
      serdesstrobe => serdesstrobe
      );

  clkout : entity work.serdes_n_to_1
    generic map (
      SF => 5
      )
    port map (
      datain       => tmdsclkint,
      gclk         => pclkx2,
      iob_data_out => tmdsclk,
      ioclk        => pclkx10,
      reset        => serdes_rst,
      serdesstrobe => serdesstrobe
      );

  TMDS0 : OBUFDS port map (I => tmdsint(0), O => O_TMDS(0), OB => O_TMDSB(0));
  TMDS1 : OBUFDS port map (I => tmdsint(1), O => O_TMDS(1), OB => O_TMDSB(1));
  TMDS2 : OBUFDS port map (I => tmdsint(2), O => O_TMDS(2), OB => O_TMDSB(2));
  TMDS3 : OBUFDS port map (I => tmdsclk, O => O_TMDS(3), OB => O_TMDSB(3));

  process
  begin
    if (serdes_rst = '1') then
      toggle <= '0';
    end if;
    wait until rising_edge(pclkx2);
    toggle <= (not toggle);
  end process;

  process
  begin
    wait until rising_edge(pclkx2);
    if (toggle = '1') then
      tmdsclkint <= "11111";
    else
      tmdsclkint <= "00000";
    end if;
  end process;


end;


