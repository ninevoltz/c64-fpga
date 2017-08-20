------------------------------------------------------------------------------
--
--  Xilinx, Inc. 2009                 www.xilinx.com
--
--  XAPP xyz
--
------------------------------------------------------------------------------
--
--  File name :       dvi_encoder.v
--
--  Description :     dvi_encoder 
--
--  Date - revision : April 2009 - 1.0.0
--
--  Author :          Bob Feng
--
--  Disclaimer: LIMITED WARRANTY AND DISCLAMER. These designs are
--              provided to you "as is". Xilinx and its licensors makeand you
--              receive no warranties or conditions, express, implied,
--              statutory or otherwise, and Xilinx specificallydisclaims any
--              implied warranties of merchantability, non-infringement,or
--              fitness for a particular purpose. Xilinx does notwarrant that
--              the functions contained in these designs will meet your
--              requirements, or that the operation of these designswill be
--              uninterrupted or error free, or that defects in theDesigns
--              will be corrected. Furthermore, Xilinx does not warrantor
--              make any representations regarding use or the results ofthe
--              use of the designs in terms of correctness, accuracy,
--              reliability, or otherwise.
--
--              LIMITATION OF LIABILITY. In no event will Xilinx or its
--              licensors be liable for any loss of data, lost profits,cost
--              or procurement of substitute goods or services, or forany
--              special, incidental, consequential, or indirect damages
--              arising from the use or operation of the designs or
--              accompanying documentation, however caused and on anytheory
--              of liability. This limitation will apply even if Xilinx
--              has been advised of the possibility of such damage. This
--              limitation shall apply not-withstanding the failure ofthe
--              essential purpose of any limited remedies herein.
--
--  Copyright © 2009 Xilinx, Inc.
--  All rights reserved
--
------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;

library UNISIM;
use UNISIM.Vcomponents.all;

entity dvi_encoder is
  port (
    clkin      : in  std_logic;         --  pixel clock
    clkx2in    : in  std_logic;         --  pixel clock x2
    rstin      : in  std_logic;         -- reset
    blue_din   : in  std_logic_vector(7 downto 0);  -- Blue data in
    green_din  : in  std_logic_vector(7 downto 0);  -- Green data in
    red_din    : in  std_logic_vector(7 downto 0);  -- Red data in
    hsync      : in  std_logic;         -- hsync data
    vsync      : in  std_logic;         -- vsync data
    de         : in  std_logic;         -- data enable
    tmds_data0 : out std_logic_vector(4 downto 0);  -- 5-bit busses converted from 10-bit
    tmds_data1 : out std_logic_vector(4 downto 0);
    tmds_data2 : out std_logic_vector(4 downto 0)
    );
end entity;

architecture rtl of dvi_encoder is
  signal red       : std_logic_vector(9 downto 0);
  signal green     : std_logic_vector(9 downto 0);
  signal blue      : std_logic_vector(9 downto 0);
  signal s_data    : std_logic_vector(29 downto 0) := (red(9 downto 5) & green(9 downto 5) & blue(9 downto 5) & red(4 downto 0) & green(4 downto 0) & blue(4 downto 0));
  signal tmds_data : std_logic_vector(14 downto 0);

begin

  tmds_data2 <= tmds_data (14 downto 10);
  tmds_data1 <= tmds_data (9 downto 5);
  tmds_data0 <= tmds_data (4 downto 0);

  encb : entity work.encode
    port map (
      c0    => hsync,
      c1    => vsync,
      clkin => clkin,
      de    => de,
      din   => blue_din,
      dout  => blue,
      rstin => rstin
      );
  encg : entity work.encode
    port map (
      c0    => '0',
      c1    => '0',
      clkin => clkin,
      de    => de,
      din   => green_din,
      dout  => green,
      rstin => rstin
      );
  encr : entity work.encode
    port map (
      c0    => '0',
      c1    => '0',
      clkin => clkin,
      de    => de,
      din   => red_din,
      dout  => red,
      rstin => rstin
      );
  pixel2x : entity work.convert_30to15_fifo
    port map (
      clk     => clkin,
      clkx2   => clkx2in,
      datain  => s_data,
      dataout => tmds_data,
      rst     => rstin
      );
end;


