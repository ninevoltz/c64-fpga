------------------------------------------------------------------------------
--
--  Xilinx, Inc. 2008                 www.xilinx.com
--
------------------------------------------------------------------------------
--
--  File name :         serdes_n_to_1.v
--
--  Description :       1-bit generic n:1 transmitter module
--                      Takes in n bits of data and serialises this to 1 bit
--                      data is transmitted LSB first
--                      0, 1, 2 ......
--
--  Date - revision :   August 1st 2008 - v 1.0
--
--  Author :            NJS
--
--  Disclaimer: LIMITED WARRANTY AND DISCLAMER. These designs are
--              provided to you "as is". Xilinx and its licensors make and you
--              receive no warranties or conditions, express, implied,
--              statutory or otherwise, and Xilinx specifically disclaims any
--              implied warranties of merchantability, non-infringement,or
--              fitness for a particular purpose. Xilinx does not warrant that
--              the functions contained in these designs will meet your
--              requirements, or that the operation of these designs will be
--              uninterrupted or error free, or that defects in the Designs
--              will be corrected. Furthermore, Xilinx does not warrantor
--              make any representations regarding use or the results of the
--              use of the designs in terms of correctness, accuracy,
--              reliability, or otherwise.
--
--              LIMITATION OF LIABILITY. In no event will Xilinx or its
--              licensors be liable for any loss of data, lost profits,cost
--              or procurement of substitute goods or services, or for any
--              special, incidental, consequential, or indirect damages
--              arising from the use or operation of the designs or
--              accompanying documentation, however caused and on any theory
--              of liability. This limitation will apply even if Xilinx
--              has been advised of the possibility of such damage. This
--              limitation shall apply not-withstanding the failure of the
--              essential purpose of any limited remedies herein.
--
--  Copyright © 2008 Xilinx, Inc.
--  All rights reserved
--
------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;

library UNISIM;
use UNISIM.Vcomponents.all;

entity serdes_n_to_1 is
  generic (
    SF : integer := 8
    );
  port (
    ioclk        : in  std_logic;
    serdesstrobe : in  std_logic;
    reset        : in  std_logic;
    gclk         : in  std_logic;
    datain       : in  std_logic_vector((SF - 1) downto 0);
    iob_data_out : out std_logic
    );
end entity;

architecture rtl of serdes_n_to_1 is
  signal cascade_di : std_logic;
  signal cascade_do : std_logic;
  signal cascade_ti : std_logic;
  signal cascade_to : std_logic;
  signal mdatain    : std_logic_vector(8 downto 0);

begin

  loop0 : for i in 0 to (SF - 1) generate
  begin
    mdatain(i) <= datain(i);
  end generate loop0;

  loop1 : for i in SF to 8 generate
  begin
    mdatain(i) <= '0';
  end generate loop1;

  oserdes_m : OSERDES2
    generic map (
      DATA_WIDTH   => SF,  -- SERDES word width.  This should match the setting is BUFPLL
      DATA_RATE_OQ => "SDR",            -- <SDR>, DDR
      DATA_RATE_OT => "SDR",            -- <SDR>, DDR
      SERDES_MODE  => "MASTER",         -- <DEFAULT>, MASTER, SLAVE
      OUTPUT_MODE  => "DIFFERENTIAL"
      )
    port map (
      CLK0      => ioclk,
      CLK1      => '0',
      CLKDIV    => gclk,
      D1        => mdatain(4),
      D2        => mdatain(5),
      D3        => mdatain(6),
      D4        => mdatain(7),
      IOCE      => serdesstrobe,
      OCE       => '1',
      OQ        => iob_data_out,
      RST       => reset,
      SHIFTIN1  => '1',                 -- Dummy input in Master
      SHIFTIN2  => '1',                 -- Dummy input in Master
      SHIFTIN3  => cascade_do,          -- Cascade output D data from slave
      SHIFTIN4  => cascade_to,          -- Cascade output T data from slave
      SHIFTOUT1 => cascade_di,          -- Cascade input D data to slave
      SHIFTOUT2 => cascade_ti,          -- Cascade input T data to slave
      SHIFTOUT3 => open,                -- Dummy output in Master
      SHIFTOUT4 => open,                -- Dummy output in Master
      T1        => '0',
      T2        => '0',
      T3        => '0',
      T4        => '0',
      TCE       => '1',
      TQ        => open,
      TRAIN     => '0'
      );
  oserdes_s : OSERDES2
    generic map (
      DATA_WIDTH   => SF,  -- SERDES word width.  This should match the setting is BUFPLL
      DATA_RATE_OQ => "SDR",            -- <SDR>, DDR
      DATA_RATE_OT => "SDR",            -- <SDR>, DDR
      SERDES_MODE  => "SLAVE",          -- <DEFAULT>, MASTER, SLAVE
      OUTPUT_MODE  => "DIFFERENTIAL"
      )
    port map (
      CLK0      => ioclk,
      CLK1      => '0',
      CLKDIV    => gclk,
      D1        => mdatain(0),
      D2        => mdatain(1),
      D3        => mdatain(2),
      D4        => mdatain(3),
      IOCE      => serdesstrobe,
      OCE       => '1',
      OQ        => open,
      RST       => reset,
      SHIFTIN1  => cascade_di,          -- Cascade input D from Master
      SHIFTIN2  => cascade_ti,          -- Cascade input T from Master
      SHIFTIN3  => '1',                 -- Dummy input in Slave
      SHIFTIN4  => '1',                 -- Dummy input in Slave
      SHIFTOUT1 => open,                -- Dummy output in Slave
      SHIFTOUT2 => open,                -- Dummy output in Slave
      SHIFTOUT3 => cascade_do,          -- Cascade output D data to Master
      SHIFTOUT4 => cascade_to,          -- Cascade output T data to Master
      T1        => '0',
      T2        => '0',
      T3        => '0',
      T4        => '0',
      TCE       => '1',
      TQ        => open,
      TRAIN     => '0'
      );
end;


