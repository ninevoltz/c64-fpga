------------------------------------------------------------------------------
--
--  Xilinx, Inc. 2008                 www.xilinx.com
--
------------------------------------------------------------------------------
--
--  File name :       encode.v
--
--  Description :     TMDS encoder  
--
--  Date - revision : Jan. 2008 - v 1.0
--
--  Author :          Bob Feng
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
--  Copyright © 2006 Xilinx, Inc.
--  All rights reserved
--
------------------------------------------------------------------------------  

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.std_logic_arith.all;

entity encode is
  generic (
    CTRLTOKEN0 : std_logic_vector (9 downto 0) := "1101010100";  -- 852
    CTRLTOKEN1 : std_logic_vector (9 downto 0) := "0010101011";  -- 171
    CTRLTOKEN2 : std_logic_vector (9 downto 0) := "0101010100";  -- 340
    CTRLTOKEN3 : std_logic_vector (9 downto 0) := "1010101011"   -- 683
    );
  port (
    clkin : in  std_logic;              -- pixel clock input
    rstin : in  std_logic;              -- async. reset input (active high)
    din   : in  std_logic_vector(7 downto 0);  -- data inputs: expect registered
    c0    : in  std_logic;              -- c0 input
    c1    : in  std_logic;              -- c1 input
    de    : in  std_logic;              -- de input
    dout  : out std_logic_vector(9 downto 0)   -- data outputs
    );
end entity;


architecture rtl of encode is
  signal n1d   : unsigned(3 downto 0);  -- number of 1s in din
  signal din_q : std_logic_vector(7 downto 0);

  signal decision1 : std_logic;
  signal q_m       : std_logic_vector(8 downto 0);
  signal n1q_m     : unsigned(3 downto 0);  -- number of 1s and 0s for q_m
  signal n0q_m     : unsigned(3 downto 0);
  signal cnt       : unsigned(4 downto 0);  -- disparity counter, MSB is the sign bit
  signal decision2 : std_logic;
  signal decision3 : std_logic;
  ------------------------------------
  -- pipeline alignment
  ------------------------------------
  signal de_q      : std_logic;
  signal de_reg    : std_logic;
  signal c0_q      : std_logic;
  signal c1_q      : std_logic;
  signal c0_reg    : std_logic;
  signal c1_reg    : std_logic;
  signal c_reg     : std_logic_vector(1 downto 0);
  signal q_m_reg   : std_logic_vector(8 downto 0);

begin

  c_reg <= (c1_reg & c0_reg);

  ------------------------------------------------------------
  -- Counting number of 1s and 0s for each incoming pixel
  -- component. Pipe line the result.
  -- Register Data Input so it matches the pipe lined adder
  -- output
  ------------------------------------------------------------
  process
  begin
    wait until rising_edge(clkin);
    n1d <= (("000" & din(0)) + ("000" & din(1)) + ("000" & din(2)) + ("000" & din(3)) + ("000" & din(4))
            + ("000" & din(5)) + ("000" & din(6)) + ("000" & din(7))) after 1 ns;
    din_q <= din after 1 ns;
    ------------------------------------------------------
    -- Stage 1: 8 bit -> 9 bit
    -- Refer to DVI 1.0 Specification, page 29, Figure 3-5
    ------------------------------------------------------
    if ((n1d > 4) or ((n1d = 4) and (din_q(0) = '0'))) then
      decision1 <= '1';
    else
      decision1 <= '0';
    end if;

    q_m(0) <= din_q(0);

    if (decision1 = '1') then
      q_m(1) <= (q_m(0) xor not din_q(1));
      q_m(2) <= (q_m(1) xor not din_q(2));
      q_m(3) <= (q_m(2) xor not din_q(3));
      q_m(4) <= (q_m(3) xor not din_q(4));
      q_m(5) <= (q_m(4) xor not din_q(5));
      q_m(6) <= (q_m(5) xor not din_q(6));
      q_m(7) <= (q_m(6) xor not din_q(7));
      q_m(8) <= '0';
    else
      q_m(1) <= (q_m(0) xor din_q(1));
      q_m(2) <= (q_m(1) xor din_q(2));
      q_m(3) <= (q_m(2) xor din_q(3));
      q_m(4) <= (q_m(3) xor din_q(4));
      q_m(5) <= (q_m(4) xor din_q(5));
      q_m(6) <= (q_m(5) xor din_q(6));
      q_m(7) <= (q_m(6) xor din_q(7));
      q_m(8) <= '1';
    end if;
    --------------------------------------------------------
    -- Stage 2: 9 bit -> 10 bit
    -- Refer to DVI 1.0 Specification, page 29, Figure 3-5
    --------------------------------------------------------
    n1q_m <= (("000" & q_m(0)) + ("000" & q_m(1)) + ("000" & q_m(2)) + ("000" & q_m(3))
              + ("000" & q_m(4)) + ("000" & q_m(5)) + ("000" & q_m(6)) + ("000" & q_m(7))) after 1 ns;
    n0q_m <= ("1000" - (("000" & q_m(0)) + ("000" & q_m(1)) + ("000" & q_m(2)) + ("000" & q_m(3))
                        + ("000" & q_m(4)) + ("000" & q_m(5)) + ("000" & q_m(6)) + ("000" & q_m(7)))) after 1 ns;
    if ((cnt = 0) or (n1q_m = n0q_m)) then
      decision2 <= '1';
    else
      decision2 <= '0';
    end if;
    if (not cnt(4) = '1' and (n1q_m > n0q_m)) or (cnt(4) = '1' and (n0q_m > n1q_m)) then
      decision3 <= '1';
    else
      decision3 <= '0';
    end if;
    ------------------------------------
    -- pipeline alignment
    ------------------------------------       
    de_q    <= de   after 1 ns;
    de_reg  <= de_q after 1 ns;
    c0_q    <= c0   after 1 ns;
    c0_reg  <= c0_q after 1 ns;
    c1_q    <= c1   after 1 ns;
    c1_reg  <= c1_q after 1 ns;
    q_m_reg <= q_m  after 1 ns;
  end process;

  -------------------------------
  -- 10-bit out
  -- disparity counter
  -------------------------------
  process
  begin
    if (rstin = '1') then
      dout <= "0000000000";
      cnt  <= "00000";
    end if;
    wait until rising_edge(clkin);
    if (de_reg = '1') then
      if (decision2 = '1') then
        dout(9) <= (not q_m_reg(8)) after 1 ns;
        dout(8) <= q_m_reg(8)       after 1 ns;
        if (q_m_reg(8) = '1') then
          dout(7 downto 0) <= q_m_reg(7 downto 0) after 1 ns;
        else
          dout(7 downto 0) <= (not q_m_reg(7 downto 0)) after 1 ns;
        end if;
        if ((not q_m_reg(8)) = '1') then
          cnt <= ((cnt + ("0" & n0q_m)) - ("0" & n1q_m)) after 1 ns;
        else
          cnt <= ((cnt + ("0" & n1q_m)) - ("0" & n0q_m)) after 1 ns;
        end if;
      else
        if (decision3 = '1') then
          dout(9)          <= '1'                                            after 1 ns;
          dout(8)          <= q_m_reg(8)                                     after 1 ns;
          dout(7 downto 0) <= (not q_m_reg(7 downto 0))                      after 1 ns;
          cnt              <= ((cnt + (q_m_reg(8) & '0')) + (n0q_m - n1q_m)) after 1 ns;
        else
          dout(9)          <= '0'                                                  after 1 ns;
          dout(8)          <= q_m_reg(8)                                           after 1 ns;
          dout(7 downto 0) <= q_m_reg(7 downto 0)                                  after 1 ns;
          cnt              <= ((cnt - ((not q_m_reg(8)) & '0')) + (n1q_m - n0q_m)) after 1 ns;
        end if;
      end if;
    else
      case (c_reg) is
        when "00" =>
          dout <= CTRLTOKEN0 after 1 ns;
        when "01" =>
          dout <= CTRLTOKEN1 after 1 ns;
        when "10" =>
          dout <= CTRLTOKEN2 after 1 ns;
        when others =>
          dout <= CTRLTOKEN3 after 1 ns;
      end case;
      cnt <= "00000" after 1 ns;
    end if;
  end process;
end;


