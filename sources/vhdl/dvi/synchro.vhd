------------------------------------------------------------------------------
-- Copyright (c) 2006 Xilinx, Inc.
-- This design is confidential and proprietary of Xilinx, All Rights Reserved.
------------------------------------------------------------------------------
--   ____  ____
--  /   /\/   /
-- /___/  \  /   Vendor:        Xilinx
-- \   \   \/    Version:       1.0.0
--  \   \        Filename:      synchro.v
--  /   /        Date Created:  December 25, 2006
-- /___/   /\    Last Modified: December 25, 2006
-- \   \  /  \
--  \___\/\___\
--
-- Devices:   Spartan-3 Generation FPGA
-- Purpose:   Signal synchronizer, for async inputs
-- Contact:   crabill@xilinx.com
-- Reference: None
--
-- Revision History:
--   Rev 1.0.0 - (crabill) First created December 25, 2006.
--
------------------------------------------------------------------------------
--
-- LIMITED WARRANTY AND DISCLAIMER. These designs are provided to you "as is".
-- Xilinx and its licensors make and you receive no warranties or conditions,
-- express, implied, statutory or otherwise, and Xilinx specifically disclaims
-- any implied warranties of merchantability, non-infringement, or fitness for
-- a particular purpose. Xilinx does not warrant that the functions contained
-- in these designs will meet your requirements, or that the operation of
-- these designs will be uninterrupted or error free, or that defects in the
-- designs will be corrected. Furthermore, Xilinx does not warrant or make any
-- representations regarding use or the results of the use of the designs in
-- terms of correctness, accuracy, reliability, or otherwise.
--
-- LIMITATION OF LIABILITY. In no event will Xilinx or its licensors be liable
-- for any loss of data, lost profits, cost or procurement of substitute goods
-- or services, or for any special, incidental, consequential, or indirect
-- damages arising from the use or operation of the designs or accompanying
-- documentation, however caused and on any theory of liability. This
-- limitation will apply even if Xilinx has been advised of the possibility
-- of such damage. This limitation shall apply not-withstanding the failure
-- of the essential purpose of any limited remedies herein.
--
------------------------------------------------------------------------------
-- Copyright (c) 2006 Xilinx, Inc.
-- This design is confidential and proprietary of Xilinx, All Rights Reserved.
------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;
use ieee.numeric_std.all;

library UNISIM;
use UNISIM.Vcomponents.all;

entity synchro is
  port (
    async : in  std_logic;
    clk   : in  std_logic;
    sync  : out std_logic
    );
end entity;

--******************************************************************
-- Synchronizer.                                                   
--******************************************************************
architecture rtl of synchro is

  attribute ASYNC_REG        : string;
  attribute HU_SET           : string;
  attribute RLOC             : string;
  attribute ASYNC_REG of fda : label is "TRUE";
  attribute ASYNC_REG of fdb : label is "TRUE";
  attribute HU_SET of fda    : label is "SYNC";
  attribute HU_SET of fdb    : label is "SYNC";
  attribute RLOC of fda      : label is "X0Y0";
  attribute RLOC of fdb      : label is "X0Y0";

  signal temp : std_logic;

begin
  fda : FDP port map (C => clk, D => async, PRE => '0', Q => temp);
  fdb : FDP port map (C => clk, D => temp, PRE => '0', Q => sync);
end;
