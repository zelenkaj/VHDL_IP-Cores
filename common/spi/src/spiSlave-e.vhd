-------------------------------------------------------------------------------
--! @file spiSlaveRtl.vhd
--
--! @brief SPI Slave IP-Core
--
--! @details This is the SPI ipcore entity defining the interface to an Spi
--! slave.
-------------------------------------------------------------------------------
--
--    (c) B&R, 2013
--
--    Redistribution and use in source and binary forms, with or without
--    modification, are permitted provided that the following conditions
--    are met:
--
--    1. Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--    2. Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--
--    3. Neither the name of B&R nor the names of its
--       contributors may be used to endorse or promote products derived
--       from this software without prior written permission. For written
--       permission, please contact office@br-automation.com
--
--    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
--    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
--    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
--    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
--    COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
--    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
--    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
--    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
--    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
--    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
--    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--    POSSIBILITY OF SUCH DAMAGE.
--
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.global.all;

entity spiSlave is
    generic (
        --! SPI register size (= frame size)
        gRegisterSize : natural := 8;
        --! SPI clock polarity (allowed values 0 and 1)
        gPolarity : natural := 0;
        --! SPI clock phase (allowed values 0 and 1)
        gPhase : natural := 0;
        --! Shift direction (0 = LSB first, otherwise = MSB first)
        gShiftDir : natural := 0
    );
    port (
        --! Asynchronous reset
        iArst : in std_logic;
        --! Clock
        iClk : in std_logic;
        -- SPI
        --! SPI clock
        iSpiClk : in std_logic;
        --! SPI select (low-active)
        inSpiSel : in std_logic;
        --! SPI master-out-slave-in
        iSpiMosi : in std_logic;
        --! SPI master-in-slave-out
        oSpiMiso : out std_logic;
        --! SPI master-in-slave-out buffer enable
        oSpiMiso_t : out std_logic;
        -- Control interface
        --! Register load data
        iLoadData : in std_logic_vector(gRegisterSize-1 downto 0);
        --! Load register with load data
        iLoad : in std_logic;
        --! Register read data
        oReadData : out std_logic_vector(gRegisterSize-1 downto 0);
        --! Valid data to be read from read data
        oValid : out std_logic
    );
end spiSlave;
