-------------------------------------------------------------------------------
--! @file alteraSpiBridgeRtl.vhd
--
--! @brief SPI to bus interface bridge for Altera platform
--
--! @details The SPI to bus interface bridge translates the data received at
--! SPI to master transactions.
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

entity alteraSpiBridge is
    generic (
        -- SPI slave settings
        --! SPI register size (= frame size)
        gRegisterSize : natural := 8;
        --! SPI clock polarity (allowed values 0 and 1)
        gPolarity : natural := 0;
        --! SPI clock phase (allowed values 0 and 1)
        gPhase : natural := 0;
        --! Shift direction (0 = LSB first, otherwise = MSB first)
        gShiftDir : natural := 0;
        -- Bus master settings
        --! Bus interface data width
        gBusDataWidth : natural := 32;
        --! Bus interface address width
        gBusAddrWidth : natural := 8;
        --! Write buffer base
        gWrBufBase : natural := 16#00#;
        --! Write buffer size
        gWrBufSize : natural := 128;
        --! Read buffer base
        gRdBufBase : natural := 16#80#;
        --! Read buffer size
        gRdBufSize : natural := 128
    );
    port (
        --! Reset Source input
        rsi_r0_reset : in std_logic;
        --! Clock Source input
        csi_c0_clock : in std_logic;
        -- SPI
        --! SPI clock
        coe_spi_clk : in std_logic;
        --! SPI select (low-active)
        coe_spi_sel_n : in std_logic;
        --! SPI master-out-slave-in
        coe_spi_mosi : in std_logic;
        --! SPI master-in-slave-out
        coe_spi_miso : out std_logic;
        -- Bus master
        --! Avalon-MM master bridge address
        avm_bridge_address : out std_logic_vector(gBusAddrWidth-1 downto 0);
        --! Avalon-MM master bridge byteenable
        avm_bridge_byteenable : out std_logic_vector(gBusDataWidth/8-1 downto 0);
        --! Avalon-MM master bridge write
        avm_bridge_write : out std_logic;
        --! Avalon-MM master bridge writedata
        avm_bridge_writedata : out std_logic_vector(gBusDataWidth-1 downto 0);
        --! Avalon-MM master bridge read
        avm_bridge_read : out std_logic;
        --! Avalon-MM master bridge readdata
        avm_bridge_readdata : in std_logic_vector(gBusDataWidth-1 downto 0);
        --! Avalon-MM master bridge waitrequest
        avm_bridge_waitrequest : in std_logic;
        --! Interrupt receiver
        inr_sync_irq : in std_logic
    );
end alteraSpiBridge;

architecture rtl of alteraSpiBridge is
    --! Spi miso output
    signal spiMiso : std_logic;
    --! Spi miso output enable
    signal spiMiso_t : std_logic;
begin
    theSpiBridge : entity work.spiBridge
        generic map (
            gRegisterSize => gRegisterSize,
            gPolarity => gPolarity,
            gPhase => gPhase,
            gShiftDir => gShiftDir,
            gBusDataWidth => gBusDataWidth,
            gBusAddrWidth => gBusAddrWidth,
            gWrBufBase => gWrBufBase,
            gWrBufSize => gWrBufSize,
            gRdBufBase => gRdBufBase,
            gRdBufSize => gRdBufSize
        )
        port map (
            iArst => rsi_r0_reset,
            iClk => csi_c0_clock,
            iSpiClk => coe_spi_clk,
            inSpiSel => coe_spi_sel_n,
            iSpiMosi => coe_spi_mosi,
            oSpiMiso => spiMiso,
            oSpiMiso_t => spiMiso_t,
            oBusAddress => avm_bridge_address,
            oBusWrite => avm_bridge_write,
            oBusWritedata => avm_bridge_writedata,
            oBusRead => avm_bridge_read,
            iBusReaddata => avm_bridge_readdata,
            iBusWaitrequest => avm_bridge_waitrequest,
            iSrst => inr_sync_irq
        );

    -- miso output driver
    coe_spi_miso <= spiMiso when spiMiso_t = cActivated else 'Z';

    -- enable always all available bytes
    avm_bridge_byteenable <= (others => cActivated);
end rtl;
