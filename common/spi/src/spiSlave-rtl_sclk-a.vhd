-------------------------------------------------------------------------------
--! @file spiSlave-rtl_sclk-a.vhd
--
--! @brief SPI Slave single clock architecture
--
--! @details This architecture implements an SPI Slave with single clock domain.
--! The Spi signals are sampled with the bus interface clock (iClk).
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

architecture rtl_sclk of spiSlave is
    --! function to swap vector
    function swapVector (din : std_logic_vector) return std_logic_vector is
        variable vTmp : std_logic_vector(din'range);
    begin
        for i in din'range loop
            vTmp(i) := din(din'length-1-i);
        end loop;

        return vTmp;
    end function;

    --! Number of synchronizer stages
    constant cSyncStages : natural := 2;
    --! Synchronized SPI clock
    signal spiClk : std_logic;
    --! Synchronized SPI select (low-active)
    signal nSpiSel : std_logic;
    --! Synchronized SPI mosi
    signal spiMosi : std_logic;
    --! SPI miso
    signal spiMiso : std_logic;

    --! SPI select (high-active)
    signal spiSel : std_logic;

    --! SPI clock rising edge
    signal spiClkEdge_ris : std_logic;
    --! SPI clock falling edge
    signal spiClkEdge_fal : std_logic;
    --! SPI clock commands shift
    signal spiShift : std_logic;
    --! SPI clock commands capture
    signal spiCapture : std_logic;

    --! Serial input data
    signal inSerial : std_logic_vector(0 downto 0);
    --! Serial output data
    signal outSerial : std_logic_vector(0 downto 0);
    --! Parallel shift load
    signal loadShiftReg : std_logic_vector(gRegisterSize-1 downto 0);
    --! Parallel shift out
    signal outShiftReg : std_logic_vector(gRegisterSize-1 downto 0);

    --! Capture register
    signal capReg : std_logic;
    --! Load capture register
    signal loadCapReg : std_logic;

    --! Counter enable
    signal cntEnable : std_logic;
    --! Terminal count
    signal tc : std_logic;
begin
    --! The SPI output driver
    oSpiMiso <= spiMiso;
    oSpiMiso_t <=   cActivated when spiSel = cActivated else
                    cInactivated;

    --! Assign shift and capture edge
    assignEdge : process (
        spiClkEdge_ris,
        spiClkEdge_fal
    )
    begin
        if gPhase = 0 and gPolarity = 0 then
            spiShift <= spiClkEdge_fal;
            spiCapture <= spiClkEdge_ris;
        elsif gPhase = 0 and gPolarity = 1 then
            spiShift <= spiClkEdge_ris;
            spiCapture <= spiClkEdge_fal;
        elsif gPhase = 1 and gPolarity = 0 then
            spiShift <= spiClkEdge_ris;
            spiCapture <= spiClkEdge_fal;
        elsif gPhase = 1 and gPolarity = 1 then
            spiShift <= spiClkEdge_fal;
            spiCapture <= spiClkEdge_ris;
        else
            assert (FALSE)
            report "Polarity/Phase setting should be 0 or 1!" severity failure;
        end if;
    end process;

    --! Assign "frame counter" enable to shift or capture flag
    assignCntEnable : process (
        spiShift,
        spiCapture
    )
    begin
        if gPhase = 0 then
            cntEnable <= spiShift;
        elsif gPhase = 1 then
            cntEnable <= spiCapture;
        else
            assert (FALSE)
            report "Phase setting should be 0 or 1!" severity failure;
        end if;
    end process;

    --! Generate valid strobe with one cycle delay.
    genValidStrobe : process(iArst, iClk)
    begin
        if iArst = cActivated then
            oValid <= cInactivated;
        elsif rising_edge(iClk) then
            oValid <= cInactivated;
            if tc = cActivated and cntEnable = cActivated then
                oValid <= cActivated;
            end if;
        end if;
    end process;

    --! Capture register latches mosi input and is loaded with parallel load.
    captureRegister : process(iArst, iClk)
    begin
        if iArst = cActivated then
            capReg <= cInactivated;
        elsif rising_edge(iClk) then
            if iLoad = cActivated then
                capReg <= loadCapReg;
            elsif spiCapture = cActivated then
                capReg <= spiMosi;
            end if;
        end if;
    end process;

    -- capture register feeds shift register
    inSerial(0) <= capReg;

    -- shift register feed miso
    spiMiso <= outSerial(0);

    --! Assign parallel load and output vectors
    assignParallelVec : process (
        iLoadData,
        capReg,
        outShiftReg
    )
        variable vTmp : std_logic_vector(gRegisterSize-1 downto 0);
    begin
        --default

        if gShiftDir = 0 then
            vTmp := swapVector(iLoadData);
        else
            vTmp := iLoadData;
        end if;

        if gPhase = 0 then
            loadShiftReg <= vTmp;
            loadCapReg <= vTmp(0);
            vTmp := outShiftReg;
        else
            loadShiftReg <= cInactivated & vTmp(gRegisterSize-1 downto 1);
            loadCapReg <= vTmp(0);
            vTmp := outShiftReg(outShiftReg'left-1 downto 0) & capReg;
        end if;

        if gShiftDir = 0 then
            oReadData <= swapVector(vTmp);
        else
            oReadData <= vTmp;
        end if;
    end process;

    --! The shift register
    theShiftReg : entity work.nShiftReg
        generic map (
            gWidth => 1,
            gTabs => gRegisterSize,
            gShiftDir => "left"
        )
        port map (
            iArst => iArst,
            iClk => iClk,
            iLoad => iLoad,
            iShift => spiShift,
            iLoadData => loadShiftReg,
            oPardata => outShiftReg,
            iData => inSerial,
            oData => outSerial
        );

    --! Terminal counter used as "frame counter"
    theTermCnt : entity work.cnt
        generic map (
            gCntWidth => LogDualis(gRegisterSize),
            gTcntVal => gRegisterSize-1
        )
        port map (
            iArst => iArst,
            iClk => iClk,
            iEnable => cntEnable,
            iSrst => nSpiSel,
            oCnt => open,
            oTcnt => tc
        );

    -- SPI input synchronizers
    theSpiClkSync : entity work.synchronizer
        generic map (
            gStages => cSyncStages,
            gInit => cInactivated
        )
        port map (
            iClk => iClk,
            iArst => iArst,
            iAsync => iSpiClk,
            oSync => spiClk
        );

    theSpiSelSync : entity work.synchronizer
        generic map (
            gStages => cSyncStages,
            gInit => cnInactivated
        )
        port map (
            iClk => iClk,
            iArst => iArst,
            iAsync => inSpiSel,
            oSync => nSpiSel
        );

    spiSel <= not nSpiSel;

    theSpiMosiSync : entity work.synchronizer
        generic map (
            gStages => cSyncStages,
            gInit => cInactivated
        )
        port map (
            iClk => iClk,
            iArst => iArst,
            iAsync => iSpiMosi,
            oSync => spiMosi
        );

    theSpiClkEdgeDet : entity work.edgedetector
        port map (
            iClk => iClk,
            iArst => iArst,
            iEnable => spiSel,
            iData => spiClk,
            oRising => spiClkEdge_ris,
            oFalling => spiClkEdge_fal,
            oAny => open
        );
end rtl_sclk;
