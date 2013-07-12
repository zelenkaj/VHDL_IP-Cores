-------------------------------------------------------------------------------
--! @file spiSlave-rtl_aclk-a.vhd
--
--! @brief SPI Slave asynchrounous clock architecture
--
--! @details This architecture implements an SPI Slave with asynchronous clock
--! domains. The bus signals are transferred to the SPI clock domain and
--! vice versa.
--! Note that parallel data loads are done with the SPI clock edges. Therefore
--! parallel loads are delayed by one frame!
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

architecture rtl_aclk of spiSlave is
    --! Spi Polarity
    constant cPolarity      : natural := gPolarity;
    --! Spi Phase
    constant cPhase         : natural := gPhase;

    --! Sync stages between spi clock and bus clock
    constant cSyncStages    : natural := 2;

    --! function to swap vector
    function swapVector (din : std_logic_vector) return std_logic_vector is
        variable vTmp : std_logic_vector(din'range);
    begin
        for i in din'range loop
            vTmp(i) := din(din'length-1-i);
        end loop;

        return vTmp;
    end function;

    --! Function to get shift edge
    function getShiftEdge (pol : natural; pha : natural) return std_logic is
    begin
        if pol = pha then
            return cInactivated;
        else
            return cActivated;
        end if;
    end function;

    --! Edge state for shifting
    constant cShift         : std_logic := getShiftEdge(cPolarity, cPhase);
    --! Edge state for capturing
    constant cCapture       : std_logic := not getShiftEdge(cPolarity, cPhase);

    --! Spi Register
    signal spiReg           : std_logic_vector(gRegisterSize-1 downto 0);
    --! Spi Register next
    signal spiReg_next      : std_logic_vector(gRegisterSize-1 downto 0);
    --! Alias for spiReg shift vector
    alias spiReg_sft        : std_logic_vector(spiReg'left-1 downto 0) is
                                spiReg(spiReg'left-1 downto 0);

    --! Spi Capture Register
    signal spiCap           : std_logic;
    --! Spi Capture Register next
    signal spiCap_next      : std_logic;

    --! Frame counter
    signal frmCnt           : std_logic_vector(logDualis(gRegisterSize)-1 downto 0);
    --! Frame counter next
    signal frmCnt_next      : std_logic_vector(logDualis(gRegisterSize)-1 downto 0);
    --! Terminal count value
    constant cFrmCnt_tc     : std_logic_vector(frmCnt'range) :=
        std_logic_vector(to_unsigned(gRegisterSize-1, logDualis(gRegisterSize)));
    --! Terminal count
    signal frmCnt_tc        : std_logic;
    --! Count value is zero
    signal frmCnt_zero      : std_logic;
    --! Counter value zero
    constant cFrmCnt_zero   : std_logic_vector(frmCnt'range) :=
        (others => cInactivated);
    --! Counter value to get load value
    constant cFrmCnt_load   : std_logic_vector(frmCnt'range) :=
        std_logic_vector(to_unsigned(gRegisterSize-2,frmCnt'length));

    --! Output register
    signal outReg           : std_logic_vector(gRegisterSize-1 downto 0);
    --! Output register next
    signal outReg_next      : std_logic_vector(gRegisterSize-1 downto 0);

    --! Load register (at bus side)
    signal loadReg          : std_logic_vector(gRegisterSize-1 downto 0);
    --! Load register next
    signal loadReg_next     : std_logic_vector(gRegisterSize-1 downto 0);

    --! Spi load register (at spi side)
    signal spiLoadReg       : std_logic_vector(gRegisterSize-1 downto 0);
    --! Spi load register next
    signal spiLoadReg_next  : std_logic_vector(gRegisterSize-1 downto 0);

    --! Terminal count transferred to bus clock domain
    signal frmCnt_tc_tf     : std_logic;
    --! Load register transferred to spi clock domain
    signal loadReg_tf       : std_logic_vector(gRegisterSize-1 downto 0);
begin
    oReadData   <= swapVector(outReg) when gShiftDir = 0 else outReg;
    oSpiMiso    <= spiReg(spiReg'left);
    oSpiMiso_t  <= cActivated when inSpiSel = cnActivated else cInactivated;

    --! Spi Register clock process
    spiRegProc : process(iArst, iSpiClk)
    begin
        if iArst = cActivated then
            spiReg      <= (others => cInactivated);
            spiLoadReg  <= (others => cInactivated);
            spiCap      <= cInactivated;
            outReg      <= (others => cInactivated);
        elsif iSpiClk = cShift and iSpiClk'event then
            spiReg      <= spiReg_next;
            spiLoadReg  <= spiLoadReg_next;
            if cPhase = 0 then
                outReg  <= outReg_next;
            end if;
        elsif iSpiClk = cCapture and iSpiClk'event then
            spiCap      <= spiCap_next;
            if cPhase /= 0 then
                outReg  <= outReg_next;
            end if;
        end if;
    end process;

    --! Frame Counter Register clock prosess
    frmCntProc : process(inSpiSel, iSpiClk)
    begin
        if inSpiSel = cnInactivated then
            frmCnt <= (others => cInactivated);
        elsif iSpiClk = cShift and iSpiClk'event then
            if cPhase = 0 then
                frmCnt <= frmCnt_next;
            end if;
        elsif iSpiClk = cCapture and iSpiClk'event and cPhase /= 0 then
            if cPhase /= 0 then
                frmCnt <= frmCnt_next;
            end if;
        end if;
    end process;

    -- Assign frame terminal count
    frmCnt_tc <=    cActivated when frmCnt = cFrmCnt_tc else
                    cInactivated;

    -- Assign frame count zero
    frmCnt_zero <=  cActivated when frmCnt = cFrmCnt_zero else
                    cInactivated;

    --! Spi Register combinatoric process
    spiCombProc : process (
        inSpiSel,
        iSpiMosi,
        spiReg,
        spiLoadReg,
        spiCap,
        outReg,
        frmCnt,
        frmCnt_tc,
        frmCnt_zero,
        loadReg_tf
    )
    begin
        --default to avoid latches
        spiReg_next     <= spiReg;
        spiLoadReg_next <= spiLoadReg;
        frmCnt_next     <= frmCnt;
        outReg_next     <= outReg;

        -- Spi capture register assigned to Mosi
        spiCap_next     <= iSpiMosi;

        -- Spi Sel enables processes
        if inSpiSel = cnActivated then
            -- Shift captured data into shift register
            spiReg_next <= spiReg_sft & spiCap;

            if cPhase = 0 then
                -- In case of phase 0 data is captured first, then shifted.
                -- Hence, received data output and load new data can be done
                -- with last shifting clock edge
                if frmCnt_tc = cActivated then
                    spiReg_next <= spiLoadReg;
                    outReg_next <= spiReg_sft & spiCap;
                end if;
            else
                -- In case of phase 1 data is shifted first, then captured.
                -- Hence, received data output can be done with last capturing
                -- clock edge.
                if frmCnt_tc = cActivated then
                    outReg_next <= spiReg_sft & iSpiMosi;
                end if;

                -- But load new data has to be done with first shifting clock
                -- edge!
                if frmCnt_zero = cActivated then
                    spiReg_next <= spiLoadReg;
                end if;
            end if;

            frmCnt_next         <= std_logic_vector(unsigned(frmCnt) + 1);

            -- Load transferred load register into spi load register
            if frmCnt = cFrmCnt_load then
                spiLoadReg_next <= loadReg_tf;
            end if;
        end if;
    end process;

    busRegProc : process(iArst, iClk)
    begin
        if iArst = cActivated then
            loadReg <= (others => cInactivated);
        elsif rising_edge(iClk) then
            loadReg <= loadReg_next;
        end if;
    end process;

    busCombProc : process (
        iLoad,
        loadReg
    )
    begin
        --default to avoid latches
        loadReg_next <= loadReg;

        if iLoad = cActivated then
            if gShiftDir = 0 then
                loadReg_next <= swapVector(iLoadData);
            else
                loadReg_next <= iLoadData;
            end if;
        end if;
    end process;

    theSpiTcSync : entity work.synchronizer
        generic map (
            gStages => cSyncStages,
            gInit   => cInactivated
        )
        port map (
            iClk    => iClk,
            iArst   => iArst,
            iAsync  => frmCnt_tc,
            oSync   => frmCnt_tc_tf
        );

    theSpiTcEdgedet : entity work.edgedetector
        port map (
            iClk        => iClk,
            iArst       => iArst,
            iEnable     => cActivated,
            iData       => frmCnt_tc_tf,
            oRising     => open,
            oFalling    => oValid,
            oAny        => open
        );

    genTheLoadSync : for i in loadReg'range generate
        theLoadSync : entity work.synchronizer
            generic map (
                gStages => cSyncStages,
                gInit   => cInactivated
            )
            port map (
                iClk    => iSpiClk,
                iArst   => iArst,
                iAsync  => loadReg(i),
                oSync   => loadReg_tf(i)
            );
    end generate;
end rtl_aclk;
