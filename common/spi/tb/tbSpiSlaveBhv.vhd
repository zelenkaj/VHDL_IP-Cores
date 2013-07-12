-------------------------------------------------------------------------------
--! @file tbNshiftRegBhv.vhd
--
--! @brief Testbench for shift register with n-bit-width
--
--! @details Testbench that verifies the n-shift-register
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

entity tbSpiSlave is
    generic (
        gRegisterSize   : natural := 8;
        gPolarity       : natural := 0;
        gPhase          : natural := 0;
        gShiftDir       : natural := 1;
        --! Architecture select (0 = rtl_sclk, 1 = rtl_aclk)
        gArchSel        : natural := 0
    );
end tbSpiSlave;

architecture bhv of tbSpiSlave is
    constant cArch_sclk : natural := 0;
    constant cArch_aclk : natural := 1;

    constant cRegisterSize  : natural := gRegisterSize;
    constant cPolarity      : natural := gPolarity;
    constant cPhase         : natural := gPhase;
    constant cShiftDir      : natural := gShiftDir;

    constant cMainClkPeriod : time := 10 ns;
    constant cSpiClkPeriod  : time := 80 ns;
    constant cSpiClkShift   : time := cMainClkPeriod/2;

    --! procedure to shift register
    procedure shiftSpi (
        signal clk          : in std_logic;
        signal spiClk       : inout std_logic;
        signal nSel         : out std_logic;
        variable holdSel    : boolean
    ) is
    begin
        --activate select signal
        nSel <= cnActivated;

        for i in 0 to 2*cRegisterSize-1 loop
            spiClk <= not spiClk;
            wait for cSpiClkPeriod/2;
        end loop;

        if not holdSel then
            --deactivate select signal
            nSel <= cnInactivated;
        end if;
    end procedure;

    --! function to swap vector
    function swapVector (din : std_logic_vector) return std_logic_vector is
        variable vTmp : std_logic_vector(din'range);
    begin
        for i in din'range loop
            vTmp(i) := din(din'length-1-i);
        end loop;

        return vTmp;
    end function;

    signal rst  : std_logic;
    signal clk  : std_logic;
    signal done : std_logic;

    -- DUT signals
    signal spiClk               : std_logic;
    signal nSpiSel              : std_logic;
    signal spiMosi              : std_logic;
    signal spiMiso              : std_logic;
    signal spiMiso_t            : std_logic;
    signal loadData             : std_logic_vector(cRegisterSize-1 downto 0);
    signal load                 : std_logic;
    signal readData             : std_logic_vector(cRegisterSize-1 downto 0);
    signal valid                : std_logic;
    signal validReadData        : std_logic_vector(cRegisterSize-1 downto 0);
    signal validReadData_ref    : std_logic_vector(cRegisterSize-1 downto 0);

    -- stim register signals
    signal stimLoad                 : std_logic;
    signal stimLoadData             : std_logic_vector(cRegisterSize-1 downto 0);
    signal stimReadData             : std_logic_vector(cRegisterSize-1 downto 0);
    signal stimValid                : std_logic;
    signal stimValidReadData        : std_logic_vector(cRegisterSize-1 downto 0);
    signal stimValidReadData_ref    : std_logic_vector(cRegisterSize-1 downto 0);
    signal stimValidReadData_l_ref  : std_logic_vector(cRegisterSize-1 downto 0);

    -- signal to load dut and stim the first time
    signal firstLoad    : std_logic;
    signal check        : std_logic;
begin
    theRstGen : entity work.resetGen
        port map (
            oReset => rst
        );

    theClkGen : entity work.clkGen
        generic map (
            gPeriod => cMainClkPeriod
        )
        port map (
            iDone => done,
            oClk => clk
        );

    syncDut : if gArchSel = cArch_sclk generate
        DUT : entity work.spiSlave(rtl_sclk)
            generic map (
                gRegisterSize   => cRegisterSize,
                gPolarity       => cPolarity,
                gPhase          => cPhase,
                gShiftDir       => cShiftDir
            )
            port map (
                iArst       => rst,
                iClk        => clk,
                iSpiClk     => spiClk,
                inSpiSel    => nSpiSel,
                iSpiMosi    => spiMosi,
                oSpiMiso    => spiMiso,
                oSpiMiso_t  => spiMiso_t,
                iLoadData   => loadData,
                iLoad       => load,
                oReadData   => readData,
                oValid      => valid
            );
    end generate;

    asyncDut : if gArchSel = cArch_aclk generate
        DUT : entity work.spiSlave(rtl_aclk)
            generic map (
                gRegisterSize   => cRegisterSize,
                gPolarity       => cPolarity,
                gPhase          => cPhase,
                gShiftDir       => cShiftDir
            )
            port map (
                iArst       => rst,
                iClk        => clk,
                iSpiClk     => spiClk,
                inSpiSel    => nSpiSel,
                iSpiMosi    => spiMosi,
                oSpiMiso    => spiMiso,
                oSpiMiso_t  => spiMiso_t,
                iLoadData   => loadData,
                iLoad       => load,
                oReadData   => readData,
                oValid      => valid
            );
    end generate;

    controlLoad : process(rst, clk)
    begin
        if rst = cActivated then
            validReadData           <= (others => cInactivated);
            stimValidReadData       <= (others => cInactivated);
            check                   <= cInactivated;
        elsif rising_edge(clk) then
            -- check one cycle after valid data arrived at dut
            check <= valid;

            if valid = cActivated then
                validReadData <= readData;
            end if;
            if stimValid = cActivated then
                if gShiftDir /= 0 then
                    stimValidReadData   <= stimReadData;
                else
                    stimValidReadData   <= swapVector(stimReadData);
                end if;
            end if;
        end if;
    end process;

    load        <= valid or firstLoad;
    stimLoad    <= stimValid or firstLoad;

    theStim : process
        variable vHoldSelect : boolean;
    begin
        done <= cInactivated;
        if cPolarity = 0 then
            spiClk <= cInactivated;
        else
            spiClk <= cActivated;
        end if;
        nSpiSel         <= cnInactivated;
        firstLoad       <= cInactivated;
        wait until rst = cInactivated;
        wait until rising_edge(clk);

        -- do a first load
        wait until rising_edge(clk);
        firstLoad       <= cActivated;
        wait until rising_edge(clk);
        firstLoad       <= cInactivated;

        -- wait for main clock
        wait until rising_edge(clk);
        wait until rising_edge(clk);
        wait until rising_edge(clk);

        --wait for clock shift time
        wait for cSpiClkShift;

        vHoldSelect := TRUE;
        for i in 0 to cRegisterSize-1 loop
            if i = cRegisterSize-1 then
                vHoldSelect := FALSE;
            end if;
            shiftSpi(clk, spiClk, nSpiSel, vHoldSelect);
        end loop;

        done <= cActivated;

        wait;
    end process;

    theCheck : process(rst, clk)
        variable vTmp : std_logic_vector(cRegisterSize-1 downto 0);
    begin
        if rst = cActivated then
            vTmp                    := (others => cInactivated);
            vTmp(vTmp'left)         := cActivated;
            stimLoadData            <= vTmp;
            validReadData_ref       <= vTmp;

            vTmp                    := (others => cInactivated);
            vTmp(0)                 := cActivated;
            loadData                <= vTmp;
            stimValidReadData_ref   <= vTmp;
            stimValidReadData_l_ref <= (others => cInactivated);
        elsif rising_edge(clk) then
            if load = cActivated then
                loadData    <= loadData(loadData'left-1 downto 0) & cInactivated;
            end if;

            if stimLoad = cActivated then
                stimLoadData    <= cInactivated & stimLoadData(stimLoadData'left downto 1);
            end if;

            --check if dut's read data is correct
            if check = cActivated then
                stimValidReadData_ref   <= stimValidReadData_ref(loadData'left-1 downto 0) & cInactivated;
                stimValidReadData_l_ref <= stimValidReadData_ref;
                validReadData_ref       <= cInactivated & validReadData_ref(stimLoadData'left downto 1);

                assert (validReadData_ref = validReadData)
                report "Stimulated data does not arrive at DUT! " &
                        integer'image(to_integer(unsigned(validReadData_ref))) &
                        " /= " &
                        integer'image(to_integer(unsigned(validReadData)))
                severity failure;

                case gArchSel is
                    when cArch_sclk =>
                        assert (stimValidReadData_ref = stimValidReadData)
                        report "Stimulated data does not arrive at STIM! " &
                                integer'image(to_integer(unsigned(stimValidReadData_ref))) &
                                " /= " &
                                integer'image(to_integer(unsigned(stimValidReadData)))
                        severity failure;
                    when cArch_aclk =>
                        assert (stimValidReadData_l_ref = stimValidReadData)
                        report "Stimulated data does not arrive at STIM! " &
                                integer'image(to_integer(unsigned(stimValidReadData_l_ref))) &
                                " /= " &
                                integer'image(to_integer(unsigned(stimValidReadData)))
                        severity failure;
                    when others =>
                        assert FALSE
                        report "Wrong architecture selected!"
                        severity failure;
                end case;
            end if;
        end if;
    end process;

    theSpiMaster : block
        signal spiClk_rising : std_logic;
        signal spiClk_falling : std_logic;
        signal spiCapture : std_logic;
        signal spiShift : std_logic;
        signal capReg : std_logic;
        signal shiftReg : std_logic_vector(cRegisterSize-1 downto 0);
        signal cnt : natural;
    begin
        edgeDet : entity work.edgedetector
            port map (
                iArst       => rst,
                iClk        => clk,
                iEnable     => cActivated,
                iData       => spiClk,
                oRising     => spiClk_rising,
                oFalling    => spiClk_falling,
                oAny        => open
            );

        stimReadData <= shiftReg when cPhase = 0 else
                        shiftReg(shiftReg'left-1 downto 0) & capReg;

        -- borrowed from spiSlaveRtl.vhd!
        assignEdge : process (
            spiClk_rising,
            spiClk_falling
        )
        begin
            if cPhase = 0 and cPolarity = 0 then
                spiShift <= spiClk_falling;
                spiCapture <= spiClk_rising;
            elsif cPhase = 0 and cPolarity = 1 then
                spiShift <= spiClk_rising;
                spiCapture <= spiClk_falling;
            elsif cPhase = 1 and cPolarity = 0 then
                spiShift <= spiClk_rising;
                spiCapture <= spiClk_falling;
            elsif cPhase = 1 and cPolarity = 1 then
                spiShift <= spiClk_falling;
                spiCapture <= spiClk_rising;
            else
                assert (FALSE)
                report "Polarity/Phase setting should be 0 or 1!" severity failure;
            end if;
        end process;

        spiMosi <= shiftReg(shiftReg'left);

        regClk : process(rst, clk)
            variable vTmp : std_logic_vector(stimLoadData'range);
        begin
            if rst = cActivated then
                capReg <= cInactivated;
                shiftReg <= (others => cInactivated);
                cnt <= 0;
                stimValid <= cInactivated;
            elsif rising_edge(clk) then
                stimValid <= cInactivated;
                if gShiftDir = 0 then
                    vTmp := swapVector(stimLoadData);
                else
                    vTmp := stimLoadData;
                end if;

                if nSpiSel = cnInactivated then
                    cnt <= 0;
                end if;

                if stimLoad = cActivated then
                    if cPhase = 0 then
                        shiftReg <= vTmp;
                    else
                        capReg <= vTmp(0);
                        shiftReg <= cInactivated & vTmp(stimLoadData'left downto 1);
                    end if;
                elsif spiCapture = cActivated then
                    capReg <= spiMiso;
                    if gPhase = 1 then
                        if cnt < cRegisterSize-1 then
                            cnt <= cnt + 1;
                        else
                            cnt <= 0;
                            stimValid <= cActivated;
                        end if;
                    end if;
                elsif spiShift = cActivated then
                    shiftReg <= shiftReg(shiftReg'left-1 downto 0) & capReg;
                    if gPhase = 0 then
                        if cnt < cRegisterSize-1 then
                            cnt <= cnt + 1;
                        else
                            cnt <= 0;
                            stimValid <= cActivated;
                        end if;
                    end if;
                end if;
            end if;
        end process;
    end block;
end bhv;
