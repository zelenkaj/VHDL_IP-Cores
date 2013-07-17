-------------------------------------------------------------------------------
--! @file tbTripleLogicBhv.vhd
--
--! @brief Triple Buffer testbench
--
--! @details The testbench verifies if the triple buffer operates correctly.
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
use work.tripleBufferPkg.all;

entity tbTripleLogic is
end tbTripleLogic;

architecture bhv of tbTripleLogic is
    signal clk          : std_logic;
    signal rst          : std_logic;
    signal done         : std_logic;
    signal detErr_col   : std_logic := cInactivated;
    signal detErr_new   : std_logic := cInactivated;

    signal stimVec          : std_logic_vector(1 downto 0);
    alias pro_trig          : std_logic is stimVec(1);
    alias con_trig          : std_logic is stimVec(0);
    signal stimVec_ones     : std_logic;
    signal stimVec_ones_l   : std_logic;

    signal con_sel          : tTripleSel;
    signal pro_sel          : tTripleSel;
    signal pro_sel_l        : tTripleSel;
begin
    DUT : entity work.tripleLogic
        port map (
            iRst        => rst,
            iClk        => clk,
            iPro_trig   => pro_trig,
            oPro_sel    => pro_sel,
            iCon_trig   => con_trig,
            oCon_sel    => con_sel
        );

    detErr_col <=   cActivated when rst = cInactivated and
                                    falling_edge(clk) and
                                    con_sel = pro_sel else
                    cInactivated;

    detErr_new <=   cActivated when rst = cInactivated and
                                    falling_edge(clk) and
                                    stimVec_ones_l = cActivated and
                                    con_sel /= pro_sel_l else
                    cInactivated;

    assert (detErr_col /= cActivated)
        report "Triple Buffer collision happend!"
        severity failure;

    assert (detErr_new /= cActivated)
        report "Consumer has not changed to latest buffer!"
        severity failure;

    lagStims : process(rst, clk)
    begin
        if rising_edge(clk) then
            stimVec_ones_l <= stimVec_ones;
            pro_sel_l <= pro_sel;
        end if;
    end process;

    stimVec_ones <= cActivated when stimVec = "11" else cInactivated;

    theStim : process
    begin
        done        <= cInactivated;
        pro_trig    <= cInactivated;
        con_trig    <= cInactivated;
        wait until rst = cInactivated;
        wait until rising_edge(clk);

        stimVec <= "10";
        wait until rising_edge(clk);

        stimVec <= "01";
        wait until rising_edge(clk);

        stimVec <= "10";
        wait until rising_edge(clk);

        stimVec <= "10";
        wait until rising_edge(clk);

        stimVec <= "00";
        wait until rising_edge(clk);

        stimVec <= "10";
        wait until rising_edge(clk);

        stimVec <= "01";
        wait until rising_edge(clk);

        stimVec <= "01";
        wait until rising_edge(clk);

        stimVec <= "01";
        wait until rising_edge(clk);

        stimVec <= "11";
        wait until rising_edge(clk);

        stimVec <= "11";
        wait until rising_edge(clk);

        stimVec <= "01";
        wait until rising_edge(clk);

        stimVec <= "00";
        wait until rising_edge(clk);

        stimVec <= "00";
        wait until rising_edge(clk);

        stimVec <= "11";
        wait until rising_edge(clk);

        stimVec <= "00";
        wait until rising_edge(clk);

        done <= cActivated;
        wait;
    end process;

    theClkGen : entity work.clkgen
        generic map (
            gPeriod => 10 ns
        )
        port map (
            iDone => done,
            oClk => clk
        );

    theRstGen : entity work.resetGen
        generic map (
            gResetTime => 100 ns
        )
        port map (
            oReset => rst,
            onReset => open
        );
end bhv;
