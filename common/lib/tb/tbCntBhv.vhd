-------------------------------------------------------------------------------
--! @file tbCntBhv.vhd
--
--! @brief Testbench for terminal counter
--
--! @details Testbench that verifies the terminal counter
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

entity tbCnt is
    generic (
        gCntWidth : natural := 8;
        gTcntVal : natural := 4
    );
end tbCnt;

architecture bhv of tbCnt is
    constant cCntWidth : natural := gCntWidth;
    constant cTcntVal : natural := gTcntVal;

    signal rst, clk : std_logic;
    signal done : std_logic;

    signal enable : std_logic;
    signal srst : std_logic;
    signal cnt : std_logic_vector(cCntWidth-1 downto 0);
    signal tcnt : std_logic;
begin
    theRstGen : entity work.resetGen
        port map (
            oReset => rst
        );

    theClkGen : entity work.clkGen
        port map (
            iDone => done,
            oClk => clk
        );

    stim : process
        variable vTmp : natural;
        variable vCnt : natural;
    begin
        done <= cInactivated;
        enable <= cInactivated;
        srst <= cInactivated;
        wait until rst = cInactivated;
        wait until rising_edge(clk);

        assert (cnt = (cnt'range => cInactivated))
        report "Counter is not reset to zero!"
        severity failure;

        assert (tcnt = cInactivated)
        report "Tc is not initialized to zero!"
        severity failure;

        --count to half of terminal count
        vCnt := 0;
        for i in 0 to gTcntVal/2-1 loop
            enable <= cActivated;
            wait until rising_edge(clk);
            vCnt := vCnt + 1;
        end loop;
        enable <= cInactivated;

        wait until rising_edge(clk);

        vTmp := to_integer(unsigned(cnt));
        assert (vTmp = vCnt)
        report "Counter value is wrong! (" & integer'image(vTmp) &
            " /= " & integer'image(vCnt) & ")"
        severity failure;

        --do srst
        wait until rising_edge(clk);
        srst <= cActivated;
        wait until rising_edge(clk);
        srst <= cInactivated;
        wait until rising_edge(clk);

        vTmp := to_integer(unsigned(cnt));
        assert (vTmp = 0)
        report "Sync reset failed! (" & integer'image(vTmp) & ")"
        severity failure;

        --count over tc reset
        vCnt := 0;
        for i in 0 to (gTcntVal*3)/2-1 loop
            enable <= cActivated;
            wait until rising_edge(clk);
            if i = gTcntVal then
                vCnt := 0;
                assert (tcnt = cActivated)
                report "Terminal count output faulty!"
                severity failure;
            else
                vCnt := vCnt + 1;
            end if;
        end loop;
        enable <= cInactivated;
        wait until rising_edge(clk);

        vTmp := to_integer(unsigned(cnt));
        assert (vTmp = vCnt)
        report "Counter value is wrong! (" & integer'image(vTmp) &
            " /= " & integer'image(vCnt) & ")"
        severity failure;

        done <= cActivated;
        wait;
    end process;

    DUT : entity work.cnt
        generic map (
            gCntWidth => cCntWidth,
            gTcntVal => cTcntVal
        )
        port map (
            iArst => rst,
            iClk => clk,
            iEnable => enable,
            iSrst => srst,
            oCnt => cnt,
            oTcnt => tcnt
        );
end bhv;
