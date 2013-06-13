-------------------------------------------------------------------------------
--! @file tbEdgedetectorBhv.vhd
--
--! @brief Edgedetector testbench
--
--! @details The testbench verifies if the edgedetector operates correctly.
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

entity tbEdgedetector is
end tbEdgedetector;

architecture bhv of tbEdgedetector is
    signal clk : std_logic;
    signal rst : std_logic;
    signal done : std_logic;
    signal error : std_logic;

    signal enable : std_logic;
    signal data : std_logic;
    signal rising : std_logic;
    signal falling : std_logic;
    signal any : std_logic;
begin
    DUT : entity work.edgedetector
        port map (
            iArst => rst,
            iClk => clk,
            iEnable => enable,
            iData => data,
            oRising => rising,
            oFalling => falling,
            oAny => any
        );

    stim : process
    begin
        done <= cInactivated;
        enable <= cInactivated;
        data <= cInactivated;
        error <= cInactivated;
        wait until rst = cInactivated;
        wait until rising_edge(clk);

        --generate edge, but don't enable detector
        data <= cActivated;
        wait until falling_edge(clk);
        assert (
            (any = cInactivated) or
            (rising = cInactivated) or
            (falling = cInactivated)
        ) report "Disabled detector does detecting!" severity failure;
        wait until rising_edge(clk);

        --enable detector
        enable <= cActivated;
        wait until falling_edge(clk);
        wait until rising_edge(clk);

        --generate falling edge
        data <= cInactivated;
        wait until falling_edge(clk);
        assert (
            (any = cActivated) or
            (rising = cInactivated) or
            (falling = cActivated)
        ) report "Falling edge is not detected!" severity failure;
        wait until rising_edge(clk);

        --generat rising edge
        data <= cActivated;
        wait until falling_edge(clk);
        assert (
            (any = cActivated) or
            (rising = cActivated) or
            (falling = cInactivated)
        ) report "Rising edge is not detected!" severity failure;
        wait until rising_edge(clk);

        --disable detector
        enable <= cInactivated;
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
