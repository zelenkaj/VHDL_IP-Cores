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

entity tbNshiftReg is
end tbNshiftReg;

architecture bhv of tbNshiftReg is
    constant cWidth : natural := 8;
    constant cTabs : natural := 4;

    constant cTestPattern : std_logic_vector(31 downto 0) := x"12345678";

    signal rst, clk : std_logic;
    signal done : std_logic;
    signal load, shift : std_logic;
    signal inData, outData : std_logic_vector(cWidth-1 downto 0);
    signal inLoadData, parOut : std_logic_vector(cWidth*cTabs-1 downto 0);
    signal count : natural;
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

    inLoadData <= cTestPattern(inLoadData'range);

    theStim : process
    begin
        load <= cInactivated;
        shift <= cInactivated;
        count <= 0;
        done <= cInactivated;
        wait until rst = cInactivated;
        wait until rising_edge(clk);
        load <= cActivated;
        wait until rising_edge(clk);
        load <= cInactivated;
        shift <= cActivated;
        while count < cTabs-1 loop
            shift <= cActivated;
            wait until rising_edge(clk);
            count <= count + 1;
        end loop;
        shift <= cInactivated;
        wait until rising_edge(clk);
        assert (inLoadData = parOut) report
            "Shift result is wrong!" severity failure;
        done <= cActivated;
        wait;
    end process;

    --ring
    inData <= outData;

    theDUT : entity work.nShiftReg
        generic map (
            gWidth => cWidth,
            gTabs => cTabs,
            gShiftDir => "left"
        )
        port map (
            iArst => rst,
            iClk => clk,
            iLoad => load,
            iShift => shift,
            iLoadData => inLoadData,
            oParData => parOut,
            iData => inData,
            oData => outData
        );
end bhv;
