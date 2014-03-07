-------------------------------------------------------------------------------
--! @file tbDataLatch-bhv-tb.vhd
--! @brief Data Latch testbench
-------------------------------------------------------------------------------
--
--    (c) B&R, 2014
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
--! Use standard ieee library
library ieee;
--! Use logic elements
use ieee.std_logic_1164.all;

--! Utility library
library libutil;

--! Use libcommon library
library libcommon;
--! Use global package
use libcommon.global.all;

entity tbDataLatch is
end tbDataLatch;

architecture bhv of tbDataLatch is
    --! Latch data width
    constant cDataWidth     : natural := 8;
    --! Test word to be written to latch
    constant cTestWord      : std_logic_vector := x"AB";
    --! Test word all zeros
    constant cTestAllZeros  : std_logic_vector := x"00";
    --! Test clock period
    constant cPeriod        : time := 20 ns;

    --! Port mapping type for dut inputs
    type tDutIn is record
        clear   : std_logic;
        enable  : std_logic;
        data    : std_logic_vector(cDataWidth-1 downto 0);
    end record;

    --! Dut inputs initialize
    constant cDutIninit : tDutIn := (
        clear => cInactivated, enable => cInactivated, data => cTestAllZeros
    );

    --! Port mapping type for dut outputs
    type tDutOut is record
        data    : std_logic_vector(cDataWidth-1 downto 0);
    end record;

    --! Stim array
    type tStimArray is array (natural range <>) of tDutIn;
    --! Out array
    type tOutArray is array (natural range <>) of tDutOut;

    --! Stimuli sequence
    constant cStimSeq : tStimArray(0 to 4) := (
        ( clear => cActivated,   enable => cInactivated, data => cTestAllZeros ),
        ( clear => cInactivated, enable => cInactivated, data => cTestWord     ),
        ( clear => cInactivated, enable => cActivated,   data => cTestWord     ),
        ( clear => cInactivated, enable => cInactivated, data => cTestAllZeros ),
        ( clear => cActivated,   enable => cInactivated, data => cTestAllZeros )
    );

    --! Reference output
    constant cOutRef : tOutArray(cStimSeq'range) := (
        ( data => cTestAllZeros ),
        ( data => cTestAllZeros ),
        ( data => cTestWord     ),
        ( data => cTestWord     ),
        ( data => cTestAllZeros )
    );

    --! Test clock
    signal clk          : std_logic;
    --! Done signal
    signal done         : std_logic;
    --! Stim counter
    signal stimCnt      : natural := 0;
    --! Check counter
    signal checkCnt     : natural := 0;
    --! Dut inputs
    signal inst_dutIn   : tDutIn := cDutIninit;
    --! Dut outputs
    signal inst_dutOut  : tDutOut;
begin
    DUT : entity work.dataLatch
        generic map (
            gDataWidth => cDataWidth
        )
        port map (
            iClear  => inst_dutIn.clear,
            iEnable => inst_dutIn.enable,
            iData   => inst_dutIn.data,
            oData   => inst_dutOut.data
        );

    done <= cActivated when stimCnt = cStimSeq'length and checkCnt = cOutRef'length else
            cInactivated;

    inst_dutIn <= cStimSeq(stimCnt) when stimCnt < cStimSeq'length else
                  cDutIninit;

    STIM : process(clk)
    begin
        if rising_edge(clk) then
            if stimCnt < cStimSeq'length then
                stimCnt <= stimCnt + 1;
            end if;
        end if;
    end process STIM;

    CHECK : process(clk)
    begin
        if falling_edge(clk) then
            if checkCnt < cOutRef'length then
                assert (inst_dutOut = cOutRef(checkCnt))
                    report "Dut output does not match cOutRef!"
                    severity error;
                checkCnt <= checkCnt + 1;
            end if;
        end if;
    end process CHECK;

    CLK_GEN : entity libutil.clkgen
        generic map (
            gPeriod => cPeriod
        )
        port map (
            iDone   => done,
            oClk    => clk
         );
end bhv;
