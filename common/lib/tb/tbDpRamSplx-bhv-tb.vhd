-------------------------------------------------------------------------------
--! @file tbDpRamSplx-bhv-tb.vhd
--
--! @brief Dpram testbench
--
--! @details The testbench verifies if the dpram.
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

library work;
use work.global.all;

entity tbDpRamSplx is
    generic (
        --! Data width port A [bit]
        gWordWidthA     : natural := 32;
        --! Number of words port A
        gNumberOfWordsA : natural := 1024;
        --! Data width port B [bit]
        gWordWidthB     : natural := 32;
        --! Number of words port B
        gNumberOfWordsB : natural := 1024;
        --! Initialization file
        gInitFile       : string := "unused";
        --! Stimuli file
        gStimFile       : string := "stim.txt"
    );
end tbDpRamSplx;

architecture bhv of tbDpRamSplx is
    --! Address width of bus master
    constant cBusMasterAddrWidth : natural := logDualis(gNumberOfWordsA);
    --! Data width of bus master
    constant cBusMasterDataWidth : natural := gWordWidthA;

    --! Type for dut connection
    type tDut is record
        write       : std_logic;
        address     : std_logic_vector(logDualis(gNumberOfWordsA)-1 downto 0);
        byteenable  : std_logic_vector(gWordWidthA/8-1 downto 0);
        writedata   : std_logic_vector(gWordWidthA-1 downto 0);
        readdata    : std_logic_vector(gWordWidthA-1 downto 0);
    end record;
    --! Type for stimuli connection
    type tStim is record
        write       : std_logic;
        read        : std_logic;
        address     : std_logic_vector(cBusMasterAddrWidth-1 downto 0);
        byteenable  : std_logic_vector(cBusMasterDataWidth/8-1 downto 0);
        writedata   : std_logic_vector(cBusMasterDataWidth-1 downto 0);
        readdata    : std_logic_vector(cBusMasterDataWidth-1 downto 0);
        ack         : std_logic;
    end record;
    --! Type for ack counter
    type tAckCnt is record
        enable  : std_logic;
        tcnt    : std_logic;
    end record;

    --! Clock
    signal clk      : std_logic;
    --! Reset
    signal rst      : std_logic;
    --! Simulation done
    signal done     : std_logic;
    --! Simulation error
    signal error    : std_logic;

    --! DUT port
    signal dut      : tDut;
    --! Stim port
    signal stim     : tStim;
    --! Read ack
    signal readAck  : tAckCnt;
    --! Write ack
    signal writeAck : tAckCnt;
begin
    assert (error /= cActivated)
    report "Bus master reports error due to assertion!"
    severity failure;

    --TODO: Enable supporting different port width
    assert (gWordWidthA = gWordWidthB)
    report "This testbench only supports same word width on port A and B!"
    severity failure;

    --! The device under test (DUT)
    theDUT : entity work.dpRamSplx
        generic map (
            gWordWidthA     => gWordWidthA,
            gNumberOfWordsA => gNumberOfWordsA,
            gWordWidthB     => gWordWidthB,
            gNumberOfWordsB => gNumberOfWordsB,
            gInitFile       => gInitFile
        )
        port map (
            iClk_A          => clk,
            iEnable_A       => cActivated,
            iWriteEnable_A  => dut.write,
            iAddress_A      => dut.address,
            iByteenable_A   => dut.byteenable,
            iWritedata_A    => dut.writedata,
            iClk_B          => clk,
            iEnable_B       => cActivated,
            iAddress_B      => dut.address,
            oReaddata_B     => dut.readdata
        );

    -- map stim to dut
    --- fixed connections
    dut.write       <= stim.write;
    dut.address     <= stim.address(dut.address'range);
    dut.byteenable  <= stim.byteenable;
    dut.writedata   <= stim.writedata;
    stim.readdata   <= dut.readdata;

    --! The testbench stimuli is done by the bus master.
    theSTIM : entity work.busMaster
        generic map (
            gAddrWidth      => cBusMasterAddrWidth,
            gDataWidth      => cBusMasterDataWidth,
            gStimuliFile    => gStimFile
        )
        port map (
            iRst        => rst,
            iClk        => clk,
            iEnable     => cActivated,
            iAck        => stim.ack,
            iReaddata   => stim.readdata,
            oWrite      => stim.write,
            oRead       => stim.read,
            oSelect     => open,
            oAddress    => stim.address,
            oByteenable => stim.byteenable,
            oWritedata  => stim.writedata,
            oError      => error,
            oDone       => done
        );

    -- map acks to stim
    stim.ack        <= readAck.tcnt or writeAck.tcnt;
    readAck.enable  <= stim.read;
    writeAck.enable <= stim.write;

    --! Read acknowlegde is generate with one cycle delay.
    theREADACK : entity work.cnt
        generic map (
            gCntWidth   => 1,
            gTcntVal    => 1
        )
        port map (
            iArst   => rst,
            iClk    => clk,
            iEnable => readAck.enable,
            iSrst   => cInactivated,
            oCnt    => open,
            oTcnt   => readAck.tcnt
        );

    --! Write acknowlegde is generate with no cycle delay.
    theWRITEACK : writeAck.tcnt <= writeAck.enable;

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
