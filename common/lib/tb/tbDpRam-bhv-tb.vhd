-------------------------------------------------------------------------------
--! @file tbDpRam-bhv-tb.vhd
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

entity tbDpRam is
    generic (
        --! Data width [bit]
        gWordWidth      : natural := 32;
        --! Number of words
        gNumberOfWords  : natural := 1024;
        --! Initialization file
        gInitFile       : string := "unused";
        --! Stimuli file
        gStimFile       : string := "stim.txt"
    );
end tbDpRam;

architecture bhv of tbDpRam is
    --! Address width of bus master (highest bit select port a or b)
    constant cBusMasterAddrWidth : natural := logDualis(gNumberOfWords) + 1;

    --! Type for dut connection (one port)
    type tDut is record
        write       : std_logic;
        address     : std_logic_vector(logDualis(gNumberOfWords)-1 downto 0);
        byteenable  : std_logic_vector(gWordWidth/8-1 downto 0);
        writedata   : std_logic_vector(gWordWidth-1 downto 0);
        readdata    : std_logic_vector(gWordWidth-1 downto 0);
    end record;
    --! Type for stimuli connection
    type tStim is record
        write       : std_logic;
        read        : std_logic;
        address     : std_logic_vector(cBusMasterAddrWidth-1 downto 0);
        byteenable  : std_logic_vector(gWordWidth/8-1 downto 0);
        writedata   : std_logic_vector(gWordWidth-1 downto 0);
        readdata    : std_logic_vector(gWordWidth-1 downto 0);
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

    --! DUT port a
    signal dutA     : tDut;
    --! DUT port b
    signal dutB     : tDut;
    --! Stim port
    signal stim     : tStim;
    --! Read ack
    signal readAck  : tAckCnt;
    --! Write ack
    signal writeAck : tAckCnt;

    --! Alias for selecting port a or b of dut
    alias dutPortSelA : std_logic is stim.address(stim.address'left);
begin
    assert (error /= cActivated)
    report "Bus master reports error due to assertion!"
    severity failure;

    --! The device under test (DUT)
    theDUT : entity work.dpRam
        generic map (
            gWordWidth      => gWordWidth,
            gNumberOfWords  => gNumberOfWords,
            gInitFile       => gInitFile
        )
        port map (
            iClk_A          => clk,
            iWriteEnable_A  => dutA.write,
            iAddress_A      => dutA.address,
            iByteenable_A   => dutA.byteenable,
            iWritedata_A    => dutA.writedata,
            oReaddata_A     => dutA.readdata,
            iClk_B          => clk,
            iWriteEnable_B  => dutB.write,
            iByteenable_B   => dutB.byteenable,
            iAddress_B      => dutB.address,
            iWritedata_B    => dutB.writedata,
            oReaddata_B     => dutB.readdata
        );

    -- map stim to dut
    --- fixed connections
    dutA.address    <= stim.address(dutA.address'range);
    dutA.byteenable <= stim.byteenable;
    dutA.writedata  <= stim.writedata;
    dutB.address    <= stim.address(dutB.address'range);
    dutB.byteenable <= stim.byteenable;
    dutB.writedata  <= stim.writedata;
    --- dynamic connections
    dutA.write      <=  dutPortSelA and stim.write;
    dutB.write      <=  not dutPortSelA and stim.write;
    stim.readdata   <=  dutA.readdata when dutPortSelA = cActivated else
                        dutB.readdata;

    --! The testbench stimuli is done by the bus master.
    theSTIM : entity work.busMaster
        generic map (
            gAddrWidth      => cBusMasterAddrWidth,
            gDataWidth      => gWordWidth,
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
