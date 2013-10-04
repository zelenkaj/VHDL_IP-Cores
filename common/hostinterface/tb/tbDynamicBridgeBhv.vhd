-------------------------------------------------------------------------------
--! @file tDynamicBridgeBhv.vhd
--
--! @brief Testbench for dynamic bridge
--
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
use work.hostInterfacePkg.all;

entity tbDynamicBridge is
    generic (
        --! Configure dynamic bridge using memory blocks (0 = false)
        gUseMemBlock : natural := 0
    );
end tbDynamicBridge;

architecture bhv of tbDynamicBridge is
    --! Configure dynamic bridge using memory blocks (0 = false)
    constant cUseMemBlock       : natural := gUseMemBlock;
    --! Configure base addresses of bridge input
    constant cBaseAddressArray  : tArrayStd32 := (
        x"0000_0000",
        x"0000_1000",
        x"0000_2000",
        x"0000_4000",
        x"0000_8000"
    );
    --! Configure number of bridge input spaces
    constant cAddressSpaceCount : natural := cBaseAddressArray'length-1;

    --! Bridge input address width
    constant cBridgeInAddrWidth     : natural := 32;
    --! Bridge output address width
    constant cBridgeOutAddrWidth    : natural := 32;
    --! Bridge base set data width
    constant cBaseSetDataWidth      : natural := 32;

    --! Translation addresses write to bridge through baseset
    constant cTransAddressArray     : tArrayStd32 := (
        x"0005_0000",
        x"0008_0000",
        x"0001_0000",
        x"0010_0000"
    );

    --! Bridge signal type
    type tBridge is record
        req     : std_logic;
        inAddr  : std_logic_vector(cBridgeInAddrWidth-1 downto 0);
        sel     : std_logic_vector(cAddressSpaceCount-1 downto 0);
        selAny  : std_logic;
        valid   : std_logic;
        outAddr : std_logic_vector(cBridgeOutAddrWidth-1 downto 0);
    end record;

    --! Bridge base set stim type
    type tBaseSet is record
        write       : std_logic;
        writedata   : std_logic_vector(cBaseSetDataWidth-1 downto 0);
        read        : std_logic;
        byteenable  : std_logic_vector(cBaseSetDataWidth/8-1 downto 0);
        address     : std_logic_vector(logDualis(cAddressSpaceCount)-1 downto 0);
    end record;

    --! Baseset stim initialization
    constant cBaseSetInit   : tBaseSet := (
        write       => cInactivated,
        writedata   => (others => cInactivated),
        read        => cInactivated,
        byteenable  => (others => cInactivated),
        address     => (others => cInactivated)
    );

    --! Simulation done
    signal done             : std_logic;
    --! Simulation error
    signal error            : std_logic;
    --! Start bridge stimulation
    signal startBridgeStim  : std_logic;
    --! Clock
    signal clk              : std_logic;
    --! Reset
    signal rst              : std_logic;
    --! Bridge signals
    signal bridge           : tBridge;
    --! Baseset signals
    signal baseset          : tBaseSet;
    --! Baseset output signal readdata
    signal basesetReaddata  : std_logic_vector(cBaseSetDataWidth-1 downto 0);
    --! Baseset output signal ack
    signal basesetAck       : std_logic;
begin
    --asert
    assert (error /= cActivated)
        report "The bus master reports an error!"
    severity failure;
    --TODO: test bridge.sel
    --TODO: test bridge.selAny

    DUT : entity work.dynamicBridge
        generic map (
            gAddressSpaceCount  => cAddressSpaceCount,
            gUseMemBlock        => cUseMemBlock,
            gBaseAddressArray   => cBaseAddressArray
        )
        port map (
            iClk                => clk,
            iRst                => rst,
            iBridgeAddress      => bridge.inAddr,
            iBridgeRequest      => bridge.req,
            oBridgeAddress      => bridge.outAddr,
            oBridgeSelect       => bridge.sel,
            oBridgeSelectAny    => bridge.selAny,
            oBridgeValid        => bridge.valid,
            iBaseSetWrite       => baseset.write,
            iBaseSetRead        => baseset.read,
            iBaseSetByteenable  => baseset.byteenable,
            iBaseSetAddress     => baseset.address,
            iBaseSetData        => baseset.writedata,
            oBaseSetData        => basesetReaddata,
            oBaseSetAck         => basesetAck
        );

    --! This process initializes the translation values in the bridge.
    --! After completion the process activates the bridge stim and hangs.
    theBaseSetStim : process(rst, clk)
        variable vAddr      : natural := 0;
        variable vTmpAddr   : std_logic_vector(baseset.address'range);
    begin
        if rst = cActivated then
            baseset         <= cBaseSetInit;
            startBridgeStim <= cInactivated;
            vAddr           := 0;
            vTmpAddr        := (others => cInactivated);
        elsif rising_edge(clk) then
            startBridgeStim <= cInactivated;
            --write translation addresses to baseset
            -- if done start bridge stime and hang
            if vAddr < cAddressSpaceCount then
                baseset.write       <= cActivated;
                baseset.byteenable  <= (others => cActivated);
                baseset.address     <= vTmpAddr;
                baseset.writedata   <= std_logic_vector(
                    resize(unsigned(cTransAddressArray(vAddr)), cBaseSetDataWidth)
                );
                if basesetAck = cActivated then
                    vAddr := vAddr + 1;
                    vTmpAddr := std_logic_vector(to_unsigned(vAddr, vTmpAddr'length));
                end if;
            else
                baseset         <= cBaseSetInit;
                startBridgeStim <= cActivated;
            end if;
        end if;
    end process;

    --! The bridge stimulation is handled by a bus master model, that simply
    --! performs reads. The read data is the translated address.
    theBridgeStim : entity work.busMaster
        generic map (
            gAddrWidth      => cBridgeInAddrWidth,
            gDataWidth      => cBridgeOutAddrWidth,
            gStimuliFile    => "../tb/tbDynamicBridgeBhv-stim.txt"
        )
        port map (
            iRst        => rst,
            iClk        => clk,
            iEnable     => startBridgeStim,
            iAck        => bridge.valid,
            iReaddata   => bridge.outAddr, --TODO: mask with bridge.sel
            oWrite      => open, --unused
            oRead       => bridge.req,
            oSelect     => open, --unused
            oAddress    => bridge.inAddr,
            oByteenable => open, --unused
            oWritedata  => open, --unused
            oError      => error,
            oDone       => done
        );

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
