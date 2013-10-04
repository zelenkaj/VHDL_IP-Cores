-------------------------------------------------------------------------------
--! @file tbHostInterfaceBhv.vhd
--
--! @brief Testbench for Hostinterface ipcore
--
-------------------------------------------------------------------------------
--
--    (c) B&R, 2012
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

entity tbHostInterface is
    generic (
        --! Configure dynamic bridge using memory blocks (0 = false)
        gUseMemBlock : natural := 0;
        gPcpStim : string := "text.txt";
        gHostStim : string := "text.txt"
    );
end tbHostInterface;

architecture Bhv of tbHostInterface is
    -- configure bridge implementation
    constant cBridgeUseMemBlock : natural := gUseMemBlock;
    -- base addresses
    constant cVersionMajor      : natural := 16#01#;
    constant cVersionMinor      : natural := 16#02#;
    constant cVersionRevision   : natural := 16#03#;
    constant cVersionCount      : natural := 16#FF#;
    constant cBaseDynBuf0       : natural := 16#00800#;
    constant cBaseDynBuf1       : natural := 16#01000#;
    constant cBaseErrCntr       : natural := 16#01800#;
    constant cBaseTxNmtQ        : natural := 16#02800#;
    constant cBaseTxGenQ        : natural := 16#03800#;
    constant cBaseTxSynQ        : natural := 16#04800#;
    constant cBaseTxVetQ        : natural := 16#05800#;
    constant cBaseRxVetQ        : natural := 16#06800#;
    constant cBaseK2UQ          : natural := 16#07000#;
    constant cBaseU2KQ          : natural := 16#09000#;
    constant cBaseTpdo          : natural := 16#0B000#;
    constant cBaseRpdo          : natural := 16#0E000#;
    constant cBaseRes           : natural := 16#14000#;

    constant cRamSize       : natural := 640 * 1024; --[byte]
    constant cRamAddrWidth  : natural := LogDualis(cRamSize);

    signal clk  : std_logic;
    signal rst  : std_logic;
    signal done : std_logic;

    signal hostBridgeRead           : std_logic;
    signal hostBridgeWaitrequest    : std_logic;
    signal hostBridgeWrite          : std_logic;
    signal hostBridgeAddress        : std_logic_vector (29 downto 0);
    signal hostBridgeByteenable     : std_logic_vector (3 downto 0);
    signal hostBridgeReaddata       : std_logic_vector (31 downto 0);
    signal hostBridgeWritedata      : std_logic_vector (31 downto 0);
    signal hostBridge_ready         : std_logic;

    signal hostRead         : std_logic;
    signal hostWaitrequest  : std_logic;
    signal hostWrite        : std_logic;
    signal hostAddress      : std_logic_vector (16 downto 0);
    signal hostByteenable   : std_logic_vector (3 downto 0);
    signal hostReaddata     : std_logic_vector (31 downto 0);
    signal hostWritedata    : std_logic_vector (31 downto 0);
    signal hostAck          : std_logic;
    signal hostDone         : std_logic;

    signal pcpRead          : std_logic;
    signal pcpWaitrequest   : std_logic;
    signal pcpWrite         : std_logic;
    signal pcpAddress       : std_logic_vector (10 downto 0);
    signal pcpByteenable    : std_logic_vector (3 downto 0);
    signal pcpReaddata      : std_logic_vector (31 downto 0);
    signal pcpWritedata     : std_logic_vector (31 downto 0);
    signal pcpAck           : std_logic;
    signal pcpDone          : std_logic;

    signal plkLedError  : std_logic;
    signal plkLedStatus : std_logic;
    signal nodeId       : std_logic_vector (7 downto 0);

    signal irqExtSync   : std_logic;
    signal irqIntSync   : std_logic;
    signal irq          : std_logic;

    signal counter : std_logic_vector (7 downto 0);
begin
    cntIrqGen : process(clk)
    begin
        if rising_edge(clk) then
            if rst = cActivated then
                counter <= (others => cInactivated);
            else
                counter <= std_logic_vector(unsigned(counter) + 1);
            end if;
        end if;
    end process;

    irqIntSync  <= cActivated when unsigned(counter) = 10 else cInactivated;
    irqExtSync  <= cInactivated;
    nodeId      <= x"F0";

    DUT : entity work.hostInterface
        generic map (
            gBaseDynBuf0        => cBaseDynBuf0,
            gBaseDynBuf1        => cBaseDynBuf1,
            gBaseErrCntr        => cBaseErrCntr,
            gBaseK2UQ           => cBaseK2UQ,
            gBaseRes            => cBaseRes,
            gBaseRpdo           => cBaseRpdo,
            gBaseRxVetQ         => cBaseRxVetQ,
            gBaseTpdo           => cBaseTpdo,
            gBaseTxGenQ         => cBaseTxGenQ,
            gBaseTxNmtQ         => cBaseTxNmtQ,
            gBaseTxSynQ         => cBaseTxSynQ,
            gBaseTxVetQ         => cBaseTxVetQ,
            gBaseU2KQ           => cBaseU2KQ,
            gVersionCount       => cVersionCount,
            gVersionMajor       => cVersionMajor,
            gVersionMinor       => cVersionMinor,
            gVersionRevision    => cVersionRevision,
            gBridgeUseMemBlock  => cBridgeUseMemBlock
        )
        port map(
            iClk                    => clk,
            iRst                    => rst,
            iPcpAddress             => pcpAddress(10 downto 2),
            iPcpByteenable          => pcpByteenable,
            iPcpRead                => pcpRead,
            oPcpReaddata            => pcpReaddata,
            oPcpWaitrequest         => pcpWaitrequest,
            iPcpWrite               => pcpWrite,
            iPcpWritedata           => pcpWritedata,
            iHostAddress            => hostAddress(16 downto 2),
            iHostByteenable         => hostByteenable,
            iHostRead               => hostRead,
            oHostReaddata           => hostReaddata,
            oHostWaitrequest        => hostWaitrequest,
            iHostWrite              => hostWrite,
            iHostWritedata          => hostWritedata,
            oHostBridgeAddress      => hostBridgeAddress,
            oHostBridgeByteenable   => hostBridgeByteenable,
            oHostBridgeRead         => hostBridgeRead,
            iHostBridgeReaddata     => hostBridgeReaddata,
            iHostBridgeWaitrequest  => hostBridgeWaitrequest,
            oHostBridgeWrite        => hostBridgeWrite,
            oHostBridgeWritedata    => hostBridgeWritedata,
            iNodeId                 => nodeId,
            oPlkLedError            => plkLedError,
            oPlkLedStatus           => plkLedStatus,
            iIrqExtSync             => irqExtSync,
            iIrqIntSync             => irqIntSync,
            oIrq                    => irq
        );

    theRam : entity work.spRam
        port map (
            iClk        => clk,
            iWrite      => hostBridgeWrite,
            iRead       => hostBridgeRead,
            iAddress    => hostBridgeAddress(cRamAddrWidth-1 downto 2),
            iByteenable => hostBridgeByteenable,
            iWritedata  => hostBridgeWritedata,
            oReaddata   => hostBridgeReaddata,
            oAck        => hostBridge_ready
        );

    hostAck                 <= not hostWaitrequest;
    done                    <= hostDone and pcpDone;
    pcpAck                  <= not pcpWaitrequest;
    hostBridgeWaitrequest   <= not hostBridge_ready;

    host : entity work.busMaster
    generic map (
        gAddrWidth      => 17,
        gDataWidth      => 32,
        gStimuliFile    => gHostStim
    )
    port map(
        iAck        => hostAck,
        iClk        => clk,
        iEnable     => cActivated,
        iReaddata   => hostReaddata,
        iRst        => rst,
        oAddress    => hostAddress,
        oByteenable => hostByteenable,
        oDone       => hostDone,
        oRead       => hostRead,
        oWrite      => hostWrite,
        oWritedata  => hostWritedata
    );

    pcp : entity work.busMaster
        generic map (
            gAddrWidth      => 11,
            gDataWidth      => 32,
            gStimuliFile    => gPcpStim
        )
        port map(
            iAck        => pcpAck,
            iClk        => clk,
            iEnable     => cActivated,
            iReaddata   => pcpReaddata,
            iRst        => rst,
            oAddress    => pcpAddress,
            oByteenable => pcpByteenable,
            oDone       => pcpDone,
            oRead       => pcpRead,
            oWrite      => pcpWrite,
            oWritedata  => pcpWritedata
        );

    theClkGen : entity work.clkgen
        generic map (
            gPeriod => 10 ns
        )
        port map (
            iDone   => done,
            oClk    => clk
        );

    theRstGen : entity work.resetGen
        generic map (
            gResetTime => 100 ns
        )
        port map (
            oReset  => rst,
            onReset => open
        );
end Bhv;
