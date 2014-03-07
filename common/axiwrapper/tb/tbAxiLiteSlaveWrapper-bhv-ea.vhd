-------------------------------------------------------------------------------
--! @file tbAxiLiteSlaveWrapper-bhv-ea.vhd
--! @brief Test bench for AXI lite Master & AXI lite Slave
-------------------------------------------------------------------------------
--
--    (c) B&R, 2012
--    (c) Kalycito Infotech Pvt Ltd
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
--! Use numeric std
use ieee.numeric_std.all;

--! Use libcommon library
library libcommon;
--! Use global package
use libcommon.global.all;

--! Utility library
library libutil;

entity tbAxiLiteSlaveWrapper is
    generic (
        --! Master Simulation file
        gMasterStim        : string := "text.txt"
    );
end entity tbAxiLiteSlaveWrapper;

architecture bhv of tbAxiLiteSlaveWrapper is
    --! Address Width
    constant cAddrWidth : integer   := 32;
    --! Data Width
    constant cDataWidth : integer   := 32;
    --! Memory Base Address
    constant cBaseAddr  : std_logic_vector(31 downto 0) := x"8C000000";
    --! Memory High Address
    constant cHighAddr  : std_logic_vector(31 downto 0) := x"8C00ffff";

    --! Global Clock
    signal clock    : std_logic := '1';
    --! Global Active low Reset
    signal nReset   : std_logic;

    --! Axi lite singals
    type tAxiLite is record
                --Write Address
        AWVALID : std_logic;
        AWREADY : std_logic;
        AWADDR  : std_logic_vector (cAddrWidth-1 downto 0);
        AWPROT  : std_logic_vector (2 downto 0);
                --Write Data
        WVALID  : std_logic;
        WREADY  : std_logic;
        WDATA   : std_logic_vector (cDataWidth-1 downto 0);
        WSTRB   : std_logic_vector (cDataWidth/8-1 downto 0);
                --Write Response
        BVALID  : std_logic;
        BREADY  : std_logic;
        BRESP   : std_logic_vector (1 downto 0);
                --Read Address
        ARVALID : std_logic;
        ARREADY : std_logic;
        ARADDR  : std_logic_vector (cAddrWidth-1 downto 0);
        ARPROT  : std_logic_vector (2 downto 0);
        RVALID  : std_logic;
        RREADY  : std_logic;
        RDATA   : std_logic_vector (cDataWidth-1 downto 0);
        RRESP   : std_logic_vector (1 downto 0);
    end record;

    --! axiLite master signals
    signal inst_axiliteMaster : tAxiLite;

    --! Avalon Interface signals
    type tAvalonInterface is record
        AvalonRead          : std_logic;
        AvalonWrite         : std_logic;
        AvalonAddr          : std_logic_vector   (cAddrWidth-1 downto 0);
        AvalonBE            : std_logic_vector   (cDataWidth/8-1 downto 0);
        AvalonWaitReq       : std_logic;
        AvalonReadValid     : std_logic;
        AvalonReadData      : std_logic_vector   (cDataWidth-1 downto 0);
        AvalonWriteData     : std_logic_vector   (cDataWidth-1 downto 0);
    end record;

    --! Bus Master Avalon signals
    signal inst_avalonMaster :  tAvalonInterface;
    --! Memory Avalon signals
    signal inst_avalonRam    :  tAvalonInterface;
    --! baus Master Enable
    signal  BusMasterEnable       : std_logic;
    --! Bus Master Ack
    signal  BusMasterAck          : std_logic;
    --! Bus Master select
    signal  BusMasterSelect       : std_logic;
    --! Bus Master Error
    signal  BusMasterError        : std_logic;
    --! Bus master Done
    signal  BusMasterDone         : std_logic;
    --! Bus master Reset
    signal  BusMasterReset        : std_logic;
    --! Memory Ack for completion
    signal memroyAck            : std_logic;
    --! Constact for Time Period
    constant cPeriode           : time := 10 ns;
    --! Stimuli file
    constant cStimuliFile       : string  := gMasterStim;
    --! External memory size
    constant cRamSize           : natural := 640 * 1024; --[byte]
    --! External Memory address width
    constant cRamAddrWidth      : natural := LogDualis(cRamSize);
begin
    assert (BusMasterError /= cActivated)
        report "The bus master reports an error!"
    severity failure;

    -- Clock and Reset generation
    --! clock generation
     theClkGen : entity libutil.clkgen
        generic map (
            gPeriod => cPeriode
        )
        port map (
            iDone   => BusMasterDone,
            oClk    => clock
        );


    nReset <=   cnActivated after 0 ns,
                cnInactivated after 100 ns;

    ---------------------------------------------------------------------------
    --  DUT:  AXI lite master
    ---------------------------------------------------------------------------
    -- TODO: Repalce axiLiteMasterWrapper with known AXI Master BFM
    --! Master Lite Wrapper for provide input for Slave
    AXI_MASTER_LITE: entity work.axiLiteMasterWrapper
    generic map (
        gAddrWidth          => cAddrWidth,
        gDataWidth          => cDataWidth
    )
    port map (
        -- System Signals
        iAclk               => clock,
        inAReset            => nReset,
        -- Master Interface Write Address
        oAwaddr             => inst_axiliteMaster.AWADDR,
        oAwprot             => inst_axiliteMaster.AWPROT,
        oAwvalid            => inst_axiliteMaster.AWVALID,
        iAwready            => inst_axiliteMaster.AWREADY,
        -- Master Interface Write Data
        oWdata              => inst_axiliteMaster.WDATA,
        oWstrb              => inst_axiliteMaster.WSTRB,
        oWvalid             => inst_axiliteMaster.WVALID,
        iWready             => inst_axiliteMaster.WREADY,
        -- Master Interface Write Response
        iBresp              => inst_axiliteMaster.BRESP,
        iBvalid             => inst_axiliteMaster.BVALID,
        oBready             => inst_axiliteMaster.BREADY,
        -- Master Interface Read Address
        oAraddr             => inst_axiliteMaster.ARADDR,
        oArprot             => inst_axiliteMaster.ARPROT,
        oArvalid            => inst_axiliteMaster.ARVALID,
        iArready            => inst_axiliteMaster.ARREADY,
        -- Master Interface Read Data
        iRdata              => inst_axiliteMaster.RDATA,
        iRresp              => inst_axiliteMaster.RRESP,
        iRvalid             => inst_axiliteMaster.RVALID,
        oRready             => inst_axiliteMaster.RREADY,
        -- Avalon Interface Signals
        iAvalonRead         => inst_avalonMaster.AvalonRead,
        iAvalonWrite        => inst_avalonMaster.AvalonWrite,
        iAvalonAddr         => inst_avalonMaster.AvalonAddr,
        iAvalonBE           => inst_avalonMaster.AvalonBE,
        oAvalonWaitReq      => inst_avalonMaster.AvalonWaitReq,
        oAvalonReadValid    => inst_avalonMaster.AvalonReadValid,
        oAvalonReadData     => inst_avalonMaster.AvalonReadData,
        iAvalonWriteData    => inst_avalonMaster.AvalonWriteData
    );

    ---------------------------------------------------------------------------
    --  Master Stimulus
    ---------------------------------------------------------------------------
    --!DUT: Avalon Master Write/Read operations as input for AXI master Wrapper
    AVALON_BUS_MASTER:entity libutil.busMaster
    generic map (
        gAddrWidth          => cAddrWidth,
        gDataWidth          => cDataWidth,
        gStimuliFile        => cStimuliFile
    )
    port map (
        iRst                =>  BusMasterReset,
        iClk                =>  clock,
        iEnable             =>  BusMasterEnable,
        iAck                =>  BusMasterAck,
        iReaddata           =>  inst_avalonMaster.AvalonReadData,
        oWrite              =>  inst_avalonMaster.AvalonWrite,
        oRead               =>  inst_avalonMaster.AvalonRead,
        oSelect             =>  BusMasterSelect,
        oAddress            =>  inst_avalonMaster.AvalonAddr,
        oByteenable         =>  inst_avalonMaster.AvalonBE,
        oWritedata          =>  inst_avalonMaster.AvalonWriteData,
        oError              =>  BusMasterError,
        oDone               =>  BusMasterDone
    );

    BusMasterReset  <= not nReset;
    BusMasterEnable <= cActivated;
    BusMasterAck    <= not inst_avalonMaster.AvalonWaitReq;

    ---------------------------------------------------------------------------
    --  DUT: AXI lite slave
    ---------------------------------------------------------------------------
    --! AXI lite slave for AXI master interface Memory read and write operations
    AXI_LITE_SLAVE:entity work.axiLiteSlaveWrapper
    generic map (
        gBaseAddr          => cBaseAddr,
        gHighAddr          => cHighAddr,
        gAddrWidth         => cAddrWidth,
        gDataWidth         => cDataWidth
    )
    port map (
        -- System Signals
        iAclk              => clock,
        inAReset           => nReset,
        -- Slave Interface Write Address Ports
        iAwaddr            => inst_axiliteMaster.AWADDR,
        iAwprot            => inst_axiliteMaster.AWPROT,
        iAwvalid           => inst_axiliteMaster.AWVALID,
        oAwready           => inst_axiliteMaster.AWREADY,
        -- Slave Interface Write Data Ports
        iWdata             => inst_axiliteMaster.WDATA,
        iWstrb             => inst_axiliteMaster.WSTRB,
        iWvalid            => inst_axiliteMaster.WVALID,
        oWready            => inst_axiliteMaster.WREADY,
        -- Slave Interface Write Response Ports
        oBresp             => inst_axiliteMaster.BRESP,
        oBvalid            => inst_axiliteMaster.BVALID,
        iBready            => inst_axiliteMaster.BREADY,
        -- Slave Interface Read Address Ports
        iAraddr            => inst_axiliteMaster.ARADDR,
        iArprot            => inst_axiliteMaster.ARPROT,
        iArvalid           => inst_axiliteMaster.ARVALID,
        oArready           => inst_axiliteMaster.ARREADY,
        -- Slave Interface Read Data Ports
        oRdata             => inst_axiliteMaster.RDATA,
        oRresp             => inst_axiliteMaster.RRESP,
        oRvalid            => inst_axiliteMaster.RVALID,
        iRready            => inst_axiliteMaster.RREADY,
        --Avalon Interface
        oAvsAddress        => inst_avalonRam.AvalonAddr,
        oAvsByteenable     => inst_avalonRam.AvalonBE,
        oAvsRead           => inst_avalonRam.AvalonRead,
        oAvsWrite          => inst_avalonRam.AvalonWrite,
        oAvsWritedata      => inst_avalonRam.AvalonWriteData,
        iAvsReaddata       => inst_avalonRam.AvalonReadData,
        iAvsWaitrequest    => inst_avalonRam.AvalonWaitReq
    );

    --! Memory model for write and write operations
    theRam : entity libutil.spRam
    generic map (
        gDataWidth  => cDataWidth,
        gAddrWidth  => cRamAddrWidth-2
    )
    port map (
        iRst        => BusMasterReset,
        iClk        => clock,
        iWrite      => inst_avalonRam.AvalonWrite,
        iRead       => inst_avalonRam.AvalonRead,
        iAddress    => inst_avalonRam.AvalonAddr(cRamAddrWidth-1 downto 2),
        iByteenable => inst_avalonRam.AvalonBE,
        iWritedata  => inst_avalonRam.AvalonWriteData,
        oReaddata   => inst_avalonRam.AvalonReadData,
        oAck        => memroyAck
    );

        inst_avalonRam.AvalonWaitReq <= not memroyAck;
end bhv;
