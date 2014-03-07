-------------------------------------------------------------------------------
--! @file tbAxiHostInterface-bhv-ea.vhd
--! @brief Test bench for host iterface IP with AXI wrapper
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
--
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

entity tbAxiHostInterface is
    generic (
        --! PCP Simulation file
        gPcpStim        : string := "text.txt";
        --! Host simulation file
        gHostStim       : string := "text.txt";
        --! Host Model 0-Parallel 1-AXI
        gHostIfModel    : natural:= 0
    );
end entity tbAxiHostInterface;

architecture bhv of  tbAxiHostInterface is
    --! Address width for AXI bus
    constant C_AXI_ADDR_WIDTH   : integer := 32;
    --! Data width for AXI bus
    constant C_AXI_DATA_WIDTH   : integer := 32;

    --! Host Interface Lower Base Address for PCP
    constant C_BASEADDR         : std_logic_vector(C_AXI_ADDR_WIDTH-1 downto 0) := x"7C000000";
    --! Host Interface Higher Base Address for PCP
    constant C_HIGHADDR         : std_logic_vector(C_AXI_ADDR_WIDTH-1 downto 0) := x"7C0FFFFF";
    --! Host Interface Lower Base Address for Host
    constant C_HOST_BASEADDR    : std_logic_vector(C_AXI_ADDR_WIDTH-1 downto 0) := x"8C000000";
    --! Host Interface Higher Base Address for Host
    constant C_HOST_HIGHADDR    : std_logic_vector(C_AXI_ADDR_WIDTH-1 downto 0) := x"8C0FFFFF";
    --! Extenal Memory model lower Base Address
    constant C_MEM_BASEADDR     : std_logic_vector(C_AXI_ADDR_WIDTH-1 downto 0) := x"30000000";
    --! Extenal Memory model Higher Base Address
    constant C_MEM_HIGHADDR     : std_logic_vector(C_AXI_ADDR_WIDTH-1 downto 0) := x"FFFFFFFF";

    --! global clock
    signal clk      : std_logic;
    --! global reset
    signal rst      : std_logic;
    --! global active low reset
    signal nRst     : std_logic;
    --! global done
    signal done     : std_logic;
    --! global error
    signal error    : std_logic;

    --! AXI lite interface signals
    type tAxiLite is record
        --Write Address
        AWVALID : std_logic;
        AWREADY : std_logic;
        AWADDR  : std_logic_vector(C_AXI_ADDR_WIDTH-1 downto 0);
        AWPROT  : std_logic_vector(2 downto 0);
        --Write Data
        WVALID  : std_logic;
        WREADY  : std_logic;
        WDATA   : std_logic_vector(C_AXI_DATA_WIDTH-1 downto 0);
        WSTRB   : std_logic_vector(3 downto 0);
        --Write Response
        BVALID  : std_logic;
        BREADY  : std_logic;
        BRESP   : std_logic_vector(1 downto 0);
        --Read Address
        ARVALID : std_logic;
        ARREADY : std_logic;
        ARADDR  : std_logic_vector(C_AXI_ADDR_WIDTH-1 downto 0);
        ARPROT  : std_logic_vector(2 downto 0);
        --Read Data
        RVALID  : std_logic;
        RREADY  : std_logic;
        RDATA   : std_logic_vector(C_AXI_DATA_WIDTH-1 downto 0);
        RRESP   : std_logic_vector(1 downto 0);
    end record;

    --! instance for PCP Interface
    signal inst_pcpAxiLite      : tAxiLite;
    --! instance for Host Interface
    signal inst_hostAxiLite     : tAxiLite;
    --! instance for Memory Bridge Interface
    signal inst_masterAxiLite   : tAxiLite;

    --!Bus Master interface signals
    type tBusMaster is record
        AvalonRead      : std_logic;
        AvalonWrite     : std_logic;
        AvalonAddr      : std_logic_vector(31 downto 0);
        AvalonBE        : std_logic_vector(3 downto 0);
        AvalonWaitReq   : std_logic;
        AvalonReadValid : std_logic;
        AvalonReadData  : std_logic_vector(31 downto 0);
        AvalonWriteData : std_logic_vector(31 downto 0);
        BusMasterEnable : std_logic;
        BusMasterAck    : std_logic;
        BusMasterSelect : std_logic;
        BusMasterError  : std_logic;
        BusMasterDone   : std_logic;
        BusMasterReset  : std_logic;
    end record;

    --! instance for PCP Busmaster
    signal inst_pcpBusMaster    : tBusMaster;
    --! instance for Host Busmaster
    signal inst_hostBusMaster   : tBusMaster;
    --! instance for memory Bridge Busmaster
    signal inst_memoryBusMaster : tBusMaster;

    --! Memory Ack for completion
    signal memroyAck            : std_logic;
    --! Interrupt receiver
    signal inr_irqSync_irq      : std_logic;
    --! Interrupt sender
    signal ins_irqOut_irq       : std_logic;
    --! External Sync Source
    signal coe_ExtSync_exsync   : std_logic;
     --! Node Id set to MN
    signal coe_NodeId_nodeid    : std_logic_vector(7 downto 0) := x"F0";
    --! POWERLINK Error LED
    signal coe_PlkLed_lederr    : std_logic;
    --! POWERLINK Status LED
    signal coe_PlkLed_ledst     : std_logic;

    --Parallel Interface Signals
    --! Data width
    constant cParallelDataWidth             : natural := 16;
    --! Chip select
    signal coe_parHost_chipselect           : std_logic;
    --! Read Enable
    signal coe_parHost_read                 : std_logic;
    --! Write Enable
    signal coe_parHost_write                : std_logic;
    --! Address latch enable
    signal coe_parHost_addressLatchEnable   : std_logic;
    --! Read/Write Ack signal
    signal coe_parHost_acknowledge          : std_logic;
    --! Byte enable
    signal coe_parHost_byteenable           : std_logic_vector(cParallelDataWidth/8-1 downto 0);
    --! Address
    signal coe_parHost_address              : std_logic_vector(15 downto 0);
    --! Data Input
    signal coe_parHost_data_I               : std_logic_vector(cParallelDataWidth-1 downto 0);
    --! Data Output
    signal coe_parHost_data_O               : std_logic_vector(cParallelDataWidth-1 downto 0);
    --! Data Trigger
    signal coe_parHost_data_T               : std_logic;
    --! Address/Data Input
    signal coe_parHost_addressData_I        : std_logic_vector(cParallelDataWidth-1 downto 0);
    --! Address/Data Output
    signal coe_parHost_addressData_O        : std_logic_vector(cParallelDataWidth-1 downto 0);
    --! Address/Data Trigger
    signal coe_parHost_addressData_T        : std_logic;

    -- Test case
    --! Time Period for clock
    constant cPeriode           : time    := 10 ns;
    --! PCP simulation file
    constant cStimuliFile       : string  := gPcpStim;
    --! Host simulation file
    constant cHostStimuliFile   : string  := gHostStim;
    --! Host Interface Type Selection
    constant cHostIfType        : integer := gHostIfModel;
    --! External memory size
    constant cRamSize           : natural := 640 * 1024; --[byte]
    --! External Memory address width
    constant cRamAddrWidth      : natural := LogDualis(cRamSize);

    --! Version Major
    constant cVersionMajor      : integer := 255;
    --! Version Minor
    constant cVersionMinor      : integer := 255;
    --! Revision
    constant cVersionRevision   : integer := 255;
    --! Version Count
    constant cVersionCount      : integer := 0;
    --! Base Addreess for Dyanamic Bufefer 0
    constant cBaseDynBuf0       : integer := 16#00800#;
    --! Base Addreess for  Dyanamic Bufefer 1
    constant cBaseDynBuf1       : integer := 16#01000#;
    --! Base Addreess for Error Counter
    constant cBaseErrCntr       : integer := 16#01800#;
    --! Base Addreess for Tx NMT queue
    constant cBaseTxNmtQ        : integer := 16#02800#;
    --! Base Addreess for Tx Generic queue
    constant cBaseTxGenQ        : integer := 16#03800#;
    --! Base Addreess for Tx Sync Queue
    constant cBaseTxSynQ        : integer := 16#04800#;
    --! Base Addreess for Tx Virtual Ethernet Queus
    constant cBaseTxVetQ        : integer := 16#05800#;
    --! Base Addreess for Rx Virtual Ethernet Queus
    constant cBaseRxVetQ        : integer := 16#06800#;
    --! Base Addreess for Kernal to user Queue
    constant cBaseK2UQ          : integer := 16#07000#;
    --! Base Addreess for User to Kernal Queus
    constant cBaseU2KQ          : integer := 16#09000#;
    --! Base Addreess for PDO
    constant cBasePdo           : integer := 16#0B000#;
    --! Base Addreess for Reverved Area
    constant cBaseRes           : integer := 16#0E000#;
    --! Bridge Address
    signal BridgeAddress : std_logic_vector(31 downto 0);
begin
    assert (error /= cActivated)
        report "The bus master reports an error!"
    severity failure;

    -- AXI Host Interface Top Entity
    --! DUT - Top level of Host Interface IP core
    DUT: entity work.axi_hostinterface
        generic map (
            C_BASEADDR              => C_BASEADDR,
            C_HIGHADDR              => C_HIGHADDR,
            C_S_AXI_ADDR_WIDTH      => C_AXI_ADDR_WIDTH,
            C_S_AXI_DATA_WIDTH      => C_AXI_DATA_WIDTH,
            C_HOST_BASEADDR         => C_HOST_BASEADDR,
            C_HOST_HIGHADDR         => C_HOST_HIGHADDR,
            C_S_HOST_AXI_DATA_WIDTH => C_AXI_ADDR_WIDTH,
            C_S_HOST_AXI_ADDR_WIDTH => C_AXI_DATA_WIDTH,
            C_M_AXI_ADDR_WIDTH      => C_AXI_ADDR_WIDTH,
            C_M_AXI_DATA_WIDTH      => C_AXI_DATA_WIDTH,
            gVersionMajor           => cVersionMajor,
            gVersionMinor           => cVersionMinor,
            gVersionRevision        => cVersionRevision,
            gVersionCount           => cVersionCount,
            gBaseDynBuf0            => cBaseDynBuf0,
            gBaseDynBuf1            => cBaseDynBuf1,
            gBaseErrCntr            => cBaseErrCntr,
            gBaseTxNmtQ             => cBaseTxNmtQ,
            gBaseTxGenQ             => cBaseTxGenQ,
            gBaseTxSynQ             => cBaseTxSynQ,
            gBaseTxVetQ             => cBaseTxVetQ,
            gBaseRxVetQ             => cBaseRxVetQ,
            gBaseK2UQ               => cBaseK2UQ,
            gBaseU2KQ               => cBaseU2KQ,
            gBasePdo                => cBasePdo,
            gBaseRes                => cBaseRes,
            gHostIfType             => cHostIfType,
            gParallelDataWidth      => cParallelDataWidth,
            gParallelMultiplex      => cFalse
        )
        port map(
            S_AXI_PCP_ACLK              =>  clk,
            S_AXI_PCP_ARESETN           =>  nRst,
            -- Slave Interface Write Address Ports
            S_AXI_PCP_AWADDR            => inst_pcpAxiLite.AWADDR,
            S_AXI_PCP_AWVALID           => inst_pcpAxiLite.AWVALID,
            S_AXI_PCP_AWREADY           => inst_pcpAxiLite.AWREADY,
            -- Slave Interface Write Data Ports
            S_AXI_PCP_WDATA             => inst_pcpAxiLite.WDATA,
            S_AXI_PCP_WSTRB             => inst_pcpAxiLite.WSTRB,
            S_AXI_PCP_WVALID            => inst_pcpAxiLite.WVALID,
            S_AXI_PCP_WREADY            => inst_pcpAxiLite.WREADY,
            -- Slave Interface Write Response Ports
            S_AXI_PCP_BRESP             => inst_pcpAxiLite.BRESP,
            S_AXI_PCP_BVALID            => inst_pcpAxiLite.BVALID,
            S_AXI_PCP_BREADY            => inst_pcpAxiLite.BREADY,
            -- Slave Interface Read Address Ports
            S_AXI_PCP_ARADDR            => inst_pcpAxiLite.ARADDR,
            S_AXI_PCP_ARVALID           => inst_pcpAxiLite.ARVALID,
            S_AXI_PCP_ARREADY           => inst_pcpAxiLite.ARREADY,
            -- Slave Interface Read Data Ports
            S_AXI_PCP_RDATA             => inst_pcpAxiLite.RDATA,
            S_AXI_PCP_RRESP             => inst_pcpAxiLite.RRESP,
            S_AXI_PCP_RVALID            => inst_pcpAxiLite.RVALID,
            S_AXI_PCP_RREADY            => inst_pcpAxiLite.RREADY,
            --! Host Processor System Signals
            S_AXI_HOST_ACLK             => clk,
            S_AXI_HOST_ARESETN          => nRst,
            -- Slave Interface Write Address Ports
            S_AXI_HOST_AWADDR           => inst_hostAxiLite.AWADDR,
            S_AXI_HOST_AWVALID          => inst_hostAxiLite.AWVALID,
            S_AXI_HOST_AWREADY          => inst_hostAxiLite.AWREADY,
            -- Slave Interface Write Data Ports
            S_AXI_HOST_WDATA            => inst_hostAxiLite.WDATA,
            S_AXI_HOST_WSTRB            => inst_hostAxiLite.WSTRB,
            S_AXI_HOST_WVALID           => inst_hostAxiLite.WVALID,
            S_AXI_HOST_WREADY           => inst_hostAxiLite.WREADY,
            -- Slave Interface Write Response Ports
            S_AXI_HOST_BRESP            => inst_hostAxiLite.BRESP,
            S_AXI_HOST_BVALID           => inst_hostAxiLite.BVALID,
            S_AXI_HOST_BREADY           => inst_hostAxiLite.BREADY,
            -- Slave Interface Read Address Ports
            S_AXI_HOST_ARADDR           => inst_hostAxiLite.ARADDR,
            S_AXI_HOST_ARVALID          => inst_hostAxiLite.ARVALID,
            S_AXI_HOST_ARREADY          => inst_hostAxiLite.ARREADY,
            -- Slave Interface Read Data Ports
            S_AXI_HOST_RDATA            => inst_hostAxiLite.RDATA,
            S_AXI_HOST_RRESP            => inst_hostAxiLite.RRESP,
            S_AXI_HOST_RVALID           => inst_hostAxiLite.RVALID,
            S_AXI_HOST_RREADY           => inst_hostAxiLite.RREADY,
            -- Magic Bridge System Signals
            M_AXI_ACLK                  => clk,
            M_AXI_ARESETN               => nRst,
            -- Master Interface Write Address
            M_AXI_AWADDR                => inst_masterAxiLite.AWADDR,
            M_AXI_AWPROT                => inst_masterAxiLite.AWPROT,
            M_AXI_AWVALID               => inst_masterAxiLite.AWVALID,
            M_AXI_AWREADY               => inst_masterAxiLite.AWREADY,
            -- Master Interface Write Data
            M_AXI_WDATA                 => inst_masterAxiLite.WDATA,
            M_AXI_WSTRB                 => inst_masterAxiLite.WSTRB,
            M_AXI_WVALID                => inst_masterAxiLite.WVALID,
            M_AXI_WREADY                => inst_masterAxiLite.WREADY,
            -- Master Interface Write Response
            M_AXI_BRESP                 => inst_masterAxiLite.BRESP,
            M_AXI_BVALID                => inst_masterAxiLite.BVALID,
            M_AXI_BREADY                => inst_masterAxiLite.BREADY,
            -- Master Interface Read Address
            M_AXI_ARADDR                => inst_masterAxiLite.ARADDR,
            M_AXI_ARPROT                => inst_masterAxiLite.ARPROT,
            M_AXI_ARVALID               => inst_masterAxiLite.ARVALID,
            M_AXI_ARREADY               => inst_masterAxiLite.ARREADY,
            -- Master Interface Read Data
            M_AXI_RDATA                  => inst_masterAxiLite.RDATA,
            M_AXI_RRESP                  => inst_masterAxiLite.RRESP,
            M_AXI_RVALID                 => inst_masterAxiLite.RVALID,
            M_AXI_RREADY                 => inst_masterAxiLite.RREADY,
            irqSync_irq                  => inr_irqSync_irq,
            irqOut_irq                   => ins_irqOut_irq,
            iExtSync_exsync              => coe_ExtSync_exsync,
            iNodeId_nodeid               => coe_NodeId_nodeid,
            oPlkLed_lederr               => coe_PlkLed_lederr,
            oPlkLed_ledst                => coe_PlkLed_ledst,
            -- Parallel Host Interface
            iParHost_chipselect          => coe_parHost_chipselect,
            iParHost_read                => coe_parHost_read,
            iParHost_write               => coe_parHost_write,
            iParHost_addressLatchEnable  => coe_parHost_addressLatchEnable,
            oParHost_acknowledge         => coe_parHost_acknowledge,
            iParHost_byteenable          => coe_parHost_byteenable,
            iParHost_address             => coe_parHost_address,
            iParHost_data_io             => coe_parHost_data_I,
            oParHost_data_io             => coe_parHost_data_O,
            oParHost_data_io_tri         => coe_parHost_data_T,
            iParHost_addressData_io      => coe_parHost_addressData_I,
            oParHost_addressData_io      => coe_parHost_addressData_O,
            oParHost_addressData_tri     => coe_parHost_addressData_T
        );

    --! AXI Powerlink Communicatio Processor Model
    PCP_MODEL: entity work.axiLiteMasterWrapper
    generic map (
            gAddrWidth          => C_AXI_ADDR_WIDTH,
            gDataWidth          => C_AXI_DATA_WIDTH
        )
    port map (
            -- System Signals
            iAclk               => clk,
            inAReset            => nRst,
            -- Master Interface Write Address
            oAwaddr             => inst_pcpAxiLite.AWADDR,
            oAwprot             => inst_pcpAxiLite.AWPROT,
            oAwvalid            => inst_pcpAxiLite.AWVALID,
            iAwready            => inst_pcpAxiLite.AWREADY,
            -- Master Interface Write Data
            oWdata              => inst_pcpAxiLite.WDATA,
            oWstrb              => inst_pcpAxiLite.WSTRB,
            oWvalid             => inst_pcpAxiLite.WVALID,
            iWready             => inst_pcpAxiLite.WREADY,
            -- Master Interface Write Response
            iBresp              => inst_pcpAxiLite.BRESP,
            iBvalid             => inst_pcpAxiLite.BVALID,
            oBready             => inst_pcpAxiLite.BREADY,
            -- Master Interface Read Address
            oAraddr             => inst_pcpAxiLite.ARADDR,
            oArprot             => inst_pcpAxiLite.ARPROT,
            oArvalid            => inst_pcpAxiLite.ARVALID,
            iArready            => inst_pcpAxiLite.ARREADY,
            -- Master Interface Read Data
            iRdata              => inst_pcpAxiLite.RDATA,
            iRresp              => inst_pcpAxiLite.RRESP,
            iRvalid             => inst_pcpAxiLite.RVALID,
            oRready             => inst_pcpAxiLite.RREADY,
            -- Avalon Interface Signals
            iAvalonRead         => inst_pcpBusMaster.AvalonRead,
            iAvalonWrite        => inst_pcpBusMaster.AvalonWrite,
            iAvalonAddr         => inst_pcpBusMaster.AvalonAddr,
            iAvalonBE           => inst_pcpBusMaster.AvalonBE,
            oAvalonWaitReq      => inst_pcpBusMaster.AvalonWaitReq,
            oAvalonReadValid    => inst_pcpBusMaster.AvalonReadValid,
            oAvalonReadData     => inst_pcpBusMaster.AvalonReadData,
            iAvalonWriteData    => inst_pcpBusMaster.AvalonWriteData
        );

    -- Avalon Master Write/Read operations
    --! BusMaster to read instruction and provide input to PCP model
    AVALON_BUS_MASTER_PCP:entity libutil.busMaster
        generic map (
            gAddrWidth          => C_AXI_ADDR_WIDTH,
            gDataWidth          => C_AXI_DATA_WIDTH,
            gStimuliFile        => cStimuliFile
         )
        port map (
            iRst                => inst_pcpBusMaster.BusMasterReset,
            iClk                => clk,
            iEnable             => inst_pcpBusMaster.BusMasterEnable,
            iAck                => inst_pcpBusMaster.BusMasterAck,
            iReaddata           => inst_pcpBusMaster.AvalonReadData,
            oWrite              => inst_pcpBusMaster.AvalonWrite,
            oRead               => inst_pcpBusMaster.AvalonRead,
            oSelect             => inst_pcpBusMaster.BusMasterSelect,
            oAddress            => inst_pcpBusMaster.AvalonAddr,
            oByteenable         => inst_pcpBusMaster.AvalonBE,
            oWritedata          => inst_pcpBusMaster.AvalonWriteData,
            oError              => inst_pcpBusMaster.BusMasterError,
            oDone               => inst_pcpBusMaster.BusMasterDone
        );

    ---------------------------------------------------------------------------
    -- Bridge Is connecting to Memory through AXI slave
    -- DUT_BRIDGE -> AXI_SLAVE->Avalon Memory model
    ---------------------------------------------------------------------------
    --! Bridge to memory interface
    MEMORY_IF_MODEL: entity work.axiLiteSlaveWrapper
        generic map (
                gBaseAddr          => C_MEM_BASEADDR,
                gHighAddr          => C_MEM_HIGHADDR,
                gAddrWidth         => C_AXI_ADDR_WIDTH,
                gDataWidth         => C_AXI_DATA_WIDTH
        )
        port map (
                -- System Signals
                iAclk              => clk,
                inAReset           => nRst,
                -- Slave Interface Write Address Ports
                iAwaddr            => BridgeAddress,
                iAwprot            => inst_masterAxiLite.AWPROT,
                iAwvalid           => inst_masterAxiLite.AWVALID,
                oAwready           => inst_masterAxiLite.AWREADY,
                -- Slave Interface Write Data Ports
                iWdata             => inst_masterAxiLite.WDATA,
                iWstrb             => inst_masterAxiLite.WSTRB,
                iWvalid            => inst_masterAxiLite.WVALID,
                oWready            => inst_masterAxiLite.WREADY,
                -- Slave Interface Write Response Ports
                oBresp             => inst_masterAxiLite.BRESP,
                oBvalid            => inst_masterAxiLite.BVALID,
                iBready            => inst_masterAxiLite.BREADY,
                -- Slave Interface Read Address Ports
                iAraddr            => inst_masterAxiLite.ARADDR,
                iArprot            => inst_masterAxiLite.ARPROT,
                iArvalid           => inst_masterAxiLite.ARVALID,
                oArready           => inst_masterAxiLite.ARREADY,
                -- Slave Interface Read Data Ports
                oRdata             => inst_masterAxiLite.RDATA,
                oRresp             => inst_masterAxiLite.RRESP,
                oRvalid            => inst_masterAxiLite.RVALID,
                iRready            => inst_masterAxiLite.RREADY,
                --Avalon Interface
                oAvsAddress        => inst_memoryBusMaster.AvalonAddr,
                oAvsByteenable     => inst_memoryBusMaster.AvalonBE,
                oAvsRead           => inst_memoryBusMaster.AvalonRead,
                oAvsWrite          => inst_memoryBusMaster.AvalonWrite,
                oAvsWritedata      => inst_memoryBusMaster.AvalonWriteData,
                iAvsReaddata       => inst_memoryBusMaster.AvalonReadData,
                iAvsWaitrequest    => inst_memoryBusMaster.AvalonWaitReq
        );

    BridgeAddress <= "00" & inst_masterAxiLite.AWADDR(29 downto 0);

    --! Memory Model
    theRam : entity libutil.spRam
        generic map (
            gDataWidth  => inst_memoryBusMaster.AvalonWriteData'length,
            gAddrWidth  => cRamAddrWidth-2
        )
        port map (
            iRst        => rst,
            iClk        => clk,
            iWrite      => inst_memoryBusMaster.AvalonWrite,
            iRead       => inst_memoryBusMaster.AvalonRead,
            iAddress    => inst_memoryBusMaster.AvalonAddr(cRamAddrWidth-1 downto 2),
            iByteenable => inst_memoryBusMaster.AvalonBE,
            iWritedata  => inst_memoryBusMaster.AvalonWriteData,
            oReaddata   => inst_memoryBusMaster.AvalonReadData,
            oAck        => memroyAck
        );

    inst_memoryBusMaster.AvalonWaitReq <= not memroyAck;

    ---------------------------------------------------------------------------
    -- Host AXI Interface IP master
    ---------------------------------------------------------------------------
    genHostAXIMaster : if cHostIfType = cFalse generate
    begin
        --! Host Processor Model
        HOST_MODEL: entity work.axiLiteMasterWrapper
            generic map (
                gAddrWidth      => C_AXI_ADDR_WIDTH,
                gDataWidth      => C_AXI_DATA_WIDTH
            )
            port map (
                -- System Signals
                iAclk               => clk,
                inAReset            => nRst,
                -- Master Interface Write Address
                oAwaddr             => inst_hostAxiLite.AWADDR,
                oAwprot             => inst_hostAxiLite.AWPROT,
                oAwvalid            => inst_hostAxiLite.AWVALID,
                iAwready            => inst_hostAxiLite.AWREADY,
                -- Master Interface Write Data
                oWdata              => inst_hostAxiLite.WDATA,
                oWstrb              => inst_hostAxiLite.WSTRB,
                oWvalid             => inst_hostAxiLite.WVALID,
                iWready             => inst_hostAxiLite.WREADY,
                -- Master Interface Write Response
                iBresp              => inst_hostAxiLite.BRESP,
                iBvalid             => inst_hostAxiLite.BVALID,
                oBready             => inst_hostAxiLite.BREADY,
                -- Master Interface Read Address
                oAraddr             => inst_hostAxiLite.ARADDR,
                oArprot             => inst_hostAxiLite.ARPROT,
                oArvalid            => inst_hostAxiLite.ARVALID,
                iArready            => inst_hostAxiLite.ARREADY,
                -- Master Interface Read Data
                iRdata              => inst_hostAxiLite.RDATA,
                iRresp              => inst_hostAxiLite.RRESP,
                iRvalid             => inst_hostAxiLite.RVALID,
                oRready             => inst_hostAxiLite.RREADY,
                -- Avalon Interface Signals
                iAvalonRead         => inst_hostBusMaster.AvalonRead,
                iAvalonWrite        => inst_hostBusMaster.AvalonWrite,
                iAvalonAddr         => inst_hostBusMaster.AvalonAddr,
                iAvalonBE           => inst_hostBusMaster.AvalonBE,
                oAvalonWaitReq      => inst_hostBusMaster.AvalonWaitReq,
                oAvalonReadValid    => inst_hostBusMaster.AvalonReadValid,
                oAvalonReadData     => inst_hostBusMaster.AvalonReadData,
                iAvalonWriteData    => inst_hostBusMaster.AvalonWriteData
            );

    --! BusMaster to read instruction and provide input to Host model
        AVALON_BUS_MASTER_HOST : entity libutil.busMaster
            generic map (
                gAddrWidth          => C_AXI_ADDR_WIDTH,
                gDataWidth          => C_AXI_DATA_WIDTH,
                gStimuliFile        => cHostStimuliFile
             )
            port map (
                iRst                => rst,
                iClk                => clk,
                iEnable             => inst_hostBusMaster.BusMasterEnable,
                iAck                => inst_hostBusMaster.BusMasterAck,
                iReaddata           => inst_hostBusMaster.AvalonReadData,
                oWrite              => inst_hostBusMaster.AvalonWrite,
                oRead               => inst_hostBusMaster.AvalonRead,
                oSelect             => inst_hostBusMaster.BusMasterSelect,
                oAddress            => inst_hostBusMaster.AvalonAddr,
                oByteenable         => inst_hostBusMaster.AvalonBE,
                oWritedata          => inst_hostBusMaster.AvalonWriteData,
                oError              => inst_hostBusMaster.BusMasterError,
                oDone               => inst_hostBusMaster.BusMasterDone
            );

        inst_hostBusMaster.BusMasterReset  <= rst;
        inst_hostBusMaster.BusMasterEnable <= cActivated;
        inst_hostBusMaster.BusMasterAck    <= not inst_hostBusMaster.AvalonWaitReq;
    end generate;

    ---------------------------------------------------------------------------
    --  Parallel Interface for Host
    ---------------------------------------------------------------------------
    genParallelMaster : if cHostIfType = cTrue generate
        --TODO: Add Parallel Master to based Simulation
    end generate;

    ---------------------------------------------------------------------------
    --  General Settings
    ---------------------------------------------------------------------------
     -- Clock & Reset
     --! clock generation
     theClkGen : entity libutil.clkgen
        generic map (
            gPeriod => cPeriode
        )
        port map (
            iDone   => done,
            oClk    => clk
        );

    rst     <= cActivated, cInactivated after 300 ns;
    nRst    <= not rst;

    done <= inst_pcpBusMaster.BusMasterDone and inst_hostBusMaster.BusMasterDone;
    error <= inst_pcpBusMaster.BusMasterError or inst_hostBusMaster.BusMasterError;

    inst_pcpBusMaster.BusMasterReset  <= rst;
    inst_pcpBusMaster.BusMasterEnable <= cActivated;
    inst_pcpBusMaster.BusMasterAck    <= not inst_pcpBusMaster.AvalonWaitReq;
end bhv;
