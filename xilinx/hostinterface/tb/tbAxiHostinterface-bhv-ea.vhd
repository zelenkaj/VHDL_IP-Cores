-------------------------------------------------------------------------------
--! @file tbAxiHostInterface-bhv-ea.vhd
--
--! @brief Test bench for host iterface IP with AXI wrapper
--
--! @details Test bench will provide necessary stimulies to host interface IP
--! to test the functionality of host interface through stimulues.Host interface
--! IP Master is connected to Memory for Data write/read through AXI slave.
--! Host Processor can be Parallel Interface or AXI mode Host
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
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
--! use global library
library work;
use work.global.all;
--! Bus master Pakg
use work.busMasterPkg.all;


entity tbAxiHostInterface is
    generic(
        --! PCP Simulation file
        gPcpStim        : string := "text.txt";
        --! Host simulation file
        gHostStim       : string := "text.txt";
        --! Host Model 0-Parallel 1-AXI
        gHostIfModel    : natural:= 0
    );
end entity tbAxiHostInterface;

architecture bhv of  tbAxiHostInterface is
    --FIXME: Add documentation
    constant C_AXI_ADDR_WIDTH   : integer := 32;
    --FIXME: Add documentation
    constant C_AXI_DATA_WIDTH   : integer := 32;

    --FIXME: Add documentation
    constant C_BASEADDR         : std_logic_vector(C_AXI_ADDR_WIDTH-1 downto 0) := x"7C000000";
    --FIXME: Add documentation
    constant C_HIGHADDR         : std_logic_vector(C_AXI_ADDR_WIDTH-1 downto 0) := x"7C0FFFFF";
    --FIXME: Add documentation
    constant C_HOST_BASEADDR    : std_logic_vector(C_AXI_ADDR_WIDTH-1 downto 0) := x"8C000000";
    --FIXME: Add documentation
    constant C_HOST_HIGHADDR    : std_logic_vector(C_AXI_ADDR_WIDTH-1 downto 0) := x"8C0FFFFF";
    --FIXME: Add documentation
    constant C_MEM_BASEADDR     : std_logic_vector(C_AXI_ADDR_WIDTH-1 downto 0) := x"30000000";
    --FIXME: Add documentation
    constant C_MEM_HIGHADDR     : std_logic_vector(C_AXI_ADDR_WIDTH-1 downto 0) := x"FFFFFFFF";

    signal clock    : std_logic;
    signal nAreset  : std_logic;

    --FIXME: Add documentation
    type tAxiLite is record
        --Write Address
        AWVALID : std_logic;
        AWREADY : std_logic;
        AWADDR  : std_logic_vector(C_AXI_ADDR_WIDTH-1 downto 0);
        --Write Data
        WVALID  : std_logic;
        WREADY  : std_logic;
        WDATA   : std_logic_vector(C_AXI_DATA_WIDTH-1 downto 0);
        --Write Response
        BVALID  : std_logic;
        BREADY  : std_logic;
        BRESP   : std_logic_vector(1 downto 0);
        --Read Address
        ARVALID : std_logic;
        ARREADY : std_logic;
        ARADDR  : std_logic_vector(C_AXI_ADDR_WIDTH-1 downto 0);
        --Read Data
        RVALID  : std_logic;
        RREADY  : std_logic;
        RDATA   : std_logic_vector(C_AXI_DATA_WIDTH-1 downto 0);
        ARPROT  : std_logic_vector(2 downto 0);
        AWPROT  : std_logic_vector(2 downto 0);
        WSTRB   : std_logic_vector(3 downto 0);
        RRESP   : std_logic_vector(1 downto 0);
    end record;

    signal inst_pcpAxiLite      : tAxiLite;
    signal inst_hostAxiLite     : tAxiLite;
    signal inst_masterAxiLite   : tAxiLite;

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

    signal inst_pcpBusMaster    : tBusMaster;
    signal inst_hostBusMaster   : tBusMaster;
    signal inst_memoryBusMaster : tBusMaster;

    signal memroyAck : std_logic;

    --Powerlink Interface Signals
    signal inr_irqSync_irq      : std_logic;
    signal ins_irqOut_irq       : std_logic;
    signal coe_ExtSync_exsync   : std_logic;
    signal coe_NodeId_nodeid    : std_logic_vector(7 downto 0) := x"F0";
    signal coe_PlkLed_lederr    : std_logic;
    signal coe_PlkLed_ledst     : std_logic;

    --Parallel Interface Signals
    constant cParallelDataWidth             : natural := 16;
    signal coe_parHost_chipselect           : std_logic;
    signal coe_parHost_read                 : std_logic;
    signal coe_parHost_write                : std_logic;
    signal coe_parHost_addressLatchEnable   : std_logic;
    signal coe_parHost_acknowledge          : std_logic;
    signal coe_parHost_byteenable           : std_logic_vector(cParallelDataWidth/8-1 downto 0);
    signal coe_parHost_address              : std_logic_vector(15 downto 0);
    signal coe_parHost_data_I               : std_logic_vector(cParallelDataWidth-1 downto 0);
    signal coe_parHost_data_O               : std_logic_vector(cParallelDataWidth-1 downto 0);
    signal coe_parHost_data_T               : std_logic ;
    signal coe_parHost_addressData_I        : std_logic_vector(cParallelDataWidth-1 downto 0);
    signal coe_parHost_addressData_O        : std_logic_vector(cParallelDataWidth-1 downto 0);
    signal coe_parHost_addressData_T        : std_logic ;

    -- Test case
    constant cPeriode           : time := 10 ns;
    constant cStimuliFile       : string  := gPcpStim ;
    constant cHostStimuliFile   : string  := gHostStim ;
    constant cHostIfType        : integer := gHostIfModel ;

    -- Isim Simulation
    -- TODO: remove absolute path or try generic
    --constant cStimuliFile     : string  := "E:/XilinxMN/HDL_GitRepo/VHDL_IP-Cores/xilinx/axiwrapper/tb/tbPCPMasterBhv_TB_stim.txt";
    --constant cHostStimuliFile : string  := "E:/XilinxMN/HDL_GitRepo/VHDL_IP-Cores/xilinx/axiwrapper/tb/tbHostMasterBhv_TB_stim.txt";
    --constant cHostIfType      : integer := 0 ; --1 Parallel 0- AXI



    constant cRamSize           : natural := 640 * 1024; --[byte]
    constant cRamAddrWidth      : natural := LogDualis(cRamSize);

    constant cVersionMajor      : integer := 255 ;
    constant cVersionMinor      : integer := 255 ;
    constant cVersionRevision   : integer := 255 ;
    constant cVersionCount      : integer := 0 ;
    constant cBaseDynBuf0       : integer := 16#00800#;
    constant cBaseDynBuf1       : integer := 16#01000#;
    constant cBaseErrCntr       : integer := 16#01800#;
    constant cBaseTxNmtQ        : integer := 16#02800#;
    constant cBaseTxGenQ        : integer := 16#03800#;
    constant cBaseTxSynQ        : integer := 16#04800#;
    constant cBaseTxVetQ        : integer := 16#05800#;
    constant cBaseRxVetQ        : integer := 16#06800#;
    constant cBaseK2UQ          : integer := 16#07000#;
    constant cBaseU2KQ          : integer := 16#09000#;
    constant cBaseTpdo          : integer := 16#0B000#;
    constant cBaseRpdo          : integer := 16#0E000#;
    constant cBaseRes           : integer := 16#14000#;

    signal BridgeAddrss : std_logic_vector(31 downto 0);
begin
--FIXME: Cleanup the rest below this line please!

-- AXI Host Interface Top Entity
--! DUT - Top level of Host Interface IP core
DUT: entity work.axi_hostinterface
    generic map (
        C_BASEADDR              =>  C_BASEADDR,
        C_HIGHADDR              =>  C_HIGHADDR,
        C_S_AXI_ADDR_WIDTH      =>  C_AXI_ADDR_WIDTH,
        C_S_AXI_DATA_WIDTH      =>  C_AXI_DATA_WIDTH ,
        C_HOST_BASEADDR         =>  C_HOST_BASEADDR,
        C_HOST_HIGHADDR         =>  C_HOST_HIGHADDR ,
        C_S_HOST_AXI_DATA_WIDTH =>  C_AXI_ADDR_WIDTH,
        C_S_HOST_AXI_ADDR_WIDTH =>  C_AXI_DATA_WIDTH,
        C_M_AXI_ADDR_WIDTH      =>  C_AXI_ADDR_WIDTH,
        C_M_AXI_DATA_WIDTH      =>  C_AXI_DATA_WIDTH,

        gVersionMajor       => cVersionMajor,
        gVersionMinor       => cVersionMinor ,
        gVersionRevision    => cVersionRevision ,
        gVersionCount       => cVersionCount,
        gBaseDynBuf0        => cBaseDynBuf0,
        gBaseDynBuf1        => cBaseDynBuf1 ,
        gBaseErrCntr        => cBaseErrCntr ,
        gBaseTxNmtQ         => cBaseTxNmtQ ,
        gBaseTxGenQ         => cBaseTxGenQ  ,
        gBaseTxSynQ         => cBaseTxSynQ  ,
        gBaseTxVetQ         => cBaseTxVetQ  ,
        gBaseRxVetQ         => cBaseRxVetQ  ,
        gBaseK2UQ           => cBaseK2UQ  ,
        gBaseU2KQ           => cBaseU2KQ  ,
        gBaseTpdo           => cBaseTpdo  ,
        gBaseRpdo           => cBaseRpdo  ,
        gBaseRes            => cBaseRes  ,
        gHostIfType         => cHostIfType  ,
        gParallelDataWidth  => cParallelDataWidth   ,
        gParallelMultiplex  => cFalse
    )
    port map(

        --! Powerlink Processer System Signals
        S_AXI_PCP_ACLK            =>  clock ,
        S_AXI_PCP_ARESETN         =>  nAreset ,
        -- Slave Interface Write Address Ports
        S_AXI_PCP_AWADDR    =>  inst_pcpAxiLite.AWADDR ,
        S_AXI_PCP_AWVALID   =>  inst_pcpAxiLite.AWVALID ,
        S_AXI_PCP_AWREADY   =>  inst_pcpAxiLite.AWREADY  ,
        -- Slave Interface Write Data Ports
        S_AXI_PCP_WDATA     =>  inst_pcpAxiLite.WDATA   ,
        S_AXI_PCP_WSTRB     =>  inst_pcpAxiLite.WSTRB   ,
        S_AXI_PCP_WVALID    =>  inst_pcpAxiLite.WVALID  ,
        S_AXI_PCP_WREADY    =>  inst_pcpAxiLite.WREADY  ,
        -- Slave Interface Write Response Ports
        S_AXI_PCP_BRESP     =>  inst_pcpAxiLite.BRESP   ,
        S_AXI_PCP_BVALID    =>  inst_pcpAxiLite.BVALID  ,
        S_AXI_PCP_BREADY    =>  inst_pcpAxiLite.BREADY  ,
        -- Slave Interface Read Address Ports
        S_AXI_PCP_ARADDR    =>  inst_pcpAxiLite.ARADDR  ,
        --S_AXI_PCP_ARPROT    =>  ARPROT  ,
        S_AXI_PCP_ARVALID   =>  inst_pcpAxiLite.ARVALID ,
        S_AXI_PCP_ARREADY   =>  inst_pcpAxiLite.ARREADY ,
        -- Slave Interface Read Data Ports
        S_AXI_PCP_RDATA     =>  inst_pcpAxiLite.RDATA   ,
        S_AXI_PCP_RRESP     =>  inst_pcpAxiLite.RRESP   ,
        S_AXI_PCP_RVALID    =>  inst_pcpAxiLite.RVALID  ,
        S_AXI_PCP_RREADY    =>  inst_pcpAxiLite.RREADY  ,

        --! Host Processor System Signals
        S_AXI_HOST_ACLK           =>  clock   ,
        S_AXI_HOST_ARESETN        =>  nAreset ,
        -- Slave Interface Write Address Ports
        S_AXI_HOST_AWADDR   =>  inst_hostAxiLite.AWADDR ,
        S_AXI_HOST_AWVALID  =>  inst_hostAxiLite.AWVALID ,
        S_AXI_HOST_AWREADY  =>  inst_hostAxiLite.AWREADY ,
        -- Slave Interface Write Data Ports
        S_AXI_HOST_WDATA    =>  inst_hostAxiLite.WDATA  ,
        S_AXI_HOST_WSTRB    =>  inst_hostAxiLite.WSTRB  ,
        S_AXI_HOST_WVALID   =>  inst_hostAxiLite.WVALID ,
        S_AXI_HOST_WREADY   =>  inst_hostAxiLite.WREADY ,
        -- Slave Interface Write Response Ports
        S_AXI_HOST_BRESP    =>  inst_hostAxiLite.BRESP  ,
        S_AXI_HOST_BVALID   =>  inst_hostAxiLite.BVALID ,
        S_AXI_HOST_BREADY   =>  inst_hostAxiLite.BREADY ,
        -- Slave Interface Read Address Ports
        S_AXI_HOST_ARADDR   =>  inst_hostAxiLite.ARADDR ,
        S_AXI_HOST_ARVALID  =>  inst_hostAxiLite.ARVALID  ,
        S_AXI_HOST_ARREADY  =>  inst_hostAxiLite.ARREADY    ,
        -- Slave Interface Read Data Ports
        S_AXI_HOST_RDATA    =>  inst_hostAxiLite.RDATA  ,
        S_AXI_HOST_RRESP    =>  inst_hostAxiLite.RRESP  ,
        S_AXI_HOST_RVALID   =>  inst_hostAxiLite.RVALID  ,
        S_AXI_HOST_RREADY   =>  inst_hostAxiLite.RREADY ,


        -- Magic Bridge System Signals
        M_AXI_ACLK          =>  clock   ,
        M_AXI_ARESETN       =>  nAreset ,
        -- Master Interface Write Address
        M_AXI_AWADDR        =>  inst_masterAxiLite.AWADDR ,
        M_AXI_AWPROT        =>  inst_masterAxiLite.AWPROT ,
        M_AXI_AWVALID       =>  inst_masterAxiLite.AWVALID    ,
        M_AXI_AWREADY       =>  inst_masterAxiLite.AWREADY    ,

        -- Master Interface Write Data
        M_AXI_WDATA         =>  inst_masterAxiLite.WDATA  ,
        M_AXI_WSTRB         =>  inst_masterAxiLite.WSTRB  ,
        M_AXI_WVALID        =>  inst_masterAxiLite.WVALID ,
        M_AXI_WREADY        =>  inst_masterAxiLite.WREADY ,

        -- Master Interface Write Response
        M_AXI_BRESP         =>  inst_masterAxiLite.BRESP  ,
        M_AXI_BVALID        =>  inst_masterAxiLite.BVALID ,
        M_AXI_BREADY        =>  inst_masterAxiLite.BREADY ,

        -- Master Interface Read Address
        M_AXI_ARADDR        =>  inst_masterAxiLite.ARADDR ,
        M_AXI_ARPROT        =>  inst_masterAxiLite.ARPROT ,
        M_AXI_ARVALID       =>  inst_masterAxiLite.ARVALID    ,
        M_AXI_ARREADY       =>  inst_masterAxiLite.ARREADY    ,

        -- Master Interface Read Data
        M_AXI_RDATA         =>  inst_masterAxiLite.RDATA  ,
        M_AXI_RRESP         =>  inst_masterAxiLite.RRESP  ,
        M_AXI_RVALID        =>  inst_masterAxiLite.RVALID ,
        M_AXI_RREADY        =>  inst_masterAxiLite.RREADY ,

        irqSync_irq     =>  inr_irqSync_irq ,
        irqOut_irq      =>  ins_irqOut_irq ,

        iExtSync_exsync  =>  coe_ExtSync_exsync ,
        iNodeId_nodeid   =>  coe_NodeId_nodeid   ,
        oPlkLed_lederr   =>  coe_PlkLed_lederr   ,
        oPlkLed_ledst    =>  coe_PlkLed_ledst    ,
        -- Parallel Host Interface
        iParHost_chipselect          => coe_parHost_chipselect ,
        iParHost_read                => coe_parHost_read,
        iParHost_write               => coe_parHost_write ,
        iParHost_addressLatchEnable  => coe_parHost_addressLatchEnable,
        oParHost_acknowledge         => coe_parHost_acknowledge ,
        iParHost_byteenable          => coe_parHost_byteenable ,
        iParHost_address             => coe_parHost_address ,
        iParHost_data_io             => coe_parHost_data_I ,
        oParHost_data_io             => coe_parHost_data_O ,
        oParHost_data_io_tri         => coe_parHost_data_T ,
        iParHost_addressData_io      => coe_parHost_addressData_I ,
        oParHost_addressData_io      => coe_parHost_addressData_O ,
        oParHost_addressData_tri     => coe_parHost_addressData_T
    );

--! AXI Powerlink Communicatio Processor Model
PCP_MODEL: entity work.axiLiteMasterWrapper
generic map
    (
        gAddrWidth  =>  C_AXI_ADDR_WIDTH ,
        gDataWidth  =>  C_AXI_DATA_WIDTH
    )
port map
    (
        -- System Signals
        iAclk          =>  clock                ,
        inAReset       =>  nAreset             ,
        -- Master Interface Write Address
        oAwaddr        =>  inst_pcpAxiLite.AWADDR              ,
        oAwprot        =>  inst_pcpAxiLite.AWPROT              ,
        oAwvalid       =>  inst_pcpAxiLite.AWVALID             ,
        iAwready       =>  inst_pcpAxiLite.AWREADY             ,
        -- Master Interface Write Data
        oWdata         =>  inst_pcpAxiLite.WDATA               ,
        oWstrb         =>  inst_pcpAxiLite.WSTRB               ,
        oWvalid        =>  inst_pcpAxiLite.WVALID              ,
        iWready        =>  inst_pcpAxiLite.WREADY              ,
        -- Master Interface Write Response
        iBresp         =>  inst_pcpAxiLite.BRESP               ,
        iBvalid        =>  inst_pcpAxiLite.BVALID              ,
        oBready        =>  inst_pcpAxiLite.BREADY              ,
        -- Master Interface Read Address
        oAraddr        =>  inst_pcpAxiLite.ARADDR              ,
        oArprot        =>  inst_pcpAxiLite.ARPROT              ,
        oArvalid       =>  inst_pcpAxiLite.ARVALID             ,
        iArready       =>  inst_pcpAxiLite.ARREADY             ,
        -- Master Interface Read Data
        iRdata         =>  inst_pcpAxiLite.RDATA               ,
        iRresp         =>  inst_pcpAxiLite.RRESP               ,
        iRvalid        =>  inst_pcpAxiLite.RVALID              ,
        oRready        =>  inst_pcpAxiLite.RREADY              ,
        -- Avalon Interface Signals
        iAvalonRead         =>  inst_pcpBusMaster.AvalonRead         ,
        iAvalonWrite        =>  inst_pcpBusMaster.AvalonWrite        ,
        iAvalonAddr         =>  inst_pcpBusMaster.AvalonAddr         ,
        iAvalonBE           =>  inst_pcpBusMaster.AvalonBE           ,
        oAvalonWaitReq      =>  inst_pcpBusMaster.AvalonWaitReq      ,
        oAvalonReadValid    =>  inst_pcpBusMaster.AvalonReadValid    ,
        oAvalonReadData     =>  inst_pcpBusMaster.AvalonReadData     ,
        iAvalonWriteData    =>  inst_pcpBusMaster.AvalonWriteData
    );
-- Avalon Master Write/Read operations
--! BusMaster to read instruction and provide input to PCP model
AVALON_BUS_MASTER_PCP:entity work.busMaster
    generic map
     (
        gAddrWidth          => C_AXI_ADDR_WIDTH,
        gDataWidth          => C_AXI_DATA_WIDTH,
        gStimuliFile        => cStimuliFile
     )
    port map
     (
        iRst                =>  inst_pcpBusMaster.BusMasterReset      ,
        iClk                =>  clock                ,
        iEnable             =>  inst_pcpBusMaster.BusMasterEnable     ,
        iAck                =>  inst_pcpBusMaster.BusMasterAck        ,
        iReaddata           =>  inst_pcpBusMaster.AvalonReadData     ,
        oWrite              =>  inst_pcpBusMaster.AvalonWrite        ,
        oRead               =>  inst_pcpBusMaster.AvalonRead         ,
        oSelect             =>  inst_pcpBusMaster.BusMasterSelect     ,
        oAddress            =>  inst_pcpBusMaster.AvalonAddr         ,
        oByteenable         =>  inst_pcpBusMaster.AvalonBE           ,
        oWritedata          =>  inst_pcpBusMaster.AvalonWriteData    ,
        oError              =>  inst_pcpBusMaster.BusMasterError      ,
        oDone               =>  inst_pcpBusMaster.BusMasterDone
    );

-------------------------------------------------------------------------------
-- Bridge Is connecting to Memory through AXI slave
-- DUT_BRIDGE -> AXI_SLAVE->Avalon Memory model
-------------------------------------------------------------------------------
--! Bridge to memory interface
MEMORY_IF_MODEL: entity work.axiLiteSlaveWrapper
generic map
    (
        gBaseAddr          =>  C_MEM_BASEADDR          ,
        gHighAddr          =>  C_MEM_HIGHADDR          ,
        gAddrWidth  =>  C_AXI_ADDR_WIDTH    ,
        gDataWidth  =>  C_AXI_DATA_WIDTH
    )
port map
    (
        -- System Signals
        iAclk          =>  clock                ,
        inAReset       =>  nAreset             ,
        -- Slave Interface Write Address Ports
        iAwaddr        =>  BridgeAddrss         , -- TODO: Check address
        iAwprot        =>  inst_masterAxiLite.AWPROT              ,
        iAwvalid       =>  inst_masterAxiLite.AWVALID             ,
        oAwready       =>  inst_masterAxiLite.AWREADY             ,
        -- Slave Interface Write Data Ports
        iWdata         =>  inst_masterAxiLite.WDATA               ,
        iWstrb         =>  inst_masterAxiLite.WSTRB               ,
        iWvalid        =>  inst_masterAxiLite.WVALID              ,
        oWready        =>  inst_masterAxiLite.WREADY              ,
        -- Slave Interface Write Response Ports
        oBresp         =>  inst_masterAxiLite.BRESP               ,
        oBvalid        =>  inst_masterAxiLite.BVALID              ,
        iBready        =>  inst_masterAxiLite.BREADY              ,
        -- Slave Interface Read Address Ports
        iAraddr        =>  inst_masterAxiLite.ARADDR              ,
        iArprot        =>  inst_masterAxiLite.ARPROT              ,
        iArvalid       =>  inst_masterAxiLite.ARVALID             ,
        oArready       =>  inst_masterAxiLite.ARREADY             ,
        -- Slave Interface Read Data Ports
        oRdata         =>  inst_masterAxiLite.RDATA               ,
        oRresp         =>  inst_masterAxiLite.RRESP               ,
        oRvalid        =>  inst_masterAxiLite.RVALID              ,
        iRready        =>  inst_masterAxiLite.RREADY              ,
        --Avalon Interface
        oAvsAddress         =>  inst_memoryBusMaster.AvalonAddr         ,
        oAvsByteenable      =>  inst_memoryBusMaster.AvalonBE           ,
        oAvsRead            =>  inst_memoryBusMaster.AvalonRead         ,
        oAvsWrite           =>  inst_memoryBusMaster.AvalonWrite        ,
        oAvsWritedata       =>  inst_memoryBusMaster.AvalonWriteData    ,
        iAvsReaddata        =>  inst_memoryBusMaster.AvalonReadData     ,
        iAvsWaitrequest     =>  inst_memoryBusMaster.AvalonWaitReq
    );
    BridgeAddrss <= "00" & inst_masterAxiLite.AWADDR(29 downto 0) ;

--! Memory Model
theRam : entity work.spRam
        port map (
            iClk        => clock,
            iWrite      => inst_memoryBusMaster.AvalonWrite,
            iRead       => inst_memoryBusMaster.AvalonRead,
            iAddress    => inst_memoryBusMaster.AvalonAddr(cRamAddrWidth-1 downto 2),
            iByteenable => inst_memoryBusMaster.AvalonBE,
            iWritedata  => inst_memoryBusMaster.AvalonWriteData,
            oReaddata   => inst_memoryBusMaster.AvalonReadData,
            oAck        => memroyAck
        );

        inst_memoryBusMaster.AvalonWaitReq <= not memroyAck ;

-------------------------------------------------------------------------
-- Host AXI Interface IP master
-------------------------------------------------------------------------
genHostAXIMaster : if cHostIfType = 0 generate

begin
--! Host Processor Model
HOST_MODEL: entity work.axiLiteMasterWrapper
generic map
    (
        gAddrWidth  =>  C_AXI_ADDR_WIDTH ,
        gDataWidth  =>  C_AXI_DATA_WIDTH
    )
port map
    (
        -- System Signals
        iAclk          =>  clock                ,
        inAReset       =>  nAreset             ,
        -- Master Interface Write Address
        oAwaddr        =>  inst_hostAxiLite.AWADDR              ,
        oAwprot        =>  inst_hostAxiLite.AWPROT              ,
        oAwvalid       =>  inst_hostAxiLite.AWVALID             ,
        iAwready       =>  inst_hostAxiLite.AWREADY             ,
        -- Master Interface Write Data
        oWdata         =>  inst_hostAxiLite.WDATA               ,
        oWstrb         =>  inst_hostAxiLite.WSTRB               ,
        oWvalid        =>  inst_hostAxiLite.WVALID              ,
        iWready        =>  inst_hostAxiLite.WREADY              ,
        -- Master Interface Write Response
        iBresp         =>  inst_hostAxiLite.BRESP               ,
        iBvalid        =>  inst_hostAxiLite.BVALID              ,
        oBready        =>  inst_hostAxiLite.BREADY              ,
        -- Master Interface Read Address
        oAraddr        =>  inst_hostAxiLite.ARADDR              ,
        oArprot        =>  inst_hostAxiLite.ARPROT              ,
        oArvalid       =>  inst_hostAxiLite.ARVALID             ,
        iArready       =>  inst_hostAxiLite.ARREADY             ,
        -- Master Interface Read Data
        iRdata         =>  inst_hostAxiLite.RDATA               ,
        iRresp         =>  inst_hostAxiLite.RRESP               ,
        iRvalid        =>  inst_hostAxiLite.RVALID              ,
        oRready        =>  inst_hostAxiLite.RREADY              ,
        -- Avalon Interface Signals
        iAvalonRead         =>  inst_hostBusMaster.AvalonRead         ,
        iAvalonWrite        =>  inst_hostBusMaster.AvalonWrite        ,
        iAvalonAddr         =>  inst_hostBusMaster.AvalonAddr         ,
        iAvalonBE           =>  inst_hostBusMaster.AvalonBE           ,
        oAvalonWaitReq      =>  inst_hostBusMaster.AvalonWaitReq      ,
        oAvalonReadValid    =>  inst_hostBusMaster.AvalonReadValid    ,
        oAvalonReadData     =>  inst_hostBusMaster.AvalonReadData     ,
        iAvalonWriteData    =>  inst_hostBusMaster.AvalonWriteData
    );

--TODO: Use common host Busmaster for both parallel & host AXI
--! BusMaster to read instruction and provide input to Host model
AVALON_BUS_MASTER_HOST:entity work.busMaster
    generic map
     (
        gAddrWidth          => C_AXI_ADDR_WIDTH,
        gDataWidth          => C_AXI_DATA_WIDTH,
        gStimuliFile        => cHostStimuliFile
     )
    port map
     (
        iRst                =>  inst_hostBusMaster.BusMasterReset      ,
        iClk                =>  clock                ,
        iEnable             =>  inst_hostBusMaster.BusMasterEnable     ,
        iAck                =>  inst_hostBusMaster.BusMasterAck        ,
        iReaddata           =>  inst_hostBusMaster.AvalonReadData     ,
        oWrite              =>  inst_hostBusMaster.AvalonWrite        ,
        oRead               =>  inst_hostBusMaster.AvalonRead         ,
        oSelect             =>  inst_hostBusMaster.BusMasterSelect     ,
        oAddress            =>  inst_hostBusMaster.AvalonAddr         ,
        oByteenable         =>  inst_hostBusMaster.AvalonBE           ,
        oWritedata          =>  inst_hostBusMaster.AvalonWriteData    ,
        oError              =>  inst_hostBusMaster.BusMasterError      ,
        oDone               =>  inst_hostBusMaster.BusMasterDone
    );
    --hBusMasterReset <= not nAreset ;
    inst_hostBusMaster.BusMasterReset <= inst_pcpBusMaster.BusMasterDone ;
    inst_hostBusMaster.BusMasterEnable <= '1' ;
    inst_hostBusMaster.BusMasterAck   <= not inst_hostBusMaster.AvalonWaitReq ;
    --TODO: Add Protocol Checker also
end generate ;
-------------------------------------------------------------------------
--  Parallel Interface for Hsot
-------------------------------------------------------------------------
genParallelMaster : if cHostIfType = 1 generate

signal paral_Wordaddress : std_logic_vector (31 downto 0) ;

begin
--! Parallel Interface Master
PARALLEL_INTERFACE_MASTER : entity work.parallelMaster
        generic map (
            gDataWidth => 16,
            gMultiplex => 0
        )
        port map (
            iClk => clock,
            iRst => inst_hostBusMaster.BusMasterReset,
            -- Avalon Interface
            iAvalonRead         =>  inst_hostBusMaster.AvalonRead         ,
            iAvalonWrite        =>  inst_hostBusMaster.AvalonWrite        ,
            iAvalonAddr         =>  paral_Wordaddress         ,
            iAvalonBE           =>  inst_hostBusMaster.AvalonBE           ,
            oAvalonWaitReq      =>  inst_hostBusMaster.AvalonWaitReq      ,
            oAvalonReadValid    =>  inst_hostBusMaster.AvalonReadValid    ,
            oAvalonReadData     =>  inst_hostBusMaster.AvalonReadData     ,
            iAvalonWriteData    =>  inst_hostBusMaster.AvalonWriteData    ,
            -- Parallel Interface
            oParHostChipselect         => coe_parHost_chipselect,
            oParHostRead               => coe_parHost_read,
            oParHostWrite              => coe_parHost_write,
            oParHostAddressLatchEnable => coe_parHost_addressLatchEnable,
            iParHostAcknowledge        => coe_parHost_acknowledge,
            oParHostByteenable         => coe_parHost_byteenable,
            oParHostAddress            => coe_parHost_address,
            iParHostData               => coe_parHost_data_O,
            oParHostData               => coe_parHost_data_I,
            iParHostDataEnable         => coe_parHost_data_T,
            iParHostAddressData        => coe_parHost_addressData_O,
            oParHostAddressData        => coe_parHost_addressData_I,
            iParHostAddressDataEnable  => coe_parHost_addressData_T
        );
--! BusMaster to read instruction and provide input to Parallel Master model
AVALON_HOST_BUS_MASTER:entity work.busMaster
    generic map
     (
        gAddrWidth          => C_AXI_ADDR_WIDTH,
        gDataWidth          => C_AXI_DATA_WIDTH,
        gStimuliFile        => cHostStimuliFile
     )
    port map
     (
        iRst                =>  inst_hostBusMaster.BusMasterReset      ,
        iClk                =>  clock                ,
        iEnable             =>  inst_hostBusMaster.BusMasterEnable     ,
        iAck                =>  inst_hostBusMaster.BusMasterAck        ,
        iReaddata           =>  inst_hostBusMaster.AvalonReadData     ,
        oWrite              =>  inst_hostBusMaster.AvalonWrite        ,
        oRead               =>  inst_hostBusMaster.AvalonRead         ,
        oSelect             =>  inst_hostBusMaster.BusMasterSelect     ,
        oAddress            =>  inst_hostBusMaster.AvalonAddr         ,
        oByteenable         =>  inst_hostBusMaster.AvalonBE           ,
        oWritedata          =>  inst_hostBusMaster.AvalonWriteData    ,
        oError              =>  inst_hostBusMaster.BusMasterError      ,
        oDone               =>  inst_hostBusMaster.BusMasterDone
    );
    --hBusMasterReset <= not nAreset ;
    inst_hostBusMaster.BusMasterReset <= inst_hostBusMaster.BusMasterDone ;
    inst_hostBusMaster.BusMasterEnable <= '1' ;
    inst_hostBusMaster.BusMasterAck   <= not inst_hostBusMaster.AvalonWaitReq ;

    paral_Wordaddress <= '0' & inst_hostBusMaster.AvalonAddr (31 downto 1) ;
end generate;

-------------------------------------------------------------------------------
--  General Settings
-------------------------------------------------------------------------------
 -- Clock & Reset
 theClkGen : entity work.clkgen
        generic map (
            gPeriod => 10 ns
        )
        port map (
            iDone   => inst_pcpBusMaster.BusMasterDone,
            oClk    => clock
        );
--clock   <= '0' after 0ns;
nAreset <=  '0' after 0 ns,
            '1' after 300 ns;
inst_pcpBusMaster.BusMasterReset <= not nAreset ;
--clock <=  not clock  after cPeriode/2 when inst_pcpBusMaster.BusMasterDone /= cActivated else
--                 '0' after cPeriode/2;
inst_pcpBusMaster.BusMasterEnable <= '1' ;
inst_pcpBusMaster.BusMasterAck    <= not inst_pcpBusMaster.AvalonWaitReq    ;

end bhv ;