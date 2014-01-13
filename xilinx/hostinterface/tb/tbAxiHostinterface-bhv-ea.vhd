-------------------------------------------------------------------------------
--! @file tb_axi_hostinterface_ip.vhd
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


entity tb_axi_hostinterface is
generic(
    --! PCP Simulation file
    gPcpStim : string := "text.txt";
    --! Host simulation file
    gHostStim : string := "text.txt";
    --! Host Model 0-Parallel 1-AXI
    gHostIfModel : natural:= 0
    );
end entity tb_axi_hostinterface ;

architecture bhv of  tb_axi_hostinterface is

constant C_AXI_ADDR_WIDTH : integer   := 32;
constant C_AXI_DATA_WIDTH : integer   := 32;
constant C_BASEADDR : std_logic_vector
                             (C_AXI_ADDR_WIDTH-1 downto 0):= x"7C000000";
constant C_HIGHADDR : std_logic_vector
                             (C_AXI_ADDR_WIDTH-1 downto 0) := x"7C0FFFFF";
constant C_HOST_BASEADDR : std_logic_vector
                             (C_AXI_ADDR_WIDTH-1 downto 0) := x"8C000000";
constant C_HOST_HIGHADDR : std_logic_vector
                             (C_AXI_ADDR_WIDTH-1 downto 0) := x"8C0FFFFF";

constant C_MEM_BASEADDR   : std_logic_vector
                             (C_AXI_ADDR_WIDTH-1 downto 0) := x"30000000";
constant C_MEM_HIGHADDR   : std_logic_vector
                             (C_AXI_ADDR_WIDTH-1 downto 0) := x"FFFFFFFF";

signal clock    : std_logic ;
signal nAreset : std_logic ;

type Axilite is record
    --Write Address
    AWVALID : std_logic ;
    AWREADY : std_logic ;
    AWADDR  : std_logic_vector (C_AXI_ADDR_WIDTH-1 downto 0);
    --Write Data
    WVALID  : std_logic ;
    WREADY  : std_logic ;
    WDATA   : std_logic_vector (C_AXI_DATA_WIDTH-1 downto 0);
    --Write Response
    BVALID  : std_logic ;
    BREADY  : std_logic ;
    BRESP   : std_logic_vector (1 downto 0);
    --Read Address
    ARVALID : std_logic ;
    ARREADY : std_logic ;
    ARADDR  : std_logic_vector (C_AXI_ADDR_WIDTH-1 downto 0);
    --Read Data
    RVALID  : std_logic ;
    RREADY  : std_logic ;
    RDATA   : std_logic_vector (C_AXI_DATA_WIDTH-1 downto 0);

    ARPROT  : std_logic_vector (2 downto 0);
    AWPROT  : std_logic_vector (2 downto 0);
    WSTRB   : std_logic_vector (3 downto 0);
    RRESP   : std_logic_vector (1 downto 0);

end record;

signal tPcpAxilite    : Axilite ;
signal tHostAxilite   : Axilite ;
signal tMasterAxilite : Axilite ;

type BusMaster is record

    AvalonRead          : std_logic                            ;
    AvalonWrite         : std_logic                            ;
    AvalonAddr          : std_logic_vector   (31 downto 0)     ;
    AvalonBE            : std_logic_vector   (3 downto 0)      ;
    AvalonWaitReq       : std_logic                            ;
    AvalonReadValid     : std_logic                            ;
    AvalonReadData      : std_logic_vector   (31 downto 0)     ;
    AvalonWriteData     : std_logic_vector   (31 downto 0)     ;

    BusMasterEnable       : std_logic ;
    BusMasterAck          : std_logic ;
    BusMasterSelect       : std_logic ;
    BusMasterError        : std_logic ;
    BusMasterDone         : std_logic ;
    BusMasterReset        : std_logic ;

end record;

signal tPcpBusMaster  : BusMaster ;
signal tHostBusMaster : BusMaster ;
signal tMemoModel     : BusMaster ;


signal  memroyAck            : std_logic                            ;

--Powerlink Interface Signals
signal inr_irqSync_irq                 : std_logic;
signal ins_irqOut_irq                  : std_logic;
signal coe_ExtSync_exsync              : std_logic;
signal coe_NodeId_nodeid               : std_logic_vector(7 downto 0) := x"F0";
signal coe_PlkLed_lederr               : std_logic;
signal coe_PlkLed_ledst                : std_logic;
--Parallel Interface Signals
constant cParallelDataWidth             : natural := 16;
signal coe_parHost_chipselect          : std_logic;
signal coe_parHost_read                : std_logic;
signal coe_parHost_write               : std_logic;
signal coe_parHost_addressLatchEnable  : std_logic;
signal coe_parHost_acknowledge         : std_logic;
signal coe_parHost_byteenable : std_logic_vector
                                         (cParallelDataWidth/8-1 downto 0);
signal coe_parHost_address    : std_logic_vector(15 downto 0);
signal coe_parHost_data_I     : std_logic_vector(cParallelDataWidth-1 downto 0);
signal coe_parHost_data_O     : std_logic_vector(cParallelDataWidth-1 downto 0);
signal coe_parHost_data_T     : std_logic ;
signal coe_parHost_addressData_I : std_logic_vector
                                              (cParallelDataWidth-1 downto 0);
signal coe_parHost_addressData_O : std_logic_vector
                                              (cParallelDataWidth-1 downto 0);
signal coe_parHost_addressData_T : std_logic ;

-- Test case
constant cPeriode       : time := 10 ns;

-- Modelsim Simulation with Script
constant cStimuliFile     : string  := gPcpStim ;
constant cHostStimuliFile : string  := gHostStim ;
constant cHostIfType      : integer := gHostIfModel ;

-- Isim Simulation
-- TODO: remove absolute path or try generic
--constant cStimuliFile     : string  := "E:/XilinxMN/HDL_GitRepo/VHDL_IP-Cores/xilinx/axiwrapper/tb/tbPCPMasterBhv_TB_stim.txt";
--constant cHostStimuliFile : string  := "E:/XilinxMN/HDL_GitRepo/VHDL_IP-Cores/xilinx/axiwrapper/tb/tbHostMasterBhv_TB_stim.txt";
--constant cHostIfType      : integer := 0 ; --1 Parallel 0- AXI



constant cRamSize       : natural := 640 * 1024; --[byte]
constant cRamAddrWidth  : natural := LogDualis(cRamSize);

constant cVersionMajor      : integer := 255 ;
constant cVersionMinor      : integer := 255 ;
constant cVersionRevision   : integer := 255 ;
constant cVersionCount      : integer := 0 ;
constant cBaseDynBuf0       : integer := 2048; --x"00800" ;
constant cBaseDynBuf1       : integer := 4096; --x"01000" ;
constant cBaseErrCntr       : integer := 6144; --x"01800" ;
constant cBaseTxNmtQ        : integer := 10240;--x"02800" ;
constant cBaseTxGenQ        : integer := 14336;--x"03800" ;
constant cBaseTxSynQ        : integer := 18432;--x"04800" ;
constant cBaseTxVetQ        : integer := 22528;--x"05800" ;
constant cBaseRxVetQ        : integer := 26624;--x"06800" ;
constant cBaseK2UQ          : integer := 28672;--x"07000" ;
constant cBaseU2KQ          : integer := 36864;--x"09000" ;
constant cBaseTpdo          : integer := 45056;--x"0B000" ;
constant cBaseRpdo          : integer := 57344;--x"0E000" ;
constant cBaseRes           : integer := 81920;--x"14000" ;

signal BridgeAddrss : std_logic_vector (31 downto 0) ;

begin

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
        S_AXI_PCP_AWADDR    =>  tpcpAxilite.AWADDR ,
        S_AXI_PCP_AWVALID   =>  tpcpAxilite.AWVALID ,
        S_AXI_PCP_AWREADY   =>  tpcpAxilite.AWREADY  ,
        -- Slave Interface Write Data Ports
        S_AXI_PCP_WDATA     =>  tpcpAxilite.WDATA   ,
        S_AXI_PCP_WSTRB     =>  tpcpAxilite.WSTRB   ,
        S_AXI_PCP_WVALID    =>  tpcpAxilite.WVALID  ,
        S_AXI_PCP_WREADY    =>  tpcpAxilite.WREADY  ,
        -- Slave Interface Write Response Ports
        S_AXI_PCP_BRESP     =>  tpcpAxilite.BRESP   ,
        S_AXI_PCP_BVALID    =>  tpcpAxilite.BVALID  ,
        S_AXI_PCP_BREADY    =>  tpcpAxilite.BREADY  ,
        -- Slave Interface Read Address Ports
        S_AXI_PCP_ARADDR    =>  tpcpAxilite.ARADDR  ,
        --S_AXI_PCP_ARPROT    =>  ARPROT  ,
        S_AXI_PCP_ARVALID   =>  tpcpAxilite.ARVALID ,
        S_AXI_PCP_ARREADY   =>  tpcpAxilite.ARREADY ,
        -- Slave Interface Read Data Ports
        S_AXI_PCP_RDATA     =>  tpcpAxilite.RDATA   ,
        S_AXI_PCP_RRESP     =>  tpcpAxilite.RRESP   ,
        S_AXI_PCP_RVALID    =>  tpcpAxilite.RVALID  ,
        S_AXI_PCP_RREADY    =>  tpcpAxilite.RREADY  ,

        --! Host Processor System Signals
        S_AXI_HOST_ACLK           =>  clock   ,
        S_AXI_HOST_ARESETN        =>  nAreset ,
        -- Slave Interface Write Address Ports
        S_AXI_HOST_AWADDR   =>  tHostAxilite.AWADDR ,
        S_AXI_HOST_AWVALID  =>  tHostAxilite.AWVALID ,
        S_AXI_HOST_AWREADY  =>  tHostAxilite.AWREADY ,
        -- Slave Interface Write Data Ports
        S_AXI_HOST_WDATA    =>  tHostAxilite.WDATA  ,
        S_AXI_HOST_WSTRB    =>  tHostAxilite.WSTRB  ,
        S_AXI_HOST_WVALID   =>  tHostAxilite.WVALID ,
        S_AXI_HOST_WREADY   =>  tHostAxilite.WREADY ,
        -- Slave Interface Write Response Ports
        S_AXI_HOST_BRESP    =>  tHostAxilite.BRESP  ,
        S_AXI_HOST_BVALID   =>  tHostAxilite.BVALID ,
        S_AXI_HOST_BREADY   =>  tHostAxilite.BREADY ,
        -- Slave Interface Read Address Ports
        S_AXI_HOST_ARADDR   =>  tHostAxilite.ARADDR ,
        S_AXI_HOST_ARVALID  =>  tHostAxilite.ARVALID  ,
        S_AXI_HOST_ARREADY  =>  tHostAxilite.ARREADY    ,
        -- Slave Interface Read Data Ports
        S_AXI_HOST_RDATA    =>  tHostAxilite.RDATA  ,
        S_AXI_HOST_RRESP    =>  tHostAxilite.RRESP  ,
        S_AXI_HOST_RVALID   =>  tHostAxilite.RVALID  ,
        S_AXI_HOST_RREADY   =>  tHostAxilite.RREADY ,


        -- Magic Bridge System Signals
        M_AXI_ACLK          =>  clock   ,
        M_AXI_ARESETN       =>  nAreset ,
        -- Master Interface Write Address
        M_AXI_AWADDR        =>  tMasterAxilite.AWADDR ,
        M_AXI_AWPROT        =>  tMasterAxilite.AWPROT ,
        M_AXI_AWVALID       =>  tMasterAxilite.AWVALID    ,
        M_AXI_AWREADY       =>  tMasterAxilite.AWREADY    ,

        -- Master Interface Write Data
        M_AXI_WDATA         =>  tMasterAxilite.WDATA  ,
        M_AXI_WSTRB         =>  tMasterAxilite.WSTRB  ,
        M_AXI_WVALID        =>  tMasterAxilite.WVALID ,
        M_AXI_WREADY        =>  tMasterAxilite.WREADY ,

        -- Master Interface Write Response
        M_AXI_BRESP         =>  tMasterAxilite.BRESP  ,
        M_AXI_BVALID        =>  tMasterAxilite.BVALID ,
        M_AXI_BREADY        =>  tMasterAxilite.BREADY ,

        -- Master Interface Read Address
        M_AXI_ARADDR        =>  tMasterAxilite.ARADDR ,
        M_AXI_ARPROT        =>  tMasterAxilite.ARPROT ,
        M_AXI_ARVALID       =>  tMasterAxilite.ARVALID    ,
        M_AXI_ARREADY       =>  tMasterAxilite.ARREADY    ,

        -- Master Interface Read Data
        M_AXI_RDATA         =>  tMasterAxilite.RDATA  ,
        M_AXI_RRESP         =>  tMasterAxilite.RRESP  ,
        M_AXI_RVALID        =>  tMasterAxilite.RVALID ,
        M_AXI_RREADY        =>  tMasterAxilite.RREADY ,

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
PCP_MODEL: entity work.axi_lite_master_wrapper
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
        oAwaddr        =>  tpcpAxilite.AWADDR              ,
        oAwprot        =>  tpcpAxilite.AWPROT              ,
        oAwvalid       =>  tpcpAxilite.AWVALID             ,
        iAwready       =>  tpcpAxilite.AWREADY             ,
        -- Master Interface Write Data
        oWdata         =>  tpcpAxilite.WDATA               ,
        oWstrb         =>  tpcpAxilite.WSTRB               ,
        oWvalid        =>  tpcpAxilite.WVALID              ,
        iWready        =>  tpcpAxilite.WREADY              ,
        -- Master Interface Write Response
        iBresp         =>  tpcpAxilite.BRESP               ,
        iBvalid        =>  tpcpAxilite.BVALID              ,
        oBready        =>  tpcpAxilite.BREADY              ,
        -- Master Interface Read Address
        oAraddr        =>  tpcpAxilite.ARADDR              ,
        oArprot        =>  tpcpAxilite.ARPROT              ,
        oArvalid       =>  tpcpAxilite.ARVALID             ,
        iArready       =>  tpcpAxilite.ARREADY             ,
        -- Master Interface Read Data
        iRdata         =>  tpcpAxilite.RDATA               ,
        iRresp         =>  tpcpAxilite.RRESP               ,
        iRvalid        =>  tpcpAxilite.RVALID              ,
        oRready        =>  tpcpAxilite.RREADY              ,
        -- Avalon Interface Signals
        iAvalonRead         =>  tPcpBusMaster.AvalonRead         ,
        iAvalonWrite        =>  tPcpBusMaster.AvalonWrite        ,
        iAvalonAddr         =>  tPcpBusMaster.AvalonAddr         ,
        iAvalonBE           =>  tPcpBusMaster.AvalonBE           ,
        oAvalonWaitReq      =>  tPcpBusMaster.AvalonWaitReq      ,
        oAvalonReadValid    =>  tPcpBusMaster.AvalonReadValid    ,
        oAvalonReadData     =>  tPcpBusMaster.AvalonReadData     ,
        iAvalonWriteData    =>  tPcpBusMaster.AvalonWriteData
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
        iRst                =>  tPcpBusMaster.BusMasterReset      ,
        iClk                =>  clock                ,
        iEnable             =>  tPcpBusMaster.BusMasterEnable     ,
        iAck                =>  tPcpBusMaster.BusMasterAck        ,
        iReaddata           =>  tPcpBusMaster.AvalonReadData     ,
        oWrite              =>  tPcpBusMaster.AvalonWrite        ,
        oRead               =>  tPcpBusMaster.AvalonRead         ,
        oSelect             =>  tPcpBusMaster.BusMasterSelect     ,
        oAddress            =>  tPcpBusMaster.AvalonAddr         ,
        oByteenable         =>  tPcpBusMaster.AvalonBE           ,
        oWritedata          =>  tPcpBusMaster.AvalonWriteData    ,
        oError              =>  tPcpBusMaster.BusMasterError      ,
        oDone               =>  tPcpBusMaster.BusMasterDone
    );

-------------------------------------------------------------------------------
-- Bridge Is connecting to Memory through AXI slave
-- DUT_BRIDGE -> AXI_SLAVE->Avalon Memory model
-------------------------------------------------------------------------------
--! Bridge to memory interface
MEMORY_IF_MODEL: entity work.axi_lite_slave_wrapper
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
        iAwprot        =>  tMasterAxilite.AWPROT              ,
        iAwvalid       =>  tMasterAxilite.AWVALID             ,
        oAwready       =>  tMasterAxilite.AWREADY             ,
        -- Slave Interface Write Data Ports
        iWdata         =>  tMasterAxilite.WDATA               ,
        iWstrb         =>  tMasterAxilite.WSTRB               ,
        iWvalid        =>  tMasterAxilite.WVALID              ,
        oWready        =>  tMasterAxilite.WREADY              ,
        -- Slave Interface Write Response Ports
        oBresp         =>  tMasterAxilite.BRESP               ,
        oBvalid        =>  tMasterAxilite.BVALID              ,
        iBready        =>  tMasterAxilite.BREADY              ,
        -- Slave Interface Read Address Ports
        iAraddr        =>  tMasterAxilite.ARADDR              ,
        iArprot        =>  tMasterAxilite.ARPROT              ,
        iArvalid       =>  tMasterAxilite.ARVALID             ,
        oArready       =>  tMasterAxilite.ARREADY             ,
        -- Slave Interface Read Data Ports
        oRdata         =>  tMasterAxilite.RDATA               ,
        oRresp         =>  tMasterAxilite.RRESP               ,
        oRvalid        =>  tMasterAxilite.RVALID              ,
        iRready        =>  tMasterAxilite.RREADY              ,
        --Avalon Interface
        oAvsAddress         =>  tMemoModel.AvalonAddr         ,
        oAvsByteenable      =>  tMemoModel.AvalonBE           ,
        oAvsRead            =>  tMemoModel.AvalonRead         ,
        oAvsWrite           =>  tMemoModel.AvalonWrite        ,
        oAvsWritedata       =>  tMemoModel.AvalonWriteData    ,
        iAvsReaddata        =>  tMemoModel.AvalonReadData     ,
        iAvsWaitrequest     =>  tMemoModel.AvalonWaitReq
    );
    BridgeAddrss <= "00" & tMasterAxilite.AWADDR(29 downto 0) ;

--! Memory Model
theRam : entity work.spRam
        port map (
            iClk        => clock,
            iWrite      => tMemoModel.AvalonWrite,
            iRead       => tMemoModel.AvalonRead,
            iAddress    => tMemoModel.AvalonAddr(cRamAddrWidth-1 downto 2),
            iByteenable => tMemoModel.AvalonBE,
            iWritedata  => tMemoModel.AvalonWriteData,
            oReaddata   => tMemoModel.AvalonReadData,
            oAck        => memroyAck
        );

        tMemoModel.AvalonWaitReq <= not memroyAck ;

-------------------------------------------------------------------------
-- Host AXI Interface IP master
-------------------------------------------------------------------------
genHostAXIMaster : if cHostIfType = 0 generate

begin
--! Host Processor Model
HOST_MODEL: entity work.axi_lite_master_wrapper
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
        oAwaddr        =>  tHostAxilite.AWADDR              ,
        oAwprot        =>  tHostAxilite.AWPROT              ,
        oAwvalid       =>  tHostAxilite.AWVALID             ,
        iAwready       =>  tHostAxilite.AWREADY             ,
        -- Master Interface Write Data
        oWdata         =>  tHostAxilite.WDATA               ,
        oWstrb         =>  tHostAxilite.WSTRB               ,
        oWvalid        =>  tHostAxilite.WVALID              ,
        iWready        =>  tHostAxilite.WREADY              ,
        -- Master Interface Write Response
        iBresp         =>  tHostAxilite.BRESP               ,
        iBvalid        =>  tHostAxilite.BVALID              ,
        oBready        =>  tHostAxilite.BREADY              ,
        -- Master Interface Read Address
        oAraddr        =>  tHostAxilite.ARADDR              ,
        oArprot        =>  tHostAxilite.ARPROT              ,
        oArvalid       =>  tHostAxilite.ARVALID             ,
        iArready       =>  tHostAxilite.ARREADY             ,
        -- Master Interface Read Data
        iRdata         =>  tHostAxilite.RDATA               ,
        iRresp         =>  tHostAxilite.RRESP               ,
        iRvalid        =>  tHostAxilite.RVALID              ,
        oRready        =>  tHostAxilite.RREADY              ,
        -- Avalon Interface Signals
        iAvalonRead         =>  tHostBusMaster.AvalonRead         ,
        iAvalonWrite        =>  tHostBusMaster.AvalonWrite        ,
        iAvalonAddr         =>  tHostBusMaster.AvalonAddr         ,
        iAvalonBE           =>  tHostBusMaster.AvalonBE           ,
        oAvalonWaitReq      =>  tHostBusMaster.AvalonWaitReq      ,
        oAvalonReadValid    =>  tHostBusMaster.AvalonReadValid    ,
        oAvalonReadData     =>  tHostBusMaster.AvalonReadData     ,
        iAvalonWriteData    =>  tHostBusMaster.AvalonWriteData
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
        iRst                =>  tHostBusMaster.BusMasterReset      ,
        iClk                =>  clock                ,
        iEnable             =>  tHostBusMaster.BusMasterEnable     ,
        iAck                =>  tHostBusMaster.BusMasterAck        ,
        iReaddata           =>  tHostBusMaster.AvalonReadData     ,
        oWrite              =>  tHostBusMaster.AvalonWrite        ,
        oRead               =>  tHostBusMaster.AvalonRead         ,
        oSelect             =>  tHostBusMaster.BusMasterSelect     ,
        oAddress            =>  tHostBusMaster.AvalonAddr         ,
        oByteenable         =>  tHostBusMaster.AvalonBE           ,
        oWritedata          =>  tHostBusMaster.AvalonWriteData    ,
        oError              =>  tHostBusMaster.BusMasterError      ,
        oDone               =>  tHostBusMaster.BusMasterDone
    );
    --hBusMasterReset <= not nAreset ;
    tHostBusMaster.BusMasterReset <= tPcpBusMaster.BusMasterDone ;
    tHostBusMaster.BusMasterEnable <= '1' ;
    tHostBusMaster.BusMasterAck   <= not tHostBusMaster.AvalonWaitReq ;
    --TODO: Add Protocol Checker also
end generate ;
-------------------------------------------------------------------------
--  Parallel Interface for Hsot
-------------------------------------------------------------------------
genParallelMaster : if cHostIfType = 1 generate

signal paral_Wordaddress : std_logic_vector (31 downto 0) ;

begin
--! Parallel Interface Master
PARALLEL_INTERFACE_MASTER : entity work.parallel_master
        generic map (
            gDataWidth => 16,
            gMultiplex => 0
        )
        port map (
            iClk => clock,
            iRst => tHostBusMaster.BusMasterReset,
            -- Avalon Interface
            iAvalonRead         =>  tHostBusMaster.AvalonRead         ,
            iAvalonWrite        =>  tHostBusMaster.AvalonWrite        ,
            iAvalonAddr         =>  paral_Wordaddress         ,
            iAvalonBE           =>  tHostBusMaster.AvalonBE           ,
            oAvalonWaitReq      =>  tHostBusMaster.AvalonWaitReq      ,
            oAvalonReadValid    =>  tHostBusMaster.AvalonReadValid    ,
            oAvalonReadData     =>  tHostBusMaster.AvalonReadData     ,
            iAvalonWriteData    =>  tHostBusMaster.AvalonWriteData    ,
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
        iRst                =>  tHostBusMaster.BusMasterReset      ,
        iClk                =>  clock                ,
        iEnable             =>  tHostBusMaster.BusMasterEnable     ,
        iAck                =>  tHostBusMaster.BusMasterAck        ,
        iReaddata           =>  tHostBusMaster.AvalonReadData     ,
        oWrite              =>  tHostBusMaster.AvalonWrite        ,
        oRead               =>  tHostBusMaster.AvalonRead         ,
        oSelect             =>  tHostBusMaster.BusMasterSelect     ,
        oAddress            =>  tHostBusMaster.AvalonAddr         ,
        oByteenable         =>  tHostBusMaster.AvalonBE           ,
        oWritedata          =>  tHostBusMaster.AvalonWriteData    ,
        oError              =>  tHostBusMaster.BusMasterError      ,
        oDone               =>  tHostBusMaster.BusMasterDone
    );
    --hBusMasterReset <= not nAreset ;
    tHostBusMaster.BusMasterReset <= tHostBusMaster.BusMasterDone ;
    tHostBusMaster.BusMasterEnable <= '1' ;
    tHostBusMaster.BusMasterAck   <= not tHostBusMaster.AvalonWaitReq ;

    paral_Wordaddress <= '0' & tHostBusMaster.AvalonAddr (31 downto 1) ;
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
            iDone   => tPcpBusMaster.BusMasterDone,
            oClk    => clock
        );
--clock   <= '0' after 0ns;
nAreset <=  '0' after 0 ns,
            '1' after 300 ns;
tPcpBusMaster.BusMasterReset <= not nAreset ;
--clock <=  not clock  after cPeriode/2 when tPcpBusMaster.BusMasterDone /= cActivated else
--                 '0' after cPeriode/2;
tPcpBusMaster.BusMasterEnable <= '1' ;
tPcpBusMaster.BusMasterAck    <= not tPcpBusMaster.AvalonWaitReq    ;

end bhv ;