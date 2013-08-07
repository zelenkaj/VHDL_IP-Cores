-------------------------------------------------------------------------------
--! @file tripleBufferRtl.vhd
--
--! @brief Triple Buffer
--
--! @details This is the triple buffer toplevel entity, which instantiates
--! several triple buffer instances build out of a triple bridge and
--! triple logic components for each port.
--! The triple buffers must be configured as consumer or producer individually.
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
use work.tripleBufferPkg.all;

entity tripleBuffer is
    generic (
        --! Address width
        gAddressWidth   : natural := 3;
        --! Number of input buffers
        gInputBuffers   : natural := 2;
        --! Input buffer base addresses
        gInputBase      : std_logic_vector := x"241404";
        --! Triple buffer mapping
        gTriBufOffset   : std_logic_vector := x"403020201000";
        --! Size of DPRAM
        gDprSize        : natural := 123;
        --! Port A configuration (0b0 = consumer and 0b1 = producer)
        gPortAconfig    : std_logic_vector := "10";
        --! Enable Stream access at port A (0 = false, otherwise = true)
        gPortAstream    : natural := 0
    );
    port (
        --! Global reset
        iRst            : in std_logic;
        --! Global clk
        iClk            : in std_logic;
        -- Port A
        --! Port A Address
        iAddress_A      : in std_logic_vector(gAddressWidth-1 downto 2);
        --! Port A Byteenable
        iByteenable_A   : in std_logic_vector(3 downto 0);
        --! Port A Write
        iWrite_A        : in std_logic;
        --! Port A Read
        iRead_A         : in std_logic;
        --! Port A Writedata
        iWritedata_A    : in std_logic_vector(31 downto 0);
        --! Port A Readdata
        oReaddata_A     : out std_logic_vector(31 downto 0);
        --! Port A Acknowledge
        oAck_A          : out std_logic;
        -- Port B
        --! Port B Address
        iAddress_B      : in std_logic_vector(gAddressWidth-1 downto 2);
        --! Port B Byteenable
        iByteenable_B   : in std_logic_vector(3 downto 0);
        --! Port B Write
        iWrite_B        : in std_logic;
        --! Port B Read
        iRead_B         : in std_logic;
        --! Port B Writedata
        iWritedata_B    : in std_logic_vector(31 downto 0);
        --! Port B Readdata
        oReaddata_B     : out std_logic_vector(31 downto 0);
        --! Port B Acknowledge
        oAck_B          : out std_logic
    );
end tripleBuffer;

architecture rtl of tripleBuffer is
    --! Data width
    constant cDataWidth     : natural := 32;

    --! Input base address array
    constant cInputBase     : tNaturalArray(gInputBuffers downto 0) :=
        convStdLogicVectorToNaturalArray(gInputBase, gInputBuffers+1);
    --! Triple buffer base address array
    constant cTriBufOffset  : tNaturalArray(gInputBuffers*3-1 downto 0) :=
        convStdLogicVectorToNaturalArray(gTriBufOffset, gInputBuffers*3);

    --! Dpr size [byte]
    constant cDprSizeByte           : natural := gDprSize;
    --! Dpr size [words]
    constant cDprSize               : natural := cDprSizeByte/(cDataWidth/8);
    --! Dpr size log2
    constant cDprSizeLog2           : natural := LogDualis(cDprSize);
    --! Bridge output address width (byte)
    constant cBridgeOutAddrWidth    : natural := LogDualis(cDprSizeByte);

    --! Base address of consumer triple buffer switch register
    constant cBaseTriSwitch_Con     : natural := 0;
    --! Address of producer triple buffer switch register
    constant cBaseTriSwitch_Pro     : natural := cInputBase(gInputBuffers);
    --! Size of triple buffer switch register (fixed to 4 byte)
    constant cSizeTriSwitch         : natural := 4;

    --! Port A and B highest address
    constant cHigh  : natural := cBaseTriSwitch_Pro + cSizeTriSwitch;
    --! Port A and B span
    constant cSpan  : natural := 2**gAddressWidth;

    --! Type for triple buffer switch register
    subtype tTriSwitchReg is std_logic_vector(gInputBuffers-1 downto 0);

    --! Triple buffer switch record
    type tTriSwitch is record
        consumer    : tTriSwitchReg;
        producer    : tTriSwitchReg;
    end record;

    --! Consumer triple buffer switch
    signal triSwitchA   : tTriSwitch;
    --! Producer triple buffer switch
    signal triSwitchB   : tTriSwitch;

    --! Bridge port record type
    type tBridgePort is record
        rst         : std_logic;
        clk         : std_logic;
        enable      : std_logic;
        inaddr      : std_logic_vector(gAddressWidth-1 downto 0);
        tripleSel   : tTripleSelArray(gInputBuffers-1 downto 0);
        addr_unreg  : std_logic_vector(cBridgeOutAddrWidth-1 downto 0);
        addr        : std_logic_vector(cBridgeOutAddrWidth-1 downto 0);
        bufSelAny   : std_logic;
        bufSelNone  : std_logic;
    end record;

    --! Bridge port A
    signal bridgePortA : tBridgePort;
    --! Bridge port B
    signal bridgePortB : tBridgePort;

    --! Dpr port record type
    type tDprPort is record
        clk         : std_logic;
        address     : std_logic_vector(cDprSizeLog2-1 downto 0);
        byteenable  : std_logic_vector(cDataWidth/8-1 downto 0);
        write       : std_logic;
        writedata   : std_logic_vector(cDataWidth-1 downto 0);
        read        : std_logic;
        readdata    : std_logic_vector(cDataWidth-1 downto 0);
        accessEn    : std_logic;
    end record;

    --! Dpr port A
    signal dprPortA : tDprPort;
    --! Dpr port B
    signal dprPortB : tDprPort;

    --! Select signals
    type tSelect is record
        consumerReg : std_logic;
        tripleBuf   : std_logic;
        producerReg : std_logic;
        reserved    : std_logic;
    end record;

    --! Select signal inactive
    constant cSelectInactive    : tSelect := (
        cInactivated,
        cInactivated,
        cInactivated,
        cInactivated
    );

    --! Select port A
    signal selectPortA  : tSelect;
    --! Select port B
    signal selectPortB  : tSelect;

    --! DPRAM read delay [cycles] = ram delay + reg delay
    constant cDpramReadDelay    : natural := 2;

    --! DPRAM write delay [cycles] = no delay
    constant cDpramWriteDelay   : natural := 0;

    --! Typedef for port acknowledge
    type tAckPort is record
        cnt     : unsigned(logDualis(MAX(cDpramReadDelay, cDpramWriteDelay)) downto 0);
        ack     : std_logic;
    end record;

    --! Port a acknowledge
    signal ackPortA : tAckPort;
    --! Port b acknowledge
    signal ackPortB : tAckPort;
begin
    -- check generics
    assert (cDataWidth = 32)
    report "This ipcore only supports 32 bit data width!"
    severity failure;

    assert (gInputBuffers <= 32)
    report "This ipcore does not support more than 32 triple buffers!"
    severity failure;

    assert (cInputBase(0) >= cSizeTriSwitch)
    report "The first 4 byte are reserved for consumer triple buffer switch!"
    severity failure;

    assert (cSpan >= cHigh)
    report "The address bus width is set too small (high = " &
            integer'image(cHigh) & " byte / addr span = " &
            integer'image(cSpan) & " byte)!"
    severity failure;

    --Assign ports
    -- dpr port A
    dprPortA.clk        <= iClk;
    dprPortA.address    <= bridgePortA.addr(cBridgeOutAddrWidth-1 downto LogDualis(cDataWidth/8));
    dprPortA.byteenable <= iByteenable_A;
    dprPortA.write      <= iWrite_A and selectPortA.tripleBuf and dprPortA.accessEn;
    dprPortA.writedata  <= iWritedata_A;
    dprPortA.read       <= iRead_A and selectPortA.tripleBuf and dprPortA.accessEn;
    oReaddata_A         <= dprPortA.readdata when selectPortA.tripleBuf = cActivated else
                           (others => cInactivated);
    oAck_A              <= selectPortA.consumerReg or
                           ackPortA.ack or
                           selectPortA.producerReg or
                           selectPortA.reserved;

    -- bridge port A
    bridgePortA.rst     <= iRst;
    bridgePortA.clk     <= iClk;
    bridgePortA.enable  <= iWrite_A or iRead_A;
    bridgePortA.inaddr  <= iAddress_A & "00";

    -- dpr port B
    dprPortB.clk        <= iClk;
    dprPortB.address    <= bridgePortB.addr(cBridgeOutAddrWidth-1 downto LogDualis(cDataWidth/8));
    dprPortB.byteenable <= iByteenable_B;
    dprPortB.write      <= iWrite_B and selectPortB.tripleBuf and dprPortB.accessEn;
    dprPortB.writedata  <= iWritedata_B;
    dprPortB.read       <= iRead_B and selectPortB.tripleBuf and dprPortB.accessEn;
    oReaddata_B         <= dprPortB.readdata when selectPortB.tripleBuf = cActivated else
                           (others => cInactivated);
    oAck_B              <= selectPortB.consumerReg or
                           ackPortB.ack or
                           selectPortB.producerReg or
                           selectPortB.reserved;

    -- bridge port B
    bridgePortB.rst     <= iRst;
    bridgePortB.clk     <= iClk;
    bridgePortB.enable  <= iWrite_B or iRead_B;
    bridgePortB.inaddr  <= iAddress_B & "00";

    --! Assign select signals
    assignSelect : process (
        bridgePortA.bufSelAny,
        iWrite_A,
        iRead_A,
        iAddress_A,
        bridgePortB.bufSelAny,
        iWrite_B,
        iRead_B,
        iAddress_B
    )
        variable vByteAddr : std_logic_vector(gAddressWidth-1 downto 0);
    begin
        --default
        selectPortA <= cSelectInactive;
        selectPortB <= cSelectInactive;

        vByteAddr := iAddress_A & "00";
        if iWrite_A = cActivated or iRead_A = cActivated then
            case to_integer(unsigned(vByteAddr)) is
                when cBaseTriSwitch_Con =>
                    selectPortA.consumerReg <= cActivated;
                when cBaseTriSwitch_Pro =>
                    selectPortA.producerReg <= cActivated;
                when others =>
                    if bridgePortA.bufSelAny = cActivated then
                        selectPortA.tripleBuf <= cActivated;
                    else
                        selectPortA.reserved <= cActivated;
                    end if;
            end case;
        end if;

        vByteAddr := iAddress_B & "00";
        if iWrite_B = cActivated or iRead_B = cActivated then
            case to_integer(unsigned(vByteAddr)) is
                when cBaseTriSwitch_Con =>
                    selectPortB.consumerReg <= cActivated;
                when cBaseTriSwitch_Pro =>
                    selectPortB.producerReg <= cActivated;
                when others =>
                    if bridgePortB.bufSelAny = cActivated then
                        selectPortB.tripleBuf <= cActivated;
                    else
                        selectPortB.reserved <= cActivated;
                    end if;
            end case;
        end if;
    end process;

    --! Assign triple buffer switch register
    assignTriSwitch : process (
        selectPortA,
        iByteenable_A,
        iWrite_A,
        iWritedata_A,
        selectPortB,
        iByteenable_B,
        iWrite_B,
        iWritedata_B
    )
    begin
        triSwitchA <= (others => (others => cInactivated));
        triSwitchB <= (others => (others => cInactivated));

        for i in gInputBuffers-1 downto 0 loop
            if iByteenable_A(i/8) = cActivated then
                -- Special stream function to switch triple buffers with any
                -- access (read/write)!
                if gPortAstream /= 0 then
                    triSwitchA.consumer(i) <= selectPortA.consumerReg;
                    triSwitchA.producer(i) <= selectPortA.producerReg;
                elsif iWrite_A = cActivated then
                    if selectPortA.consumerReg = cActivated then
                        triSwitchA.consumer(i) <= iWritedata_A(i);
                    elsif selectPortA.producerReg = cActivated then
                        triSwitchA.producer(i) <= iWritedata_A(i);
                    end if;
                end if;
            end if;

            if iByteenable_B(i/8) = cActivated then
                if iWrite_B = cActivated then
                    if selectPortB.consumerReg = cActivated then
                        triSwitchB.consumer(i) <= iWritedata_B(i);
                    elsif selectPortB.producerReg = cActivated then
                        triSwitchB.producer(i) <= iWritedata_B(i);
                    end if;
                end if;
            end if;
        end loop;
    end process;

    --! The triple logic
    genThoseTripleLogics : for i in gInputBuffers-1 downto 0 generate
        signal proTrig  : std_logic;
        signal proSel   : tTripleSel;
        signal conTrig  : std_logic;
        signal conSel   : tTripleSel;
    begin
        --! Connect consumer and producer depending on gPortAconfig.
        --! (gPortAconfig ... 0b0 consumer / 0b1 producer)
        --! Consumer triggers with byte base address
        --! Producer triggers with byte high address
        assignProCon : process (
            conSel,
            proSel,
            triSwitchA,
            triSwitchB
        )
        begin
            if gPortAconfig(i) = cInactivated then
                --port A is consumer
                conTrig <= triSwitchA.consumer(i);
                bridgePortA.tripleSel(i) <= conSel;

                --port B is producer
                proTrig <= triSwitchB.producer(i);
                bridgePortB.tripleSel(i) <= proSel;

            elsif gPortAconfig(i) = cActivated then
                --port B is consumer
                conTrig <= triSwitchB.consumer(i);
                bridgePortB.tripleSel(i) <= conSel;

                --port A is producer
                proTrig <= triSwitchA.producer(i);
                bridgePortA.tripleSel(i) <= proSel;
            else
                assert (FALSE)
                report "gPortAconfig has wrong configuration format ('0'/'1')!"
                severity failure;
            end if;
        end process;

        theTripleLogic : entity work.tripleLogic
            port map (
                iRst        =>  iRst,
                iClk        =>  iClk,
                iPro_trig   =>  proTrig,
                oPro_sel    =>  proSel,
                iCon_trig   =>  conTrig,
                oCon_sel    =>  conSel
            );
    end generate;

    --! The Port A Bridge
    thePortA_bridge : entity work.tripleBridge
        generic map (
            gInAddrWidth            => gAddressWidth,
            gOutAddrWidth           => cBridgeOutAddrWidth,
            gInputBuffers           => gInputBuffers,
            gInputBase              => cInputBase,
            gTriBufOffset           => cTriBufOffset
        )
        port map (
            iRst                    => bridgePortA.rst,
            iClk                    => bridgePortA.clk,
            iEnable                 => bridgePortA.enable,
            iAddr                   => bridgePortA.inaddr,
            iTripleSel              => bridgePortA.tripleSel,
            oAddr_unreg             => bridgePortA.addr_unreg,
            oAddr                   => bridgePortA.addr,
            oBufferSelAny_unreg     => bridgePortA.bufSelAny,
            oBufferSelNone_unreg    => bridgePortA.bufSelNone
        );

    --! The Port B Bridge
    thePortB_bridge : entity work.tripleBridge
        generic map (
            gInAddrWidth            => gAddressWidth,
            gOutAddrWidth           => cBridgeOutAddrWidth,
            gInputBuffers           => gInputBuffers,
            gInputBase              => cInputBase,
            gTriBufOffset           => cTriBufOffset
        )
        port map (
            iRst                    => bridgePortB.rst,
            iClk                    => bridgePortB.clk,
            iEnable                 => bridgePortB.enable,
            iAddr                   => bridgePortB.inaddr,
            iTripleSel              => bridgePortB.tripleSel,
            oAddr_unreg             => bridgePortB.addr_unreg,
            oAddr                   => bridgePortB.addr,
            oBufferSelAny_unreg     => bridgePortB.bufSelAny,
            oBufferSelNone_unreg    => bridgePortB.bufSelNone
        );

    --! The DPRAM stores the triple buffer information.
    theDpr : entity work.dpRam
        generic map (
            gWordWidth      => cDataWidth,
            gNumberOfWords  => cDprSize
        )
        port map (
            iClk_A          => dprPortA.clk,
            iWriteEnable_A  => dprPortA.write,
            iAddress_A      => dprPortA.address,
            iByteenable_A   => dprPortA.byteenable,
            iWritedata_A    => dprPortA.writedata,
            oReaddata_A     => dprPortA.readdata,
            iClk_B          => dprPortB.clk,
            iWriteEnable_B  => dprPortB.write,
            iAddress_B      => dprPortB.address,
            iByteenable_B   => dprPortB.byteenable,
            iWritedata_B    => dprPortB.writedata,
            oReaddata_B     => dprPortB.readdata
        );

    --! The port A dpram access acknowledge
    ackPortA.ack <= dprPortA.write when to_integer(ackPortA.cnt) = cDpramWriteDelay else
                    dprPortA.read when to_integer(ackPortA.cnt) = cDpramReadDelay else
                    cInactivated;

    --! The port B dpram access acknowledge
    ackPortB.ack <= dprPortB.write when to_integer(ackPortB.cnt) = cDpramWriteDelay else
                    dprPortB.read when to_integer(ackPortB.cnt) = cDpramReadDelay else
                    cInactivated;

    --! Control ack counters
    controlAckCounter : process(iRst, iClk)
    begin
        if iRst = cActivated then
            ackPortA.cnt <= (others => cInactivated);
            ackPortB.cnt <= (others => cInactivated);
        elsif rising_edge(iClk) then
            if dprPortA.accessEn = cActivated then
                ackPortA.cnt <= ackPortA.cnt + 1;
            else
                ackPortA.cnt <= (others => cInactivated);
            end if;

            if dprPortB.accessEn = cActivated then
                ackPortB.cnt <= ackPortB.cnt + 1;
            else
                ackPortB.cnt <= (others => cInactivated);
            end if;
        end if;
    end process;

    --! Generate DPRAM access delay
    --! Since the registered bridge output address is used, every DPRAM access
    --! needs one cycle delay.
    genDpramAccessDelay : process(iRst, iClk)
    begin
        if iRst = cActivated then
             dprPortA.accessEn <= cInactivated;
             dprPortB.accessEn <= cInactivated;
        elsif rising_edge(iClk) then
            --default
            dprPortA.accessEn <= cInactivated;
            dprPortB.accessEn <= cInactivated;

            if selectPortA.tripleBuf = cActivated and ackPortA.ack = cInactivated then
                dprPortA.accessEn <= cActivated;
            end if;

            if selectPortB.tripleBuf = cActivated and ackPortB.ack = cInactivated then
                dprPortB.accessEn <= cActivated;
            end if;
        end if;
    end process;
end rtl;
