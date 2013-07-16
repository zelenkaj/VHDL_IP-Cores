-------------------------------------------------------------------------------
--! @file tbMagicBridgeBhv.vhd
--
--! @brief testbench for MagicBridgeBhv and is stimulated with the busMasterBhv
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
-- Design unit header --
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.global.all;
use work.hostInterfacePkg.all;

entity tbMagicBridge is
end tbMagicBridge;

architecture Bhv of tbMagicBridge is
    --! defines wheater a magicBridge uses registers or a DPRAM(M9K-block)
    constant cUseMemBlock                  : integer   := 1;
    --! simulated latency of control signal for the slave
    constant cRegisterLatency           : natural := 2;
    constant cWordWidth                 : natural := 32;
    --! Master address width (busMaster)
    constant cMS_AddressWidth           : natural := 16;
    --! Slave address width (registerFile), must be greater the master's width in this case
    constant cSL_AddressWidth           : natural := 17;
    --! number of address spaces recognized by the magicBridge
    constant cAddressSpaceCount         : natural := 3;
    constant cAddressSpaceCount_log2    : natural := LogDualis(cAddressSpaceCount);
    --! base(e.g.: element 1) and top=base+1(e.g.: element 2) addresses of the address space
    constant cLutBaseAddressArray              : tArrayStd32(cAddressSpaceCount downto 0) :=
        (0=>x"0000_1000", 1=> x"0000_2000", 2=> x"0000_3000", 3=> x"0000_3100");
    --! dynamic offset for the according static address.
    constant cRegBaseAddressArray       : tArrayStd32(cAddressSpaceCount-1 downto 0) :=
        (0=>x"0000_A00F", 1=> x"0000_B00F", 2=> x"0000_C00F");
    --! periode of the clock signal
    constant cPeriode                       : time := 20 ns;
    --! general signals:
    signal Clk ,Rst                         : std_logic := cActivated;
    --! signals for master busMaster(MS):
    signal MS_Enable, MS_Ack                : std_logic := cInactivated;
    signal MS_Write, MS_Read                : std_logic := cInactivated;
    signal MS_readData, MS_writeData        : std_logic_vector(cWordWidth-1 downto 0) := (others => '0');
    signal MS_Address                       : std_logic_vector(cMS_AddressWidth-1 downto 0) := (others => cInactivated);
    signal MS_Byteenable                    : std_logic_vector(cWordWidth/8-1 downto 0) := (others => cInactivated);
    signal MS_Done, MS_Error, MS_Select     : std_logic;
    --! signals for slave register(SL) file:
    signal SL_ReaddataA, SL_ReaddataB       : std_logic_vector(cWordWidth-1 downto 0) := (others => cInactivated);
    signal SL_WritedataA, SL_WritedataB     : std_logic_vector(cWordWidth-1 downto 0) := (others => cInactivated);
    signal SL_ByteenableA, SL_ByteenableB   : std_logic_vector(cWordWidth/8-1 downto 0) := (others => cInactivated);
    signal SL_WriteA, SL_WriteB             : std_logic := cInactivated;
    signal SL_AddressA, SL_AddressB         : std_logic_vector(cSL_AddressWidth-1 downto 0) := (others => cInactivated);
    signal SL_WRQ, MS_WRQ                   : std_logic := cInactivated;
    --! signals for magic bridge(MB) configuration:
    signal MB_Write                         : std_logic := cInactivated;
    signal MB_Byteenable                    : std_logic_vector(cWordWidth/8-1 downto 0) := (others => cInactivated);
    signal MB_Address                       : std_logic_vector(cAddressSpaceCount_log2-1 downto 0) := (others => cInactivated);
    signal MB_Writedata, MB_Readdata        : std_logic_vector(cWordWidth-1 downto 0) := (others => cInactivated);
    signal InitMBfinished                   : std_logic := cInactivated;
    --! note: some signal assignments are superfluous, but they increase the readability!
begin

    ---- User Signal Assignments ----
    Clk             <= not Clk after cPeriode/2 when MS_Done /= cActivated else cInactivated after cPeriode;
    Rst             <= cInactivated after cPeriode;
    --! register file assignments:
    SL_WritedataB   <= (others => cInactivated);
    SL_AddressB     <= (others => cInactivated);
    SL_ByteenableB  <= (others => cInactivated);
    SL_WriteB       <= cInactivated;
    SL_ByteenableA  <= MS_Byteenable;
    SL_WritedataA       <= MS_writeData;
    --! busMaster assignments:
    MS_readData     <= SL_ReaddataA;
    MS_WRQ          <= SL_WRQ;
    MS_Ack          <= not MS_WRQ;

    --! assert the Done flag from the busMaster
    ASSERT_MS: process
    begin
        wait until Clk'event and Clk = cActivated;
        if MS_Done = cActivated then
            assert MS_Error = cActivated report "Simulation of tbMagicBridgeBhv FAILED" severity failure;
            wait;
        end if;
    end process ASSERT_MS;

    --! first after the reset init the magicBridge:
    INIT_MB: process
    begin
        assert cRegBaseAddressArray'length = cLutBaseAddressArray'length-1 report
            "INIT_MB: array length wrong" severity warning;

        wait until Rst = cInactivated;
        MB_Write        <= cActivated;
        MB_Byteenable   <= (others => cActivated);
        for i in cRegBaseAddressArray'range loop
            MB_Writedata    <= cRegBaseAddressArray(i);
            MB_Address      <= std_logic_vector(to_unsigned(i, MB_Address'length));
            wait until Clk'event and Clk = cActivated;
        end loop;
        MB_Write        <= cInactivated;
        MB_Byteenable   <= (others => cInactivated);
        wait until Clk'event and Clk = cActivated;
        InitMBfinished  <= cActivated;
        wait;
    end process INIT_MB;

    --! after initialization check if the magicBridge is translating the addresses correctly
    ASSERT_MB: process
        variable NrErrors : natural := 0;
        variable oldAddress, cmpAddress : std_logic_vector(cSL_AddressWidth-1 downto 0);
        variable tempAddress : std_logic_vector(31 downto 0);
        variable index      : natural := 0;
        variable offset     : std_logic_vector(cSL_AddressWidth-1 downto 0);
    begin
        wait until InitMBfinished = cActivated;
        -- enalbe busMaster for stimulating the MagicBridge.
        MS_Enable <= cActivated;
        -- check if address calculation is correct...
        loop
            oldAddress := (others => cInactivated);
            cmpAddress := (others => cInactivated);
            offset      := (others => cInactivated);
            index       := 0;
            wait until Clk'event;
            if(MS_Write = cActivated or MS_Read = cActivated) then
                oldAddress(MS_Address'range) := MS_Address;

                -- find index of offset:
                for i in cLutBaseAddressArray'length-1 downto 0 loop
                    if oldAddress > cLutBaseAddressArray(i)(oldAddress'range)
                        and oldAddress < cLutBaseAddressArray(i+1)(oldAddress'range) then
                        index := i;
                        exit;
                    end if;
                end loop;
                -- get offset:
                tempAddress := (others => cInactivated);
                tempAddress := std_logic_vector(
                        unsigned(cRegBaseAddressArray(index)) - unsigned(cLutBaseAddressArray(index)));
                offset := tempAddress(offset'range);
                tempAddress := (others => cInactivated);
                tempAddress(cmpAddress'range) := std_logic_vector(
                        unsigned(oldAddress) + unsigned(offset) );
                cmpAddress := tempAddress(cmpAddress'range);
                -- wait on slave's acknowledge:
                if MS_Done /= cActivated then
                    wait until MS_Ack'event and MS_Ack = cActivated;
                    if SL_AddressA /= cmpAddress then
                        NrErrors := NrErrors + 1;
                    end if;
                end if;
            end if;
            -- wait until MS finished
            exit when MS_Done = cActivated;
        end loop;
        assert NrErrors = 0 report
                " tbMagicBridgeBhv exited with: "
                & integer'image(NrErrors) &
                " errors!" severity failure;
        assert NrErrors /= 0 report "tbMagicBrigeBhv finished: SUCCESSFULLY" severity note;
        wait;
    end process ASSERT_MB;

    --! simulate the wait request of the slave (registerFile)
    STIMULATE_SL_WRQ: process
    begin
        SL_WRQ <= cActivated;
        wait until Clk'event and Clk = cActivated;
        if MS_Enable = cActivated and (SL_WriteA = cActivated or MS_Read = cActivated) then
            for i in 0 to 0 loop
                wait until Clk'event and Clk = cActivated;
            end loop;
            SL_WRQ <= cInactivated;
            wait until Clk'event and Clk = cActivated;
        end if;
        if MS_Done = cActivated then
            wait;
        end if;
    end process STIMULATE_SL_WRQ;

    --! delay the control signals for the slave, as the addressses are delayed by the magicBridge
    STIMULATE_SL_WRITE: process
    begin
        SL_WriteA <= cInactivated;
        wait until Clk'event and Clk = cActivated;
        if MS_Enable = cActivated and MS_Write = cActivated then
            for i in 0 to 2 loop
                wait until Clk'event and Clk = cActivated;
            end loop;

            SL_WriteA <= cActivated and not MS_Read;
            wait until Clk'event and Clk = cActivated;
        end if;

        if MS_Done = cActivated then
            wait;
        end if;
    end process STIMULATE_SL_WRITE;

    ----  Component instantiations  ----
    SLAVE: entity work.registerFile(Rtl)
        generic map(
            gRegCount           => 2**cSL_AddressWidth
        )
        port map(
            iClk                =>  Clk,
            iRst                =>  Rst,
            iWriteA             =>  SL_WriteA,
            iWriteB             =>  SL_WriteB,
            iByteenableA        =>  SL_ByteenableA,
            iByteenableB        =>  SL_ByteenableB,
            iAddrA              =>  SL_AddressA,
            iAddrB              =>  SL_AddressB,
            iWritedataA         =>  SL_WritedataA,
            oReaddataA          =>  SL_ReaddataA,
            iWritedataB         =>  SL_WritedataB,
            oReaddataB          =>  SL_ReaddataB
        );

    DUT : entity work.magicBridge
        generic map (
           gAddressSpaceCount   => cAddressSpaceCount,
           gBaseAddressArray    => cLutBaseAddressArray,
           gUseMemBlock            =>  cUseMemBlock
        )
        port map(
           iClk                 => Clk,
           iRst                 => Rst,
           iBridgeAddress       => MS_Address,
           iBridgeSelect        => MS_Select,
           oBridgeAddress       => SL_AddressA,
           oBridgeSelectAny     => open,
           oBridgeSelect        => open,
           iBaseSetWrite        => MB_Write,
           iBaseSetByteenable   => MB_Byteenable,
           iBaseSetAddress      => MB_Address,
           iBaseSetData         => MB_Writedata,
           oBaseSetData         => MB_Readdata
        );

    MASTER : entity work.busMaster
        generic map (
           gAddrWidth           => cMS_AddressWidth,
           gDataWidth           => cWordWidth,
           gStimuliFile         => "../../hostinterface/tb/tbMagicBridge_stim.txt"
        )
        port map(
           iRst                 =>  Rst,
           iClk                 =>  Clk,
           iEnable              =>  MS_Enable,
           iAck                 =>  MS_Ack,
           iReaddata            =>  MS_readData,
           oWrite               =>  MS_Write,
           oRead                =>  MS_Read,
           oSelect              =>  MS_Select,
           oAddress             =>  MS_Address,
           oByteenable          =>  MS_Byteenable,
           oWritedata           =>  MS_writeData,
           oError               =>  MS_Error,
           oDone                =>  MS_Done
        );

end architecture Bhv;
