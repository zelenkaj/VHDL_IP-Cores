-------------------------------------------------------------------------------
--! @file tbProtStreamBhv.vhd
--
--! @brief Testbench for Stream Protocol ipcore
--
--! @details Testbench that verifies the stream protocol ipcore
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

entity tbProtStream is
    generic (
        gStreamDataWidth : natural := 8;
        gStreamSkipNum      : natural := 4;
        gBusDataWidth : natural := 8;
        gBusAddrWidth : natural := 8;
        gWrBufBase : natural := 16#00#;
        gWrBufSize : natural := 128;
        gRdBufBase : natural := 16#80#;
        gRdBufSize : natural := 128
    );
end tbProtStream;

architecture bhv of tbProtStream is
    constant cWrBufBase : natural := gWrBufBase;
    constant cWrBufSize : natural := gWrBufSize;
    constant cRdBufBase : natural := gRdBufBase;
    constant cRdBufSize : natural := gRdBufSize;

    -- timing for 12.5 MHz SPI
    constant cMainClkPeriod : time := 10 ns;
    constant cWaitPeriod : time := 64 * cMainClkPeriod;

    constant cByte : natural := 8;

    signal rst : std_logic;
    signal clk : std_logic;
    signal done : std_logic;

    -- DUT signals
    signal srst : std_logic;
    signal load : std_logic;
    signal loadData : std_logic_vector(gStreamDataWidth-1 downto 0);
    signal valid : std_logic;
    signal validData : std_logic_vector(gStreamDataWidth-1 downto 0);
    signal address : std_logic_vector(gBusAddrWidth-1 downto 0);
    signal write : std_logic;
    signal writedata : std_logic_vector(gBusDataWidth-1 downto 0);
    signal read : std_logic;
    signal readdata : std_logic_vector(gBusDataWidth-1 downto 0);
    signal waitrequest : std_logic;

    -- Bus signals
    signal ack : std_logic;
    signal busWrite_cnt : natural;
    signal busRead_cnt : natural;
    signal streamLoad_cnt : natural;
begin
    theRstGen : entity work.resetGen
        port map (
            oReset => rst
        );

    theClkGen : entity work.clkGen
        generic map (
            gPeriod => cMainClkPeriod
        )
        port map (
            iDone => done,
            oClk => clk
        );

    stimProc : process
    begin
        done <= cInactivated;
        srst <= cInactivated;
        valid <= cInactivated;
        validData <= (others => cInactivated);
        wait until rst = cInactivated;
        wait until rising_edge(clk);

        -- all good things go by three...
        for k in 1 to 3 loop
            -- reset protocol
            srst <= cActivated;
            wait until rising_edge(clk);
            srst <= cInactivated;
            wait until rising_edge(clk);

            if gStreamSkipNum > 0 then
                for i in 0 to gStreamSkipNum-1 loop
                    wait for cWaitPeriod;
                    wait until rising_edge(clk);
                    valid <= cActivated;
                    validData <= (others => cActivated);
                    wait until rising_edge(clk);
                    valid <= cInactivated;
                end loop;
            end if;

            for i in 0 to 2*MAX(cWrBufSize, cRdBufSize)-1 loop
                wait for cWaitPeriod;
                wait until rising_edge(clk);
                valid <= cActivated;
                validData <= std_logic_vector(to_unsigned(i, gStreamDataWidth));
                wait until rising_edge(clk);
                valid <= cInactivated;
            end loop;

            -- do nothing for some time
            wait for cWaitPeriod;
            wait until rising_edge(clk);
        end loop;

        done <= cActivated;
        wait;
    end process;

    DUT : entity work.protStream
        generic map (
            gStreamDataWidth => gStreamDataWidth,
            gStreamSkipNum      => gStreamSkipNum,
            gBusDataWidth => gBusDataWidth,
            gBusAddrWidth => gBusAddrWidth,
            gWrBufBase => cWrBufBase,
            gWrBufSize => cWrBufSize,
            gRdBufBase => cRdBufBase,
            gRdBufSize => cRdBufSize
        )
        port map (
            iArst => rst,
            iClk => clk,
            iSrst => srst,
            oStreamLoad => load,
            oStreamLoadData => loadData,
            iStreamValid => valid,
            iStreamValidData => validData,
            oBusAddress => address,
            oBusWrite => write,
            oBusWritedata => writedata,
            oBusRead => read,
            iBusReaddata => readdata,
            iBusWaitrequest => waitrequest
        );

    waitrequest <=  cInactivated when write = cActivated else
                    not ack when read = cActivated else
                    cActivated;

    ctrlBus : process(rst, clk)
    begin
        if rst = cActivated then
            ack <= cInactivated;
            readdata <= (others => cInactivated);
        elsif rising_edge(clk) then
            ack <= cInactivated;
            if read = cActivated and ack = cInactivated then
                ack <= cActivated;
            end if;
            if ack = cActivated then
                readdata <= std_logic_vector(unsigned(readdata) + 1);
            end if;
        end if;
    end process;

    issueCnt : process(rst, clk)
        constant cInitStreamLoad_cnt : natural := gBusDataWidth/gStreamDataWidth-1;
    begin
        if rst = cActivated then
            busWrite_cnt <= 0;
            busRead_cnt <= 0;
            streamLoad_cnt <= 0;
        elsif rising_edge(clk) then
            if srst = cActivated then
                busWrite_cnt <= 0;
                busRead_cnt <= 0;
                streamLoad_cnt <= cInitStreamLoad_cnt;
            end if;

            if write = cActivated and waitrequest = cInactivated then
                busWrite_cnt <= busWrite_cnt + 1;
            elsif read = cActivated and waitrequest = cInactivated then
                busRead_cnt <= busRead_cnt + 1;
            end if;

            if load = cActivated then
                if streamLoad_cnt > 0 then
                    streamLoad_cnt <= streamLoad_cnt - 1;
                else
                    streamLoad_cnt <= cInitStreamLoad_cnt;
                end if;
            end if;
        end if;
    end process;

    checkBus : process(rst, clk)
        variable vPattern       : natural;
        variable vPattern_ref   : natural;
        variable vAddr          : natural;
        variable vAddr_ref      : natural;
    begin
        if rst = cActivated then
        elsif rising_edge(clk) then
            -- check bus write
            if write = cActivated and waitrequest = cInactivated then
                for i in gBusDataWidth/gStreamDataWidth-1 downto 0 loop
                    vPattern := to_integer(unsigned(writedata((i+1)*gStreamDataWidth-1 downto i*gStreamDataWidth)));
                    vPattern_ref := i + gBusDataWidth/gStreamDataWidth*busWrite_cnt;
                    assert(vPattern = vPattern_ref)
                    report "Wrong pattern is written to bus! (" &
                            " shall = " & integer'image(vPattern_ref) &
                            " | " &
                            " is = " & integer'image(vPattern) &
                            ")"
                    severity failure;
                end loop;
            end if;

            -- check address
            if waitrequest = cInactivated then
                vAddr := to_integer(unsigned(address));
                if write = cActivated then
                    vAddr_ref := busWrite_cnt * gBusDataWidth/cByte + cWrBufBase;
                    assert (vAddr = vAddr_ref)
                    report "Bus write address is wrong!(" &
                            " shall = " & integer'image(vAddr_ref) &
                            " | " &
                            " is = " & integer'image(vAddr) &
                            ")"
                    severity failure;
                elsif read = cActivated then
                    vAddr_ref := busRead_cnt * gBusDataWidth/cByte + cRdBufBase;
                    assert (vAddr = vAddr_ref)
                    report "Bus read address is wrong!(" &
                            " shall = " & integer'image(vAddr_ref) &
                            " | " &
                            " is = " & integer'image(vAddr) &
                            ")"
                    severity failure;
                end if;
            end if;
        end if;
    end process;

    checkStream : process(rst, clk)
        variable vLoadData : natural;
        variable vBusRead : natural;
        variable vReaddata : std_logic_vector(readdata'range);
        variable vSkipCnt   : natural;
    begin
        if rst = cActivated then
        elsif rising_edge(clk) then
            if srst = cActivated then
                vSkipCnt := 0;
            -- check load
            elsif load = cActivated then
                if vSkipCnt < gStreamSkipNum then
                    vSkipCnt := vSkipCnt + 1;
                else
                    vLoadData := to_integer(unsigned(loadData));
                    vReaddata := std_logic_vector(unsigned(readdata) - 1);
                    for i in gBusDataWidth/gStreamDataWidth-1 downto 0 loop
                        if i = streamLoad_cnt then
                            vBusRead := to_integer(unsigned(vReaddata((i+1)*gStreamDataWidth-1 downto i*gStreamDataWidth)));
                            assert (vLoadData = vBusRead)
                            report "Wrong data is loaded to stream! (" &
                                " shall = " & integer'image(vBusRead) &
                                " | " &
                                " is = " & integer'image(vLoadData) &
                                ")"
                            severity failure;
                        end if;
                    end loop;
                end if;
            end if;
        end if;
    end process;
end bhv;
