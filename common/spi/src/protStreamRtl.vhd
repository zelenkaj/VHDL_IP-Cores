-------------------------------------------------------------------------------
--! @file protStreamRtl.vhd
--
--! @brief Stream Protocol ipcore
--
--! @details The stream protocol ipcore implements a specific write/read access
--! controlled by e.g. SPI slave endpoint. The stream data is written to or
--! read from a specified buffer (generics) by using a generic memory mapped
--! master interface.
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

entity protStream is
    generic (
        -- Stream interface
        --! Stream interface data width
        gStreamDataWidth    : natural := 8;
        --! Skip first loads
        gStreamSkipLoads    : natural := 3;
        --! Skip first valids
        gStreamSkipValids   : natural := 4;
        -- Bus interface
        --! Bus interface data width
        gBusDataWidth       : natural := 32;
        --! Bus interface address width
        gBusAddrWidth       : natural := 8;
        --! Write buffer base
        gWrBufBase          : natural := 16#00#;
        --! Write buffer size
        gWrBufSize          : natural := 128;
        --! Read buffer base
        gRdBufBase          : natural := 16#80#;
        --! Read buffer size
        gRdBufSize          : natural := 128
    );
    port (
        --! Asynchronous reset
        iArst               : in std_logic;
        --! Clock
        iClk                : in std_logic;
        --! Synchronous protocol reset
        iSrst               : in std_logic;
        -- Stream interface
        --! Stream load
        oStreamLoad         : out std_logic;
        --! Stream load data
        oStreamLoadData     : out std_logic_vector(gStreamDataWidth-1 downto 0);
        --! Stream valid
        iStreamValid        : in std_logic;
        --! Stream valid data
        iStreamValidData    : in std_logic_vector(gStreamDataWidth-1 downto 0);
        -- Bus interface
        --! Bus address
        oBusAddress         : out std_logic_vector(gBusAddrWidth-1 downto 0);
        --! Bus write
        oBusWrite           : out std_logic;
        --! Bus write data
        oBusWritedata       : out std_logic_vector(gBusDataWidth-1 downto 0);
        --! Bus read
        oBusRead            : out std_logic;
        --! Bus read data
        iBusReaddata        : in std_logic_vector(gBusDataWidth-1 downto 0);
        --! Bus waitrequest
        iBusWaitrequest     : in std_logic
    );
end protStream;

architecture rtl of protStream is
    constant cByte : natural := 8;

    --! Register type
    type tReg is record
        addr    : std_logic_vector(gBusAddrWidth-1 downto 0);
        buf     : std_logic_vector(gBusDataWidth-1 downto 0);
        level   : natural;
        nDone   : std_logic;
    end record;

    --! address increment
    constant cRegAddr_incr      : natural := gBusDataWidth/cByte;
    --! level empty
    constant cRegLevel_empty    : natural := 0;
    --! level full
    constant cRegLevel_full     : natural := gBusDataWidth/gStreamDataWidth;

    --! Register initialize
    constant cRegInit : tReg := (
        addr    => (others => cInactivated),
        buf     => (others => cInactivated),
        level   => 0,
        nDone   => cnActivated
    );

    --! Function to increment address register by access width (cRegAddr_incr)
    function incrAddr (din : tReg) return std_logic_vector is
        variable vVal   : unsigned(din.addr'range);
        variable vIncr  : unsigned(din.addr'range);
    begin
        vVal    := unsigned(din.addr);
        vIncr   := to_unsigned(cRegAddr_incr, vIncr'length);
        vVal    := vVal + vIncr;

        return std_logic_vector(vVal);
    end function;

    --! State machine type
    type tFsm is (
        sIdle,
        sWait,
        sBusRd,
        sBusWr
    );

    --! State machin reset state
    constant cFsmRst : tFsm := sIdle;

    --! Write register set
    signal wrReg            : tReg;
    --! Write register set next
    signal wrReg_next       : tReg;
    --! Write register start address
    constant cWrStartAddr   : std_logic_vector(wrReg.addr'range) :=
        std_logic_vector(to_unsigned(gWrBufBase, wrReg.addr'length));
    --! Write high address
    constant cWrHighAddr    : std_logic_vector(wrReg.addr'range) :=
        std_logic_vector(to_unsigned(
            (gWrBufBase + gWrBufSize - cRegAddr_incr),
        wrReg.addr'length));
    --! Read register set
    signal rdReg            : tReg;
    --! Read register set next
    signal rdReg_next       : tReg;
    --! Read register start address
    constant cRdStartAddr   : std_logic_vector(rdReg.addr'range) :=
        std_logic_vector(to_unsigned(gRdBufBase, rdReg.addr'length));
    --! Read high address
    constant cRdHighAddr    : std_logic_vector(rdReg.addr'range) :=
        std_logic_vector(to_unsigned(
            (gRdBufBase + gRdBufSize - cRegAddr_incr),
        rdReg.addr'length));

    --! State machine
    signal fsm      : tFsm;
    --! State machine next
    signal fsm_next : tFsm;

    --! First load saver
    signal firstLoad        : std_logic;
    --! First load next
    signal firstLoad_next   : std_logic;

    --! Load
    signal load         : std_logic;
    --! Load next
    signal load_next    : std_logic;

    --! Skip counter value for skip loads
    constant cSkipLoads         : natural := gStreamSkipLoads;
    --! Skip counter value for skip valids
    constant cSkipValids        : natural := gStreamSkipValids;
    --! Maximum skip counter value
    constant cStreamSkipMax     : natural := MAX(cSkipLoads, cSkipValids);
    --! Skip counter
    signal skipCnt              : std_logic_vector(logDualis(cStreamSkipMax) downto 0);
    --! Skip counter next
    signal skipCnt_next         : std_logic_vector(logDualis(cStreamSkipMax) downto 0);
    --! Skip counter load done
    signal skipCnt_LoadsDone    : std_logic;
    --! Skip counter valid done
    signal skipCnt_ValidsDone   : std_logic;
    --! Skip counter done
    signal skipCnt_done         : std_logic;
    --! Skip count value loads
    constant cStreamSkipLoads   : std_logic_vector :=
            std_logic_vector(to_unsigned(cSkipLoads, skipCnt'length));
    --! Skip count value valids
    constant cStreamSkipValids  : std_logic_vector :=
            std_logic_vector(to_unsigned(cSkipValids, skipCnt'length));
begin
    -- check generic
    assert (gBusDataWidth >= gStreamDataWidth)
    report "The bus interface data width must not be smaller the stream size!"
    severity failure;

    -- assign outputs
    oBusAddress     <=  wrReg.addr when fsm = sBusWr else
                        rdReg.addr;
    oBusWrite       <=  cActivated when fsm = sBusWr else
                        cInactivated;
    oBusRead        <=  cActivated when fsm = sBusRd else
                        cInactivated;
    oBusWritedata   <=  wrReg.buf;
    oStreamLoad     <=  load;

    --! The register process assigns the next signals.
    regProc : process(iArst, iClk)
    begin
        if iArst = cActivated then
            wrReg       <= cRegInit;
            rdReg       <= cRegInit;
            fsm         <= cFsmRst;
            firstLoad   <= cInactivated;
            load        <= cInactivated;
            skipCnt     <= (others => cInactivated);
        elsif rising_edge(iClk) then
            wrReg       <= wrReg_next;
            rdReg       <= rdReg_next;
            fsm         <= fsm_next;
            firstLoad   <= firstLoad_next;
            load        <= load_next;
            skipCnt     <= skipCnt_next;
        end if;
    end process;

    skipCnt_LoadsDone   <=  cActivated when skipCnt >= cStreamSkipLoads else
                            cInactivated;

    skipCnt_ValidsDone  <=  cActivated when skipCnt > cStreamSkipValids else
                            cInactivated;

    skipCnt_done <= skipCnt_LoadsDone and skipCnt_ValidsDone;

    --! The combinational process assigns the next signals.
    comb : process (
        wrReg,
        rdReg,
        fsm,
        iSrst,
        firstLoad,
        load,
        skipCnt,
        skipCnt_done,
        skipCnt_LoadsDone,
        skipCnt_ValidsDone,
        iStreamValid,
        iStreamValidData,
        iBusWaitrequest,
        iBusReaddata
    )
        variable vTmp : natural;
    begin
        -- default
        wrReg_next      <= wrReg;
        rdReg_next      <= rdReg;
        fsm_next        <= fsm;
        firstLoad_next  <= firstLoad;
        load_next       <= cInactivated;
        oStreamLoadData <= (others => cInactivated);
        skipCnt_next    <= skipCnt;

        -- protocol synchronous reset
        if iSrst = cActivated then
            -- register initials
            wrReg_next  <= cRegInit;
            rdReg_next  <= cRegInit;
            fsm_next    <= sIdle;

            -- prepare for first load
            firstLoad_next      <= cActivated;
            wrReg_next.addr     <= cWrStartAddr;
            wrReg_next.nDone    <= cnInactivated;
            rdReg_next.addr     <= cRdStartAddr;
            rdReg_next.nDone    <= cnInactivated;

            -- initialize skip counter to zero
            skipCnt_next    <= (others => cInactivated);
        else
            -- handle skip counter after loading
            if skipCnt_done = cInactivated and load = cActivated then
                skipCnt_next <= std_logic_vector(unsigned(skipCnt) + 1);
            end if;

            -- load data after valid data
            if rdReg.nDone = cnInactivated then
                load_next <= iStreamValid;
            end if;

            -- capture read data
            if fsm = sBusRd and iBusWaitrequest = cInactivated then
                rdReg_next.buf <= iBusReaddata;
            end if;

            -- incr/decr levels
            if iStreamValid = cActivated and wrReg.nDone = cnInactivated then
                if skipCnt_ValidsDone = cActivated then
                    wrReg_next.level <= wrReg.level + 1;
                end if;
            end if;
            if load = cActivated and rdReg.nDone = cnInactivated then
                if skipCnt_LoadsDone = cActivated then
                    rdReg_next.level <= rdReg.level - 1;
                end if;
            end if;

            -- Assign valid stream data to write buffer.
            -- The offset is determined by the fill level!
            if iStreamValid = cActivated then
                vTmp := wrReg.level;
                wrReg_next.buf((vTmp+1)*gStreamDataWidth-1 downto vTmp*gStreamDataWidth) <= iStreamValidData;
            end if;

            -- Assign read buffer to load stream data.
            -- The offset is determined by the fill level!
            if load = cActivated then
                vTmp := cRegLevel_full - rdReg.level;
                oStreamLoadData <= rdReg.buf((vTmp+1)*gStreamDataWidth-1 downto vTmp*gStreamDataWidth);
            end if;

            -- state machine processing
            case fsm is
                when sIdle =>
                    if firstLoad = cActivated then
                        fsm_next <= sBusRd;
                    end if;

                when sWait =>
                    -- do first load and reset first load saver
                    if firstLoad = cActivated then
                        load_next <= cActivated;
                    end if;
                    firstLoad_next <= cInactivated;

                    -- check levels
                    if (rdReg.level = cRegLevel_empty and
                        rdReg.nDone = cnInactivated) then
                        fsm_next <= sBusRd;
                    elsif (wrReg.level = cRegLevel_full and
                        wrReg.nDone = cnInactivated) then
                        fsm_next <= sBusWr;
                    elsif (wrReg.nDone = cnActivated and
                        rdReg.nDone = cnActivated) then
                        fsm_next <= sIdle;
                    end if;

                when sBusRd =>
                    -- after read change to write
                    -- buffer is full now
                    if iBusWaitrequest = cInactivated then
                        fsm_next <= sWait;

                        -- increment address and level
                        if rdReg.addr >= cRdHighAddr then
                            rdReg_next.nDone <= cnActivated;
                        else
                            rdReg_next.addr <= incrAddr(rdReg);
                        end if;
                        rdReg_next.level <= cRegLevel_full;
                    end if; --waitrequest

                when sBusWr =>
                    -- exit state if bus is ready
                    -- buffer is empty now
                    if iBusWaitrequest = cInactivated then
                        fsm_next <= sWait;

                        -- increment address and level
                        if wrReg.addr >= cWrHighAddr then
                            wrReg_next.nDone <= cnActivated;
                        else
                            wrReg_next.addr <= incrAddr(wrReg);
                        end if;
                        wrReg_next.level <= cRegLevel_empty;
                    end if;
            end case;
        end if; --iSrs
    end process;
end rtl;
