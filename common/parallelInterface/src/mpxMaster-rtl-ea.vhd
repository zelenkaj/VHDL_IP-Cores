-------------------------------------------------------------------------------
--! @file mpxMaster-rtl-ea.vhd
--! @brief Multiplexed memory mapped master
-------------------------------------------------------------------------------
--
--    (c) B&R, 2014
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

entity mpxMaster is
    generic (
        --! Data bus width
        gDataWidth  : natural := 16;
        --! Address bus width
        gAddrWidth  : natural := 16;
        --! Address low
        gAddrLow    : natural := 0;
        --! Ad bus width
        gAdWidth    : natural := 16
    );
    port (
        --! Clock
        iClk                : in    std_logic;
        --! Reset
        iRst                : in    std_logic;
        -- Memory mapped slave
        --! Address
        iSlv_address        : in    std_logic_vector(gAddrWidth-1 downto gAddrLow);
        --! Read strobe
        iSlv_read           : in    std_logic;
        --! Readdata
        oSlv_readdata       : out   std_logic_vector(gDataWidth-1 downto 0);
        --! Write strobe
        iSlv_write          : in    std_logic;
        --! Writedata
        iSlv_writedata      : in    std_logic_vector(gDataWidth-1 downto 0);
        --! Waitrequest
        oSlv_waitrequest    : out   std_logic;
        --! Byteenable
        iSlv_byteenable     : in    std_logic_vector(gDataWidth/8-1 downto 0);
        -- Memory mapped multiplexed master
        --! Chipselect
        oMpxMst_cs          : out   std_logic;
        --! Multiplexed address data bus input
        iMpxMst_ad_i       : in    std_logic_vector(gAdWidth-1 downto 0);
        --! Multiplexed address data bus output
        oMpxMst_ad_o       : out   std_logic_vector(gAdWidth-1 downto 0);
        --! Multiplexed address data bus enable
        oMpxMst_ad_oen     : out   std_logic;
        --! Byteenable
        oMpxMst_be          : out   std_logic_vector(gDataWidth/8-1 downto 0);
        --! Address latch enable
        oMpxMst_ale         : out   std_logic;
        --! Write strobe
        oMpxMst_wr          : out   std_logic;
        --! Read strobe
        oMpxMst_rd          : out   std_logic;
        --! Acknowledge
        iMpxMst_ack         : in    std_logic
    );
end entity mpxMaster;

architecture rtl of mpxMaster is
    -- Counter to wait in states
    signal count        : std_logic_vector(2 downto 0);
    signal count_rst    : std_logic;

    constant cCount_AleDisable  : std_logic_vector := "011";
    constant cCount_AleExit     : std_logic_vector := "101";
    constant cCount_max         : std_logic_vector := "111";

    -- State machine for bus timing
    type tFsm is (
        sIdle,
        sAle,
        sWrd,
        sWait
    );
    signal fsm : tFsm;

    signal ack          : std_logic;
    signal ack_d        : std_logic;
    signal ack_l        : std_logic;
    signal readdata     : std_logic_vector(oSlv_readdata'range);
    signal readdata_l   : std_logic_vector(oSlv_readdata'range);
    signal adReg        : std_logic_vector(oMpxMst_ad_o'range);
begin

    process(iClk, iRst)
    begin
        if iRst = cActivated then
            count           <= (others => cInactivated);
            count_rst       <= cInactivated;
            oMpxMst_cs      <= cInactivated;
            oMpxMst_ale     <= cInactivated;
            oMpxMst_ad_oen  <= cInactivated;
            oMpxMst_rd      <= cInactivated;
            oMpxMst_wr      <= cInactivated;
            ack             <= cInactivated;
            ack_l           <= cInactivated;
            readdata        <= (others => cInactivated);
            readdata_l      <= (others => cInactivated);
            oMpxMst_be      <= (others => cInactivated);
            adReg           <= (others => cInactivated);
        elsif rising_edge(iClk) then
            --default
            count_rst <= cInactivated;

            ack_l       <= iMpxMst_ack;
            ack         <= ack_l;
            ack_d       <= ack;
            readdata_l  <= iMpxMst_ad_i;
            readdata    <= readdata_l;

            if count_rst = cActivated then
                count <= (others => cInactivated);
            else
                count <= std_logic_vector(unsigned(count) + 1);
            end if;

            oMpxMst_be <= iSlv_byteenable;

            case fsm is
                when sIdle =>
                    count_rst       <= cActivated;
                    oMpxMst_cs      <= cInactivated;
                    oMpxMst_ale     <= cInactivated;
                    oMpxMst_ad_oen  <= cInactivated;
                    oMpxMst_rd      <= cInactivated;
                    oMpxMst_wr      <= cInactivated;

                    if iSlv_read = cActivated or iSlv_write = cActivated then
                        fsm                         <= sAle;
                        oMpxMst_cs                  <= cActivated;
                        oMpxMst_ale                 <= cActivated;
                        oMpxMst_ad_oen              <= cActivated;
                        adReg                       <= (others => cInactivated);
                        adReg(iSlv_address'range)   <= iSlv_address;
                    end if;
                when sAle =>
                    if count = cCount_AleDisable then
                        oMpxMst_ale                 <= cInactivated;
                    elsif count = cCount_AleExit then
                        count_rst                   <= cActivated;
                        fsm                         <= sWrd;
                        oMpxMst_wr                  <= iSlv_write;
                        oMpxMst_rd                  <= iSlv_read;
                        oMpxMst_ad_oen              <= iSlv_write;
                        adReg                       <= (others => cInactivated);
                        adReg(iSlv_writedata'range) <= iSlv_writedata;
                    end if;
                when sWrd =>
                    if ack = cActivated then
                        count_rst       <= cActivated;
                        fsm             <= sWait;
                        oMpxMst_cs      <= cInactivated;
                        oMpxMst_rd      <= cInactivated;
                        oMpxMst_wr      <= cInactivated;
                        oMpxMst_ad_oen  <= cInactivated;
                    end if;
                when sWait =>
                    if ack = cInactivated or count = cCount_max then
                        count_rst   <= cActivated;
                        fsm         <= sIdle;
                    end if;
            end case;
        end if;
    end process;

    oMpxMst_ad_o    <= adReg;

    -- if ack goes high deassert waitrequest (edge detection)
    oSlv_waitrequest    <= cInactivated when ack_d = cInactivated and ack = cActivated else cActivated;
    oSlv_readdata       <= readdata;
end architecture rtl;
