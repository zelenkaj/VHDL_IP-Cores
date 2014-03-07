-------------------------------------------------------------------------------
--! @file tbMpxMaster-bhv-tb.vhd
--
--! @brief Testbench for Multiplex parallel master ipcore
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

--! Utility library
library libutil;

--! Use libcommon library
library libcommon;
--! Use global package
use libcommon.global.all;

entity tbMpxMaster is
    generic (
        gStim : string := "text.txt"
    );
end tbMpxMaster;

architecture bhv of tbMpxMaster is
    signal clk  : std_logic;
    signal rst  : std_logic;
    signal done : std_logic;

    constant cAddrWidth : natural := 6;
    constant cDataWidth : natural := 32;
    constant cAdWidth   : natural := maximum(cAddrWidth, cDataWidth);

    -- Multiplex master
    type tMpxMaster is record
        slv_address     : std_logic_vector(cAddrWidth-1 downto 0);
        slv_read        : std_logic;
        slv_readdata    : std_logic_vector(cDataWidth-1 downto 0);
        slv_write       : std_logic;
        slv_writedata   : std_logic_vector(cDataWidth-1 downto 0);
        slv_waitrequest : std_logic;
        slv_byteenable  : std_logic_vector(cDataWidth/8-1 downto 0);
        mpxMst_cs       : std_logic;
        mpxMst_ad_i     : std_logic_vector(cAdWidth-1 downto 0);
        mpxMst_ad_o     : std_logic_vector(cAdWidth-1 downto 0);
        mpxMst_ad_oen   : std_logic;
        mpxMst_be       : std_logic_vector(cDataWidth/8-1 downto 0);
        mpxMst_ale      : std_logic;
        mpxMst_wr       : std_logic;
        mpxMst_rd       : std_logic;
        mpxMst_ack      : std_logic;
    end record;

    signal inst_mpxMaster : tMpxMaster;

    -- multiplexed slave
    type tMpxSlave is record
        mpxSlv_cs       : std_logic;
        mpxSlv_rd       : std_logic;
        mpxSlv_wr       : std_logic;
        mpxSlv_ale      : std_logic;
        mpxSlv_ack      : std_logic;
        mpxSlv_be       : std_logic_vector(cDataWidth/8-1 downto 0);
        mpxSlv_ad_i     : std_logic_vector(cAdWidth-1 downto 0);
        mpxSlv_ad_o     : std_logic_vector(cAdWidth-1 downto 0);
        mpxSlv_ad_oen   : std_logic;
        mst_chipselect  : std_logic;
        mst_read        : std_logic;
        mst_write       : std_logic;
        mst_byteenable  : std_logic_vector(cDataWidth/8-1 downto 0);
        mst_address     : std_logic_vector(cAddrWidth-1 downto 0);
        mst_writedata   : std_logic_vector(cDataWidth-1 downto 0);
        mst_readdata    : std_logic_vector(cDataWidth-1 downto 0);
        mst_waitrequest : std_logic;
    end record;

    signal inst_mpxSlave : tMpxSlave;

    -- Single port ram
    type tSpram is record
        write       : std_logic;
        read        : std_logic;
        address     : std_logic_vector(cAddrWidth-1 downto 0);
        byteenable  : std_logic_vector(cDataWidth/8-1 downto 0);
        writedata   : std_logic_vector(cDataWidth-1 downto 0);
        readdata    : std_logic_vector(cDataWidth-1 downto 0);
        ready       : std_logic;
    end record;

    signal inst_spram : tSpram;

    -- Stim bus master
    type tBusMaster is record
        ack        : std_logic;
        enable     : std_logic;
        readdata   : std_logic_vector(cDataWidth-1 downto 0);
        address    : std_logic_vector(cAddrWidth-1 downto 0);
        byteenable : std_logic_vector(cDataWidth/8-1 downto 0);
        done       : std_logic;
        error      : std_logic;
        read       : std_logic;
        write      : std_logic;
        writedata  : std_logic_vector(cDataWidth-1 downto 0);
    end record;

    signal inst_busMaster : tBusMaster;
begin
    done                    <= inst_busMaster.done;
    inst_busMaster.enable   <= cActivated;

    assert (inst_busMaster.error /= cActivated)
    report "Bus master reports error!" severity failure;

    ---------------------------------------------------------------------------
    -- Map components

    -- inst_busMaster --- inst_mpxMaster
    inst_mpxMaster.slv_read     <= inst_busMaster.read;
    inst_mpxMaster.slv_write    <= inst_busMaster.write;
    inst_busMaster.ack          <= not inst_mpxMaster.slv_waitrequest;

    inst_mpxMaster.slv_address      <= inst_busMaster.address(inst_mpxMaster.slv_address'range);
    inst_mpxMaster.slv_byteenable   <= inst_busMaster.byteenable;
    inst_mpxMaster.slv_writedata    <= inst_busMaster.writedata(inst_mpxMaster.slv_writedata'range);
    inst_busMaster.readdata         <= inst_mpxMaster.slv_readdata;

    -- inst_mpxMaster --- inst_mpxSlave
    inst_mpxSlave.mpxSlv_cs     <= inst_mpxMaster.mpxMst_cs;
    inst_mpxSlave.mpxSlv_rd     <= inst_mpxMaster.mpxMst_rd;
    inst_mpxSlave.mpxSlv_wr     <= inst_mpxMaster.mpxMst_wr;
    inst_mpxSlave.mpxSlv_ale    <= inst_mpxMaster.mpxMst_ale;
    inst_mpxMaster.mpxMst_ack   <= inst_mpxSlave.mpxSlv_ack;

    inst_mpxSlave.mpxSlv_be     <= inst_mpxMaster.mpxMst_be;

    inst_mpxMaster.mpxMst_ad_i  <=  inst_mpxSlave.mpxSlv_ad_o when inst_mpxSlave.mpxSlv_ad_oen = cActivated else
                                    (others => 'Z');

    inst_mpxSlave.mpxSlv_ad_i   <=  inst_mpxMaster.mpxMst_ad_o when inst_mpxMaster.mpxMst_ad_oen = cActivated else
                                    (others => 'Z');

    -- inst_mpxSlave --- inst_spram
    inst_spram.write        <= inst_mpxSlave.mst_write;
    inst_spram.read         <= inst_mpxSlave.mst_read;

    inst_mpxSlave.mst_waitrequest   <= not inst_spram.ready;
    inst_mpxSlave.mst_readdata      <= inst_spram.readdata;

    inst_spram.byteenable   <= inst_mpxSlave.mst_byteenable;
    inst_spram.writedata    <= inst_mpxSlave.mst_writedata;
    inst_spram.address      <= inst_mpxSlave.mst_address;

    ---------------------------------------------------------------------------

    DUT_master : entity work.mpxMaster
        generic map (
            gDataWidth  => cDataWidth,
            gAddrWidth  => cAddrWidth,
            gAdWidth    => cAdWidth
        )
        port map (
            iClk                => clk,
            iRst                => rst,
            iSlv_address        => inst_mpxMaster.slv_address,
            iSlv_read           => inst_mpxMaster.slv_read,
            oSlv_readdata       => inst_mpxMaster.slv_readdata,
            iSlv_write          => inst_mpxMaster.slv_write,
            iSlv_writedata      => inst_mpxMaster.slv_writedata,
            oSlv_waitrequest    => inst_mpxMaster.slv_waitrequest,
            iSlv_byteenable     => inst_mpxMaster.slv_byteenable,
            oMpxMst_cs          => inst_mpxMaster.mpxMst_cs,
            iMpxMst_ad_i        => inst_mpxMaster.mpxMst_ad_i,
            oMpxMst_ad_o        => inst_mpxMaster.mpxMst_ad_o,
            oMpxMst_ad_oen      => inst_mpxMaster.mpxMst_ad_oen,
            oMpxMst_be          => inst_mpxMaster.mpxMst_be,
            oMpxMst_ale         => inst_mpxMaster.mpxMst_ale,
            oMpxMst_wr          => inst_mpxMaster.mpxMst_wr,
            oMpxMst_rd          => inst_mpxMaster.mpxMst_rd,
            iMpxMst_ack         => inst_mpxMaster.mpxMst_ack
        );

    DUT_slave : entity work.mpxSlave
        generic map (
            gDataWidth  => cDataWidth,
            gAddrWidth  => cAddrWidth,
            gAdWidth    => cAdWidth
        )
        port map (
            iClk                => clk,
            iRst                => rst,
            iMpxSlv_cs          => inst_mpxSlave.mpxSlv_cs,
            iMpxSlv_rd          => inst_mpxSlave.mpxSlv_rd,
            iMpxSlv_wr          => inst_mpxSlave.mpxSlv_wr,
            iMpxSlv_ale         => inst_mpxSlave.mpxSlv_ale,
            oMpxSlv_ack         => inst_mpxSlave.mpxSlv_ack,
            iMpxSlv_be          => inst_mpxSlave.mpxSlv_be,
            oMpxSlv_ad_o        => inst_mpxSlave.mpxSlv_ad_o,
            iMpxSlv_ad_i        => inst_mpxSlave.mpxSlv_ad_i,
            oMpxSlv_oen         => inst_mpxSlave.mpxSlv_ad_oen,
            oMst_address        => inst_mpxSlave.mst_address,
            oMst_byteenable     => inst_mpxSlave.mst_byteenable,
            oMst_read           => inst_mpxSlave.mst_read,
            iMst_readdata       => inst_mpxSlave.mst_readdata,
            oMst_write          => inst_mpxSlave.mst_write,
            oMst_writedata      => inst_mpxSlave.mst_writedata,
            iMst_waitrequest    => inst_mpxSlave.mst_waitrequest
        );

    theRam : entity libutil.spRam
        generic map (
            gDataWidth  => inst_spram.writedata'length,
            gAddrWidth  => inst_spram.address'length
        )
        port map (
            iRst        => rst,
            iClk        => clk,
            iWrite      => inst_spram.write,
            iRead       => inst_spram.read,
            iAddress    => inst_spram.address,
            iByteenable => inst_spram.byteenable,
            iWritedata  => inst_spram.writedata,
            oReaddata   => inst_spram.readdata,
            oAck        => inst_spram.ready
        );

    theBusMaster : entity libutil.busMaster
        generic map (
            gAddrWidth      => inst_busMaster.address'length,
            gDataWidth      => inst_busMaster.writedata'length,
            gStimuliFile    => gStim
        )
        port map(
            iClk        => clk,
            iRst        => rst,
            iAck        => inst_busMaster.ack,
            iEnable     => inst_busMaster.enable,
            iReaddata   => inst_busMaster.readdata,
            oAddress    => inst_busMaster.address,
            oByteenable => inst_busMaster.byteenable,
            oDone       => inst_busMaster.done,
            oError      => inst_busMaster.error,
            oRead       => inst_busMaster.read,
            oWrite      => inst_busMaster.write,
            oWritedata  => inst_busMaster.writedata
        );

    theClkGen : entity libutil.clkgen
        generic map (
            gPeriod => 10 ns
        )
        port map (
            iDone   => done,
            oClk    => clk
        );

    theRstGen : entity libutil.resetGen
        generic map (
            gResetTime => 100 ns
        )
        port map (
            oReset  => rst,
            onReset => open
        );
end bhv;
