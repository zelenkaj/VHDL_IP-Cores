-------------------------------------------------------------------------------
--! @file alteraTripleBufferRtl.vhd
--
--! @brief Triple Buffer for Altera platform
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

entity alteraTripleBuffer is
    generic (
        --! Address width
        gAddressWidth   : natural := 3;
        --! Number of input buffers
        gInputBuffers   : natural := 2;
        --! Input buffer base addresses
        gInputBase      : string := "041424";
        --! Triple buffer mapping
        gTriBufOffset   : string := "001020203040";
        --! Port A configuration (0b0 = consumer and 0b1 = producer)
        gPortAconfig    : string := "2";
        --! Enable Stream access at port A (0 = false, otherwise = true)
        gPortAstream    : natural := 0
    );
    port (
        --! Reset source input
        rsi_r0_reset            : in std_logic;
        --! Clock source input
        csi_c0_clock            : in std_logic;
        -- Port A
        --! Avalon MM slave port A Address
        avs_porta_address       : in std_logic_vector(gAddressWidth-1 downto 2);
        --! Avalon MM slave port A Byteenable
        avs_porta_byteenable    : in std_logic_vector(3 downto 0);
        --! Avalon MM slave port A Write
        avs_porta_write         : in std_logic;
        --! Avalon MM slave port A Read
        avs_porta_read          : in std_logic;
        --! Avalon MM slave port A Writedata
        avs_porta_writedata     : in std_logic_vector(31 downto 0);
        --! Avalon MM slave port A Readdata
        avs_porta_readdata      : out std_logic_vector(31 downto 0);
        --! Avalon MM slave port A Acknowledge
        avs_porta_waitrequest   : out std_logic;
        -- Port B
        --! Avalon MM slave port B Address
        avs_portb_address       : in std_logic_vector(gAddressWidth-1 downto 2);
        --! Avalon MM slave port B Byteenable
        avs_portb_byteenable    : in std_logic_vector(3 downto 0);
        --! Avalon MM slave port B Write
        avs_portb_write         : in std_logic;
        --! Avalon MM slave port B Read
        avs_portb_read          : in std_logic;
        --! Avalon MM slave port B Writedata
        avs_portb_writedata     : in std_logic_vector(31 downto 0);
        --! Avalon MM slave port B Readdata
        avs_portb_readdata      : out std_logic_vector(31 downto 0);
        --! Avalon MM slave port B Acknowledge
        avs_portb_waitrequest   : out std_logic
    );
end alteraTripleBuffer;

architecture rtl of alteraTripleBuffer is
    --! port A acknowledge
    signal porta_ack    : std_logic;
    --! port B acknowledge
    signal portb_ack    : std_logic;
begin
    --assign outputs
    avs_porta_waitrequest   <= not porta_ack;
    avs_portb_waitrequest   <= not portb_ack;

    theTripleBuffer : entity work.tripleBuffer
        generic map (
            gAddressWidth   => gAddressWidth,
            gInputBuffers   => gInputBuffers,
            gInputBase      => convStringToStdLogicVectorQuad(gInputBase),
            gTriBufOffset   => convStringToStdLogicVectorQuad(gTriBufOffset),
            gPortAconfig    => convStringToStdLogicVector(gPortAconfig),
            gPortAstream    => gPortAstream
        )
        port map (
            iRst            => rsi_r0_reset,
            iClk            => csi_c0_clock,
            iAddress_A      => avs_porta_address,
            iByteenable_A   => avs_porta_byteenable,
            iWrite_A        => avs_porta_write,
            iRead_A         => avs_porta_read,
            iWritedata_A    => avs_porta_writedata,
            oReaddata_A     => avs_porta_readdata,
            oAck_A          => porta_ack,
            iAddress_B      => avs_portb_address,
            iByteenable_B   => avs_portb_byteenable,
            iWrite_B        => avs_portb_write,
            iRead_B         => avs_portb_read,
            iWritedata_B    => avs_portb_writedata,
            oReaddata_B     => avs_portb_readdata,
            oAck_B          => portb_ack
        );
end rtl;
