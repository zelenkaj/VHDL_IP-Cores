-------------------------------------------------------------------------------
--! @file tripleBridgeRtl.vhd
--
--! @brief Triple Buffer Bridge
--
--! @details This compontent is a triple buffer bridge. Each input buffer
--! is forwarded to the selected triple buffer.
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
--! need reduce or operation
use ieee.std_logic_misc.OR_REDUCE;
use work.global.all;
use work.tripleBufferPkg.all;

entity tripleBridge is
    generic (
        --! Input address width
        gInAddrWidth    : natural := 3;
        --! Output address width
        gOutAddrWidth   : natural := 4;
        --! Number of input buffers
        gInputBuffers   : natural := 2;
        --! Base addresses of input buffers (for address decoders)
        gInputBase      : tNaturalArray := (
            16#00#, 16#10#, 16#20#
        );
        --! Triple buffer mapping (translation values for LUT)
        gTriBufOffset   : tNaturalArray := (
            16#00#, 16#10#, 16#20#,
            16#20#, 16#30#, 16#40#
        )
    );
    port (
        --! Global reset
        iRst                    : in std_logic;
        --! Global clk
        iClk                    : in std_logic;
        --! Input enable
        iEnable                 : in std_logic;
        --! Input address (byte-addresses)
        iAddr                   : in std_logic_vector(gInAddrWidth-1 downto 0);
        --! Triple buffer select
        iTripleSel              : in tTripleSelArray(gInputBuffers-1 downto 0);
        --! Unregistered output address (byte-addresses)
        oAddr_unreg             : out std_logic_vector(gOutAddrWidth-1 downto 0);
        --! Output address (byte-addresses)
        oAddr                   : out std_logic_vector(gOutAddrWidth-1 downto 0);
        --! Any buffer selected
        oBufferSelAny_unreg     : out std_logic;
        --! None buffer selected
        oBufferSelNone_unreg    : out std_logic
    );
end tripleBridge;

architecture rtl of tripleBridge is
    --! Function to generate LUT values (stuff reserved fields)
    function lutInitGen (iMap   : tNaturalArray) return tNaturalArray is
        variable vTmpArray      : tNaturalArray(0 to gInputBuffers*4-1);
        variable vTmpFull       : std_logic_vector(logDualis(vTmpArray'length)-1 downto 0);
        variable vTmpDibit      : std_logic_vector(1 downto 0);
        variable vCnt           : natural;
    begin
        --default
        vTmpArray := (others => 0);
        vCnt := 0;

        for i in vTmpArray'range loop
            vTmpFull := std_logic_vector(to_unsigned(i, vTmpFull'length));
            vTmpDibit := vTmpFull(vTmpDibit'range);
            case vTmpDibit is
                when "00" => NULL;
                    --reserved
                when "01" | "10" | "11" =>
                    --not reserved
                    vTmpArray(i) := iMap(vCnt);
                    vCnt := vCnt + 1;
                when others => NULL;
            end case;
        end loop;

        return vTmpArray;
    end function;

    --! LUT values (natural array)
    constant cLutNaturalArray : tNaturalArray := lutInitGen(gTriBufOffset);

    --! Any buffer selected
    signal bufferSelAny         : std_logic;
    --! Address decoder select input buffer (one-hot-coded)
    signal bufferSel            : std_logic_vector(gInputBuffers-1 downto 0);
    --! Select input buffer (binary-coded)
    signal bufferSel_binary     : std_logic_vector(logDualis(gInputBuffers)-1 downto 0);
    --! Selected triple buffer select
    signal triSel               : tTripleSel;
    --! LUT input address
    signal lutAddr              : std_logic_vector(logDualis(gInputBuffers * 4)-1 downto 0);
    --! LUT output
    signal lutOut               : natural;
    --! LUT translation offset
    signal lutTransOffset       : std_logic_vector(gOutAddrWidth-1 downto 0);
    --! Translated address
    signal transAddr            : unsigned(gOutAddrWidth-1 downto 0);
    --! Translation address register
    signal transAddrReg         : std_logic_vector(gOutAddrWidth-1 downto 0);
    --! Translation address register next
    signal transAddrReg_next    : std_logic_vector(transAddrReg'range);
begin
    -- output signals
    oBufferSelAny_unreg     <= bufferSelAny;
    oBufferSelNone_unreg    <= not bufferSelAny and iEnable;
    oAddr                   <= transAddrReg;
    oAddr_unreg             <= transAddrReg_next;

    bufferSelAny            <= OR_REDUCE(bufferSel);

    --! Generate for every input buffer an address decoder
    genAddrDec : for i in bufferSel'range generate
    begin
        theAddrDecode : entity work.addrDecode
            generic map (
                gAddrWidth  => gInAddrWidth,
                gBaseAddr   => gInputBase(i),
                gHighAddr   => gInputBase(i+1)-1
            )
            port map (
                iEnable     => iEnable,
                iAddress    => iAddr,
                oSelect     => bufferSel(i)
            );
    end generate;

    --! Convert the one-hot-coded address decoders' output to binary
    theBinaryEncode : entity work.binaryEncoder
        generic map (
            gDataWidth  => bufferSel'length
        )
        port map (
            iOneHot     => bufferSel,
            oBinary     => bufferSel_binary
        );

    triSel          <= iTripleSel(to_integer(unsigned(bufferSel_binary)));
    lutAddr         <= bufferSel_binary & triSel;

    -- Input buffer look-up-table
    lutOut          <= cLutNaturalArray(to_integer(unsigned(lutAddr)));
    --convert natural to std_logic_vector
    lutTransOffset  <= std_logic_vector(to_unsigned(lutOut, lutTransOffset'length));

    -- Address arithmetic
    --Add translation offset to input address
    transAddr       <= resize(unsigned(iAddr), gOutAddrWidth) +
                              unsigned(lutTransOffset);

    transAddrReg_next <= std_logic_vector(transAddr);

    --! Register output address
    regClk : process(iRst, iClk)
    begin
        if iRst = cActivated then
            transAddrReg <= (others => cInactivated);
        elsif rising_edge(iClk) then
            transAddrReg <= transAddrReg_next;
        end if;
    end process;
end rtl;
