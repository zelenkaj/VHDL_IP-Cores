-------------------------------------------------------------------------------
--! @file triplePkg.vhd
--
--! @brief Triple Buffer Package
--
--! @details This package is mandatory for the triple buffer components.
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

library work;
use work.global.all;

package tripleBufferPkg is
    -- TRIPLE BUFFER SELECT
    --! Triple buffer select
    subtype tTripleSel is std_logic_vector(1 downto 0);
    --! Triple buffer select array
    type tTripleSelArray is array (natural range <>) of tTripleSel;
    --! Invalid triple buffer select
    constant cTripleSel_invalid : tTripleSel := "00";

    -- MEMORY MAPPING
    --! Natural array
    type tNaturalArray is array (natural range <>) of natural;

    --! Function to convert natural array in std_logic_vector stream.
    --! The elementSize gives the number of bits for an array entry.
    function convNaturalArrayToStdLogicVector (
        din         : tNaturalArray;
        elementSize : natural
    ) return std_logic_vector;

    --! Function to convert std_logic_vector stream to natural array.
    --! The elementCnt gives the number of array entry.
    function convStdLogicVectorToNaturalArray (
        din     : std_logic_vector;
        elementCnt : natural
    ) return tNaturalArray;

    --! Function to convert string into std_logic_vector.
    --! The string characters are considered as bits!
    function convStringToStdLogicVector (
        din : string
    ) return std_logic_vector;

    --! Function to convert string into std_logic_vector.
    --! The string characters are considered as hexadecimal values!
    function convStringToStdLogicVectorQuad (
        din : string
    ) return std_logic_vector;
end package;

package body tripleBufferPkg is
    function convNaturalArrayToStdLogicVector (
        din : tNaturalArray;
        elementSize : natural
    ) return std_logic_vector is
        variable vTmp : std_logic_vector(din'length * elementSize -1 downto 0);
    begin
        --default
        vTmp := (others => cInactivated);

        for i in din'range loop
            vTmp((i+1)*elementSize-1 downto i*elementSize) :=
                std_logic_vector(to_unsigned(din(i), elementSize));
        end loop;

        return vTmp;
    end function;

    function convStdLogicVectorToNaturalArray (
        din : std_logic_vector;
        elementCnt : natural
    ) return tNaturalArray is
        variable vIn : std_logic_vector(din'length-1 downto 0);
        variable vTmp : tNaturalArray(elementCnt - 1 downto 0);
        constant cElementBitCnt : natural := din'length / elementCnt;
    begin
        assert((din'length mod elementCnt) = 0)
        report "The element bit count is not an integer value!"
        severity failure;

        --default
        vTmp := (others => 0);

        -- assign to downto vector to use it correctly in for loop!
        vIn := din;

        for i in vTmp'range loop
            vTmp(i) := to_integer(unsigned(
                vIn((i+1) * cElementBitCnt - 1 downto i * cElementBitCnt)
            ));
        end loop;

        return vTmp;
    end function;

function convStringToStdLogicVector (
        din : string
    ) return std_logic_vector is
        variable vTmp   : std_logic_vector(din'length-1 downto 0);
        variable vBit   : std_logic;
        variable vCnt   : natural;
    begin
        vTmp := (others => cInactivated);

        vCnt := din'length;
        for i in 1 to din'length loop
            case din(i) is
                when '0' => vBit := '0';
                when '1' => vBit := '1';
                when others =>
                    assert (FALSE) report "Is not a bit value!" severity failure;
            end case;
            vTmp(vCnt-1) := vBit;
            vCnt := vCnt - 1;
        end loop;

        return vTmp;
    end function;

    function convStringToStdLogicVectorQuad (
        din : string
    ) return std_logic_vector is
        variable vTmp   : std_logic_vector(din'length*4-1 downto 0);
        variable vQuad  : std_logic_vector(3 downto 0);
        variable vCnt   : natural;
    begin
        vTmp := (others => cInactivated);

        vCnt := din'length;
        for i in 1 to din'length loop
            case din(i) is
                when '0' => vQuad := x"0";
                when '1' => vQuad := x"1";
                when '2' => vQuad := x"2";
                when '3' => vQuad := x"3";
                when '4' => vQuad := x"4";
                when '5' => vQuad := x"5";
                when '6' => vQuad := x"6";
                when '7' => vQuad := x"7";
                when '8' => vQuad := x"8";
                when '9' => vQuad := x"9";
                when 'A' => vQuad := x"A";
                when 'B' => vQuad := x"B";
                when 'C' => vQuad := x"C";
                when 'D' => vQuad := x"D";
                when 'E' => vQuad := x"E";
                when 'F' => vQuad := x"F";
                when 'a' => vQuad := x"A";
                when 'b' => vQuad := x"B";
                when 'c' => vQuad := x"C";
                when 'd' => vQuad := x"D";
                when 'e' => vQuad := x"E";
                when 'f' => vQuad := x"F";
                when others =>
                    assert (FALSE) report "Is not a hex value!" severity failure;
            end case;
            vTmp(vCnt*4-1 downto (vCnt-1)*4) := vQuad;
            vCnt := vCnt - 1;
        end loop;

        return vTmp;
    end function;
end package body;
