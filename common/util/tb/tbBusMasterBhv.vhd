-------------------------------------------------------------------------------
--! @file tbBusMasterBhv.vhd
--
--! @brief Testbench for busMaster
--
--! @details the busMaster will be stimulated with the cStimuliFile.
--
-------------------------------------------------------------------------------
-- Entity : busMaster
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

use std.textio.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_textio.all;
use std.env.all;

library work;
use work.global.all;
use work.busMasterPkg.all;

entity tbBusMasterBhv is
end entity tbBusMasterBhv;

architecture Bhv of tbBusMasterBhv is
    --***********************************************************************--
    -- TYPES, RECORDS and CONSTANTS:
    --***********************************************************************--
    type tMemory is array (natural range <>) of std_logic_vector(cMaxBitWidth-1 downto 0);

    constant cMemoryRange   : natural := 10;
    constant cInitMemory    : tMemory(cMemoryRange-1 downto 0) := (others => (others => '0'));
    constant cPeriode       : time := 2 ns;
    constant cStimuliFile   : string := "../tb/tbBusMasterBhv_TB_stim.txt";

    --***********************************************************************--
    -- SIGNALS and VARIABLES:
    --***********************************************************************--
    signal Memory : tMemory(cMemoryRange-1 downto 0) := cInitMemory;
    signal oRst, oClk, oEnable, oAck : std_logic := cInactivated;
    signal iWrite, iRead, iSelect, iError, iDone : std_logic;
    signal iByteenable : std_logic_vector((cMaxBitWidth/8)-1 downto 0);
    signal iAddress, iWriteData, oReadData : std_logic_vector(cMaxBitWidth-1 downto 0);
    signal memoryAccessCounter : natural;
begin

    oRst <= '1' after 0 ns,
            '0' after 10 ns;
    oClk <= not oClk after cPeriode/2 when iDone /= cActivated else '0' after cPeriode/2;

    --***********************************************************************--
    -- : simulates a single port memory with byte enable
    --***********************************************************************--
    TheMemory : process(iSelect, iWrite, iRead, iAddress, iWriteData, iByteenable)
    begin
        oReadData <= (others => 'X');
        if iSelect = cActivated and iWrite /= iRead and  cMemoryRange >= to_integer(unsigned(iAddress))then
            if iWrite = cActivated then
                Memory(to_integer(unsigned(iAddress))) <= (others => 'X');
                case iByteenable is
                    when "1111" =>
                        Memory(to_integer(unsigned(iAddress))) <= iWriteData after cPeriode/4;
                    when "0011" =>
                        Memory(to_integer(unsigned(iAddress)))(15 downto 0) <= iWriteData(15 downto 0) after cPeriode/4;
                    when "1100" =>
                        Memory(to_integer(unsigned(iAddress)))(31 downto 16) <= iWriteData(15 downto 0) after cPeriode/4;
                    when "0001" | "0010" | "0100" | "1000" =>
                        for i in iByteenable'range loop
                            if iByteenable(i) = cActivated then
                                Memory(to_integer(unsigned(iAddress)))((i+1)*8-1 downto i*8) <= iWriteData(7 downto 0) after cPeriode/4;
                            end if;
                        end loop;
                    when others =>
                        -- default assignment
                end case;

            elsif iRead = cActivated then
                 case iByteenable is
                    when "1111" =>
                        oReadData <= Memory(to_integer(unsigned(iAddress)));
                    when "0011" =>
                        oReadData(15 downto 0) <= Memory(to_integer(unsigned(iAddress)))(15 downto 0);
                    when "1100" =>
                        oReadData(15 downto 0)  <= Memory(to_integer(unsigned(iAddress)))(31 downto 16);
                    when "0001" | "0010" | "0100" | "1000" =>
                        for i in iByteenable'range loop
                            if iByteenable(i) = cActivated then
                                oReadData(7 downto 0) <= Memory(to_integer(unsigned(iAddress)))((i+1)*8-1 downto i*8);
                            end if;
                        end loop;
                    when others =>
                        -- default assignment
                end case;
            end if;
        end if;
    end process TheMemory;

    --***********************************************************************--
    -- : stimualte the ACK signal
    --***********************************************************************--
    StimAck: process(oClk, oRst, iWrite, iRead, iSelect )
    begin
        if oRst = cActivated then
            oAck <= cInactivated;
        elsif oClk' event and oClk = cActivated then
            oAck <= cInactivated;
            if iSelect = cActivated and (iWrite = cActivated or iRead = cActivated) then
                memoryAccessCounter <= memoryAccessCounter + 1;
                if memoryAccessCounter = 3 then
                    memoryAccessCounter <= 0;
                else
                    oAck <= cActivated;
                end if;
            end if;
        end if;
    end process StimAck;

    --***********************************************************************--
    -- :
    --***********************************************************************--
    Stimprocess : process is
    begin
        oEnable <= cActivated;

        wait;
    end process Stimprocess;

    --***********************************************************************--
    -- :
    --***********************************************************************--
    AssertBusMaster: process(iDone, iError)
    begin
        if iDone = cActivated then
            if iError = cActivated then
                assert false report "tbBusMasterBhv: self test failed " severity failure;
            else
                --stop; -- VHDL 2008 specific, but not compatible to the scripting!
                -- steady state will be detected and simulation will end!
            end if;
        end if;
    end process AssertBusMaster;

    --***********************************************************************--
    -- : INSTANTIATION of the DUT
    --***********************************************************************--
    DUT: entity work.busMaster(bhv)
        generic map(
            gAddrWidth      => cMaxBitWidth,
            gDataWidth      => cMaxBitWidth,
            gStimuliFile    => cStimuliFile
        )
        port map(
            iRst            => oRst,
            iClk            => oClk,
            iEnable         => oEnable,
            iAck            => oAck,
            iReaddata       => oReadData,
            oWrite          => iWrite,
            oRead           => iRead,
            oSelect         => iSelect,
            oAddress        => iAddress,
            oByteenable     => iByteenable,
            oWritedata      => iWriteData,
            oError          => iError,
            oDone           => iDone
        );

end architecture Bhv;