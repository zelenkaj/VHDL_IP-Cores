-------------------------------------------------------------------------------
--! @file tbRegisterFileBhv.vhd
--
--! @brief testbench for Register file
--
-------------------------------------------------------------------------------
-- Entity : btRegisterFile
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

entity tbRegisterFile is
end tbRegisterFile;

architecture Bhv of tbRegisterFile is

    constant cAddressWidth          : natural := 3;
    constant cWordWidth             : natural := 32;
    constant cPeriode               : time := 20 ns;
    ---- Signal declarations used on the diagram ----

    signal Clk, Rst                 : std_logic := cActivated;
    signal Error, Done              : std_logic;
    signal AckA, EnableA            : std_logic;
    signal ReadA, WriteA, SelectA   : std_logic;
    signal WriteB                   : std_logic;
    signal ByteenableA, ByteenableB : std_logic_vector(cWordWidth/8-1 downto 0);
    signal AddressA, AddressB       : std_logic_vector(cAddressWidth-1 downto 0);
    signal ReadDataA, ReadDataB     : std_logic_vector(cWordWidth-1 downto 0);
    signal WriteDataA, WriteDataB   : std_logic_vector(cWordWidth-1 downto 0);

begin

    WriteB          <= cInactivated;
    ByteenableB     <= (others => cInactivated);
    AddressB        <= (others => cInactivated);
    WriteDataB      <= (others => cInactivated);

    EnableA         <= cActivated;
    AckA            <= cActivated;
    Clk             <= not Clk after cPeriode/2 when Done /= cActivated else cInactivated;
    Rst             <= cInactivated after 2*cPeriode;

    ----  Component instantiations  ----
    DUT : entity work.registerFile(Rtl)
        generic map(
            gRegCount   => 2**cAddressWidth
        )
        port map(
        iClk               => Clk,
        iRst               => Rst,
        iWriteA            => WriteA,
        iWriteB            => WriteB,
        iByteenableA       => ByteenableA,
        iByteenableB       => ByteenableB,
        iAddrA             => AddressA,
        iAddrB             => AddressB,
        iWritedataA        => WriteDataA,
        oReaddataA         => ReadDataA,
        iWritedataB        => WriteDataB,
        oReaddataB         => ReadDataB
      );

    busMaster : entity work.busMaster(Bhv)
      generic map (
           gAddrWidth       => cAddressWidth,
           gDataWidth       => cWordWidth,
           gStimuliFile     => "../../lib/tb/tbRegisterFile_stim.txt"
      )
      port map(
            iRst            => Rst,
            iClk            => Clk,
            iEnable         => EnableA,
            iAck            => AckA,
            iReaddata       => ReadDataA,
            oWrite          => WriteA,
            oRead           => ReadA,
            oSelect         => SelectA,
            oAddress        => AddressA,
            oByteenable     => ByteenableA,
            oWritedata      => WriteDataA,
            oError          => Error,
            oDone           => Done
      );

      AssertBusMaster: process (Error)
      begin
            if Error = cActivated then
                assert false report "Simulation of tbRegisterFileBhv FAILED" severity failure;
            end if;
      end process AssertBusMaster;

end architecture Bhv;
