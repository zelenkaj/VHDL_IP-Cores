-------------------------------------------------------------------------------
--! @file spRamBhv.vhd
--
--! @brief Single Port Ram Model
--
--! @details This RAM model is provided for simulation purpose only!
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

--! Common library
library libcommon;
--! Use common library global package
use libcommon.global.all;

entity spRam is
    generic (
        --! Data width
        gDataWidth  : natural := 32;
        --! Number of words with size gDataWidth
        gAddrWidth  : natural := 10
    );
    port (
        --! Reset
        iRst        : in    std_logic;
        --! Clock
        iClk        : in    std_logic;
        --! Write to memory
        iWrite      : in    std_logic;
        --! Read from memory
        iRead       : in    std_logic;
        --! Address (byte-addressing)
        iAddress    : in    std_logic_vector(gAddrWidth-1 downto 0);
        --! Byteenable
        iByteenable : in    std_logic_vector(gDataWidth/cByteLength-1 downto 0);
        --! Writedata
        iWritedata  : in    std_logic_vector(gDataWidth-1 downto 0);
        --! Readdata
        oReaddata   : out   std_logic_vector(gDataWidth-1 downto 0);
        --! Access acknowledge
        oAck        : out   std_logic
    );
end spRam;

architecture bhv of spRam is
    --! Memory size [words]
    constant cMemorySize : natural := 2**gAddrWidth;
    --! Memory type
    type tMemory is array (cMemorySize-1 downto 0) of std_logic_vector(gDataWidth-1 downto 0);
    --! Memory initialization constant
    constant cMemoryInit    : tMemory := (others => (others => cInactivated));

    --! The write acknowlegde internal
    signal writeAck : std_logic;
    --! The read acknowlegde internal
    signal readAck  : std_logic;
begin
    ---------------------------------------------------------------------------
    -- Output map
    ---------------------------------------------------------------------------
    oAck <= writeAck or readAck;

    --! This is the memory process. It uses the memory as variable to save
    --! simulator resources.
    theMemoryProc : process(iRst, iClk)
        variable vAddr_tmp  : natural := 0;
        --! The memory
        variable vMemory    : tMemory;
    begin
        if iRst = cActivated then
            -- initialize memory to init vector
            vMemory := cMemoryInit;
        elsif rising_edge(iClk) then
            vAddr_tmp := to_integer(unsigned(iAddress));
            if iWrite = cActivated then
                for i in iByteenable'range loop
                    if iByteenable(i) = cActivated then
                        vMemory(vAddr_tmp)((i+1)*cByteLength-1 downto i*cByteLength) := iWritedata((i+1)*cByteLength-1 downto i*cByteLength);
                    end if;
                end loop;
            end if;

            if iRead = cActivated then
                oReaddata <= vMemory(vAddr_tmp);
            else
                oReaddata <= (others => cInactivated);
            end if;
        end if;
    end process;

    -- Write ack has no delay.
    writeAck <= iWrite and not(iRead);

    --! Assign read ack with one cycle delay.
    readAckGen : process(iRst, iClk)
    begin
        if iRst = cActivated then
            readAck <= cInactivated;
        elsif rising_edge(iClk) then
            if (iRead = cActivated and readAck = cInactivated) and iWrite = cInactivated then
                readAck <= cActivated;
            else
                readAck <= cInactivated;
            end if;
        end if;
    end process;
end bhv;
