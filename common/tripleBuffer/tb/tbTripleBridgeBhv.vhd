-------------------------------------------------------------------------------
--! @file tbTripleBridgeBhv.vhd
--
--! @brief Triple Bridge testbench
--
--! @details The testbench verifies if the triple bridge operates correctly.
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

entity tbTripleBridge is
end tbTripleBridge;

architecture bhv of tbTripleBridge is
    signal clk              : std_logic;
    signal rst              : std_logic;
    signal done             : std_logic;
    signal error            : std_logic := cInactivated;

    constant cInAddrWidth   : natural := 8;
    constant cOutAddrWidth  : natural := 10;
    constant cInputBuffers  : natural := 10;
    constant cInputBase     : tNaturalArray(0 to cInputBuffers) :=
    (
        16#04#, 16#10#, 16#1C#, 16#34#, 16#58#,
        16#7C#, 16#80#, 16#8C#, 16#A0#, 16#C4#,
        16#E8#
    );
    constant cTriBufOffset  : tNaturalArray(0 to cInputBuffers*3-1) := (
        16#000#, 16#00C#, 16#018#,
        16#018#, 16#024#, 16#030#,
        16#030#, 16#048#, 16#060#,
        16#060#, 16#084#, 16#0A8#,
        16#0A8#, 16#0CC#, 16#0F0#,
        16#0F0#, 16#0F4#, 16#0F8#,
        16#0F8#, 16#104#, 16#110#,
        16#110#, 16#124#, 16#138#,
        16#138#, 16#15C#, 16#180#,
        16#180#, 16#1A4#, 16#1C8#
    );

    signal enable           : std_logic;
    signal inAddr           : std_logic_vector(cInAddrWidth-1 downto 0);
    signal tripleSel        : tTripleSelArray(cInputBuffers-1 downto 0);
    signal outAddr          : std_logic_vector(cOutAddrWidth-1 downto 0);
    signal outAddr_unreg    : std_logic_vector(cOutAddrWidth-1 downto 0);

    signal enable_l         : std_logic;
    signal enable_ll        : std_logic;
    signal outAddr_l        : std_logic_vector(cOutAddrWidth-1 downto 0);
    signal outAddr_unreg_l  : std_logic_vector(cOutAddrWidth-1 downto 0);
begin
    DUT : entity work.tripleBridge
        generic map (
            gInAddrWidth    => cInAddrWidth,
            gOutAddrWidth   => cOutAddrWidth,
            gInputBuffers   => cInputBuffers,
            gInputBase      => cInputBase,
            gTriBufOffset   => cTriBufOffset
        )
        port map (
            iRst            => rst,
            iClk            => clk,
            iEnable         => enable,
            iAddr           => inAddr,
            iTripleSel      => tripleSel,
            oAddr_unreg     => outAddr_unreg,
            oAddr           => outAddr
        );

    assert (error = cInactivated)
    report "Triple buffer bridge translation is faulty!"
    severity failure;

    --! This process checks the result with the falling clock edge
    theDutSense : process(rst, clk)
    begin
        if rst = cActivated then
            error           <= cInactivated;
            enable_l        <= cInactivated;
            enable_ll       <= cInactivated;
            outAddr_l       <= (others => cInactivated);
            outAddr_unreg_l <= (others => cInactivated);
        elsif rising_edge(clk) then
            --add cycle delay
            enable_l        <= enable;
            enable_ll       <= enable_l;
            outAddr_l       <= outAddr;
            outAddr_unreg_l <= outAddr_unreg;
        elsif falling_edge(clk) then
            --default
            error           <= cInactivated;

            if enable = cActivated and enable_l = cActivated then
                if (unsigned(outAddr_unreg_l) + 1) /= unsigned(outAddr_unreg) then
                    --outaddress is not incrementing
                    error   <= cActivated;
                end if;
            end if;
            if enable = cActivated and enable_ll = cActivated then
                if (unsigned(outAddr_l) + 1) /= unsigned(outAddr) then
                    --outaddress is not incrementing
                    error   <= cActivated;
                end if;
            end if;
        end if;
    end process;

    theDutStim : process
    begin
        done                <= cInactivated;
        inAddr              <= (others => cInactivated);
        tripleSel           <= (others => cTripleSel_invalid);
        enable              <= cInactivated;
        wait until rst = cInactivated;
        wait until rising_edge(clk);

        --test every input buffer
        for selInBuf in 0 to cInputBuffers-1 loop
            --test every triple buffer
            for selTriBuf in 1 to 3 loop
                tripleSel   <= (others => std_logic_vector(to_unsigned(selTriBuf, 2)));
                --loop through input buffer
                for i in cInputBase(selInBuf) to cInputBase(selInBuf+1)-1 loop
                    enable  <= cActivated;
                    inAddr  <= std_logic_vector(to_unsigned(i, inAddr'length));
                    wait until rising_edge(clk);
                end loop;
            end loop;
        end loop;
        enable              <= cInactivated;

        done                <= cActivated;
        wait;
    end process;

    theClkGen : entity work.clkgen
        generic map (
            gPeriod     => 10 ns
        )
        port map (
            iDone       => done,
            oClk        => clk
        );

    theRstGen : entity work.resetGen
        generic map (
            gResetTime  => 100 ns
        )
        port map (
            oReset      => rst,
            onReset     => open
        );
end bhv;
