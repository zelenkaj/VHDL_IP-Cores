-------------------------------------------------------------------------------
--! @file tbAddrDecodeRtl.vhd
--
--! @brief Address Decoder testbench
--
--! @details The testbench verifies if the decoder operates correctly.
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

entity tbAddrDecode is
end tbAddrDecode;

architecture bhv of tbAddrDecode is
    constant cAddrWidth : natural := 32;
    constant cBase : natural := 16#0001_0020#;
    constant cHigh : natural := 16#0005_003F#;
    constant cMid : natural := (cHigh-cBase)/2;
    --! stop simulation with address
    constant cDone : natural := 16#0008_0000#;

    signal clk : std_logic;
    signal rst : std_logic;
    signal done : std_logic;
    signal errCnt_trig : std_logic;
    signal errCnt : natural := 0;

    signal en : std_logic;
    signal addr : std_logic_vector(cAddrWidth-1 downto 0) := (others => cInactivated);
    signal addr_next : std_logic_vector(addr'range) := (others => cInactivated);
    signal sel : std_logic;
begin
    DUT : entity work.addrDecode
        generic map (
            gAddrWidth => cAddrWidth,
            gBaseAddr => cBase,
            gHighAddr => cHigh
        )
        port map (
            iEnable => en,
            iAddress => addr,
            oSelect => sel
        );

    assert (not(done = cActivated and errCnt = 0))
        report "Simulation completed successful!" severity note;

    assert (not(done = cActivated and errCnt /= 0))
        report "Simulation completed with errors!" severity failure;

    --! simply increment the address
    stimAddrCnt : process(rst, clk)
    begin
        if rst = cActivated then
            addr <= (others => cInactivated);
        elsif rising_edge(clk) then
            addr <= addr_next;
        end if;
    end process;

    addr_next <= std_logic_vector(unsigned(addr) + 1);

    en <=   cActivated when to_integer(unsigned(addr)) /= cMid else
            cInactivated;

    --stop simulation
    done <= cActivated when cDone = to_integer(unsigned(addr_next)) else
            cInactivated;

    errCntProc : process(rst, errCnt_trig)
    begin
        if rst = cActivated then
            errCnt <= 0;
        elsif rising_edge(errCnt_trig) then
            errCnt <= errCnt + 1;
        end if;
    end process;

    --! check DUT's output
    checkDut : process(rst, clk)
        variable vAddr : natural;
    begin
        if rst = cActivated then
            errCnt_trig <= cInactivated;
        elsif falling_edge(clk) then
            vAddr := to_integer(unsigned(addr));
            errCnt_trig <= cInactivated;
            if en = cActivated then
                if (cBase <= vAddr) and (vAddr <= cHigh) then
                    if sel /= cActivated then
                        errCnt_trig <= cActivated;
                        assert (FALSE) report "Addr = " & integer'IMAGE(vAddr)
                            & " not decoded!"
                            severity warning;
                    end if;
                else
                    if sel /= cInactivated then
                        errCnt_trig <= cActivated;
                        assert (FALSE) report "Addr = " & integer'IMAGE(vAddr)
                            & " decoded wrongly!"
                            severity warning;
                    end if;
                end if;
            else
                if sel /= cInactivated then
                    errCnt_trig <= cActivated;
                    assert (FALSE) report "Deactivated decoder does decoding!"
                        severity warning;
                end if;
            end if;
        end if;
    end process;

    theClkGen : entity work.clkgen
        generic map (
            gPeriod => 10 ns
        )
        port map (
            iDone => done,
            oClk => clk
        );

    theRstGen : entity work.resetGen
        generic map (
            gResetTime => 100 ns
        )
        port map (
            oReset => rst,
            onReset => open
        );
end bhv;
