-------------------------------------------------------------------------------
--! @file tbBcd2ledBhv.vhd
--
--! @brief BCD to 7-segement LED testbench
--
--! @details Verifies the correct BCD-to-led conversion
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

entity tbBcd2led is
end entity;

architecture bhv of tbBcd2led is
    --! Array type for conversion lut
    type tDecArray is array (natural range <>) of std_logic_vector(7 downto 0);
    --! Lut holding the decode values
    constant cDecodeLut : tDecArray(0 to 15) := (
        -- 0      1      2      3
        x"3F", x"06", x"5B", x"4F",
        -- 4      5      6      7
        x"66", x"6D", x"7D", x"07",
        -- 8      9      A      B
        x"7F", x"6F", x"77", x"7C",
        -- C      D      E      F
        x"39", x"5E", x"79", x"71"
    );
    --! Bcd value
    signal bcd      : std_logic_vector(3 downto 0) := (others => cInactivated);
    --! Led value
    signal led      : std_logic_vector(6 downto 0);
    --! low active led value
    signal nLed     : std_logic_vector(led'range);
    --! Led value reference
    signal led_ref  : std_logic_vector(led'range);
    --! Low active led value reference
    signal nLed_ref : std_logic_vector(led'range);
begin
    DUT : entity work.bcd2led
        port map (
            iBcdVal => bcd,
            oLed => led,
            onLed => nLed
        );

    stim : process
    begin
        bcd <=  (others => cInactivated);
        wait for 10 ns;

        for i in 0 to 15 loop
            bcd <= std_logic_vector(unsigned(bcd) + 1);
            wait for 10 ns;
        end loop;

        wait;
    end process;

    -- assign reference lut
    led_ref     <= cDecodeLut(to_integer(unsigned(bcd)))(led_ref'range);
    nLed_ref    <= not led_ref;

    process(led)
    begin
        assert (led = led_ref)
        report "High active signal wrong... "
            & integer'image(to_integer(unsigned(led)))
            & " != "
            & integer'image(to_integer(unsigned(led_ref)))
        severity failure;
    end process;

    process(nLed)
    begin
        assert (nLed = nLed_ref)
        report "Low active signal wrong... "
            & integer'image(to_integer(unsigned(nLed)))
            & " != "
            & integer'image(to_integer(unsigned(nLed_ref)))
        severity failure;
    end process;

end architecture;
