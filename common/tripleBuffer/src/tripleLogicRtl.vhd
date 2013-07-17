-------------------------------------------------------------------------------
--! @file tripleLogicRtl.vhd
--
--! @brief Triple Buffer Logic
--
--! @details This instance implements a triple buffer arbitration logic.
--! The instance is connected to the producing and consuming devices, which
--! trigger a buffer change.
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

entity tripleLogic is
    port (
        --! Global reset signal
        iRst        : in std_logic;
        --! Global clock, producer and consumer must be synchronous
        iClk        : in std_logic;
        --! Producer trigger
        iPro_trig   : in std_logic;
        --! Producer buffer select
        oPro_sel    : out tTripleSel;
        --! Consumer trigger
        iCon_trig   : in std_logic;
        --! Consumer buffer select
        oCon_sel    : out tTripleSel
    );
end tripleLogic;

architecture rtl of tripleLogic is
    --! consumer initialisation (must not be zero or equal to producer's!)
    constant cTriBuf_con    : tTripleSel := "01";
    --! producer initialisation (must not be zero or equal to consumer's!)
    constant cTriBuf_pro    : tTripleSel := "10";
    --! latest initialisation buffer (must not be zero or equal to consumer's!)
    constant cTriBuf_latest : tTripleSel := "11";

    -- register type
    type tReg is record
        pro     : tTripleSel;
        con     : tTripleSel;
        latest  : tTripleSel;
    end record;

    -- regsiter initialisation
    constant cRegInit : tReg := (
        cTriBuf_pro,
        cTriBuf_con,
        cTriBuf_latest
    );

    -- registers
    signal reg      : tReg;
    -- registers next
    signal reg_next : tReg;
begin
    -- output select
    oCon_sel <= reg.con;
    oPro_sel <= reg.pro;

    regProc : process(iRst, iClk)
    begin
        if iRst = cActivated then
            reg <= cRegInit;
        elsif rising_edge(iClk) then
            reg <= reg_next;
        end if;
    end process;

    --! This process assigns the consumer and producer instance to a free
    --! triple buffer. The consumer is always assigned to the latest buffer,
    --! which is basically assigned to the lastly produced buffer (by the
    --! producer). The producer is assigned to the buffer that is not assigned
    --! to itself and the consumer (xor operation).
    --! This process ensures consuming consistent and latest data.
    comProc : process (
        iPro_trig,
        iCon_trig,
        reg
    )
    begin
        --default
        reg_next <= reg;

        -- handle special case -> both instances request buffer change
        if iPro_trig = cActivated and iCon_trig = cActivated then
            -- assign latest buffer to consumer, since producer gets free buffer
            reg_next.con    <= reg.pro;
        end if;

        if iPro_trig = cActivated then
            -- producer gets free buffer
            reg_next.pro    <= reg.con xor reg.pro;

            -- mark latest buffer for next consumer switch
            reg_next.latest <= reg.pro;
        elsif iCon_trig = cActivated then
            -- assign latest buffer to consumer
            -- Note: If latest does not change, consumer stays at this buffer!
            reg_next.con    <= reg.latest;
        end if;
    end process;
end rtl;
