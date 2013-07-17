-------------------------------------------------------------------------------
--! @file tbTripleBufferBhv.vhd
--
--! @brief Triple Buffer testbench
--
--! @details The testbench verifies if the triple buffer operates correctly.
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

entity tbTripleBuffer is
    generic (
        --! Number of input buffers
        gInputBuffers   : natural := 2;
        --! Input buffer base addresses
        gInputBase      : std_logic_vector := x"241404";
        --! Triple buffer mapping
        gTriBufOffset   : std_logic_vector := x"403020201000";
        --! Port A configuration (0b0 = consumer and 0b1 = producer)
        gPortAconfig    : std_logic_vector := "10";
        --! Enable Stream access at port A (0 = false, otherwise = true)
        gPortAstream    : natural := 0
    );
end tbTripleBuffer;

architecture bhv of tbTripleBuffer is
    constant cInputBase     : tNaturalArray(gInputBuffers downto 0) :=
        convStdLogicVectorToNaturalArray(gInputBase, gInputBuffers+1);
    constant cTriBufOffset  : tNaturalArray(gInputBuffers*3-1 downto 0) :=
        convStdLogicVectorToNaturalArray(gTriBufOffset, gInputBuffers*3);

    constant cDataWidth     : natural := 32;
    constant cAddressWidth  : natural := logDualis(
        cInputBase(cInputBase'left) + cDataWidth/8
    );

    signal clk  : std_logic;
    signal rst  : std_logic;
    signal done : std_logic;

    type tPort is record
        address     : std_logic_vector(cAddressWidth-1 downto 2);
        byteenable  : std_logic_vector(cDataWidth/8-1 downto 0);
        write       : std_logic;
        read        : std_logic;
        writedata   : std_logic_vector(cDataWidth-1 downto 0);
        readdata    : std_logic_vector(cDataWidth-1 downto 0);
        ack         : std_logic;
    end record;

    signal portA : tPort;
    signal portB : tPort;
begin
    --! The DUT
    DUT : entity work.tripleBuffer
        generic map (
            gAddressWidth   => cAddressWidth,
            gInputBuffers   => gInputBuffers,
            gInputBase      => gInputBase,
            gTriBufOffset   => gTriBufOffset,
            gPortAconfig    => gPortAconfig,
            gPortAstream    => gPortAstream
        )
        port map (
            iRst            => rst,
            iClk            => clk,
            iAddress_A      => portA.address,
            iByteenable_A   => portA.byteenable,
            iWrite_A        => portA.write,
            iRead_A         => portA.read,
            iWritedata_A    => portA.writedata,
            oReaddata_A     => portA.readdata,
            oAck_A          => portA.ack,
            iAddress_B      => portB.address,
            iByteenable_B   => portB.byteenable,
            iWrite_B        => portB.write,
            iRead_B         => portB.read,
            iWritedata_B    => portB.writedata,
            oReaddata_B     => portB.readdata,
            oAck_B          => portB.ack
        );

    --! Check the conversion functions from the package
    thePkgCheck : process
        variable vElementSize : natural;
        constant vStringIn : string :=
         "01F801D401B001B0018C0168016801480128012801180108010800E400C000C0009C00780078005800380038002800180018000C0000";
        constant vConvString : std_logic_vector :=
        x"01F801D401B001B0018C0168016801480128012801180108010800E400C000C0009C00780078005800380038002800180018000C0000";
    begin
        assert (convStringToStdLogicVectorQuad(vStringIn) = vConvString)
        report "String to std_logic_vector conversion failed!"
        severity failure;

        -- get element size of input base stream
        vElementSize := gInputBase'length / (gInputBuffers+1);

        -- convert natural array back to a stream
        assert (
            convNaturalArrayToStdLogicVector(cInputBase, vElementSize) =
            gInputBase
        )
        report "Conversion failed!"
        severity failure;

        -- get element size of triple buffer mapping stream
        vElementSize := gTriBufOffset'length / (gInputBuffers*3);

        -- convert natural array back to a stream
        assert (
            convNaturalArrayToStdLogicVector(cTriBufOffset, vElementSize) =
            gTriBufOffset
        )
        report "Conversion failed!"
        severity failure;

        wait;
    end process;

    theStim : process
        variable vByteAddr  : std_logic_vector(cAddressWidth-1 downto 0);
    begin
        done                <= cInactivated;

        portA.address       <= (others => cInactivated);
        portA.byteenable    <= (others => cInactivated);
        portA.read          <= cInactivated;
        portA.write         <= cInactivated;
        portA.writedata     <= (others => cInactivated);

        portB.address       <= (others => cInactivated);
        portB.byteenable    <= (others => cInactivated);
        portB.read          <= cInactivated;
        portB.write         <= cInactivated;
        portB.writedata     <= (others => cInactivated);
        wait until rst = cInactivated;
        wait until rising_edge(clk);

        portA.address       <= std_logic_vector(to_unsigned(0, cAddressWidth-2));
        portA.byteenable    <= "1111";
        portA.write         <= cActivated;
        portA.writedata     <= (others => cActivated);
        wait until rising_edge(clk);
        while portA.ack /= cActivated loop
            wait until rising_edge(clk);
        end loop;

        portA.write         <= cInactivated;

        for i in 0 to 2**cAddressWidth-1 loop
            portA.byteenable    <= "1111";
            vByteAddr           := std_logic_vector(to_unsigned(i, cAddressWidth));
            portA.address       <= vByteAddr(cAddressWidth-1 downto 2);
            portA.read          <= cActivated;
            wait until rising_edge(clk);
            while portA.ack /= cActivated loop
                wait until rising_edge(clk);
            end loop;
        end loop;

        portA.read          <= cInactivated;

        for i in cInputBase(0)/4 to (cInputBase(cInputBase'high)-4)/4 loop
            vByteAddr       := std_logic_vector(to_unsigned(i, cAddressWidth));
            portA.address       <= vByteAddr(cAddressWidth-2-1 downto 0);
            portA.writedata     <= std_logic_vector(to_unsigned(i, cDataWidth));
            portA.write         <= cActivated;
            wait until rising_edge(clk);
            while portA.ack /= cActivated loop
                wait until rising_edge(clk);
            end loop;
        end loop;

        portA.write             <= cInactivated;

        portA.address       <= std_logic_vector(to_unsigned(cInputBase(cInputBase'high)/4, cAddressWidth-2));
        portA.byteenable    <= "1111";
        portA.write         <= cActivated;
        portA.writedata     <= (others => cActivated);
        wait until rising_edge(clk);
        while portA.ack /= cActivated loop
            wait until rising_edge(clk);
        end loop;

        portA.write         <= cInactivated;

        done <= cActivated;
        wait;
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
