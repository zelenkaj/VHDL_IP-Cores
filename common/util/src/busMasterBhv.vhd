-------------------------------------------------------------------------------
--! @file busMasterBhv.vhd
--
--! @brief Behavioral design of the busMaster
--
--! @details This design is used to stimualte bus slaves with a stimulation file
--! in the .txt file format. This file contains instructions, which will be
--! interpreted by the busMasterBhv. The supported instructions are defined
--! in the busMasterPkg. One intstruction per line can be excecuted
--! per clock cycle! For further information refer to busMasterPkg.vhd
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

library work;
use work.global.all;
use work.busMasterPkg.all;

entity busMaster is
    generic (
        gAddrWidth      : integer := 32;
        gDataWidth      : integer := 32;
        gStimuliFile    : string := "name_TB_stim.txt"
    );
    port (
        iRst        : in std_logic;
        iClk        : in std_logic;
        iEnable     : in std_logic;
        iAck        : in std_logic;     -- slave says: on write- if have read data; on read- data are now valid!
        iReaddata   : in std_logic_vector(gDataWidth-1 downto 0);
        oWrite      : out std_logic;
        oRead       : out std_logic;
        oSelect     : out std_logic;
        oAddress    : out std_logic_vector(gAddrWidth-1 downto 0);
        oByteenable : out std_logic_vector(gDataWidth/8-1 downto 0);
        oWritedata  : out std_logic_vector(gDataWidth-1 downto 0);
        oError      : out std_logic;
        oDone       : out std_logic
    );
    begin
        assert gDataWidth = 32 report "other bit widths are not yet supported" &
            "by the package! Use a bus converter after the busMaster" &
            "to access slaves with smaller data widths!" severity error;

end busMaster;

architecture bhv of busMaster is
    --***********************************************************************--
    -- TYPES, RECORDS and CONSTANTS:
    --***********************************************************************--
    constant cRelativJumpRange  : natural := 63;
    constant cInitBusProtocol   : tBusProtocol :=(
        command     => s_UNDEF,
        memAccess   => s_UNDEF,
        address     => (others => '0'),
        value1      => (others => '0'),
        value2      => (others => '0')
    );

    type tInterpreterStates is (
        s_READOUT,
        s_INIT_REG,
        s_INSTR_FETCH,
        s_REGISTER,        -- sync. barrier
        s_WAIT,
        s_COMPARE,
        s_ERROR,
        s_FINISHED
    );

     type tRegSet is record
        writeEnable         : std_logic;
        readEnable          : std_logic;
        selectEnable        : std_logic;
        address             : std_logic_vector(gAddrWidth-1 downto 0);
        byteEnable          : std_logic_vector(gDataWidth/8-1 downto 0);
        writeData           : std_logic_vector(gDataWidth-1 downto 0);
        compareValue        : std_logic_vector(gDataWidth-1 downto 0);
        relativeJump        : natural;
        doneEnable          : std_logic;
        errorEnable         : std_logic;
        command             : tCommand;
        memAccess           : tMemoryAccess;
    end record tRegSet;

    constant cInitRegSet : tRegSet :=(
        writeEnable             => cInactivated,
        readEnable              => cInactivated,
        selectEnable            => cInactivated,
        address                 => (others => cInactivated),
        byteEnable              => (others => cInactivated),
        writeData               => (others => cInactivated),
        compareValue            => (others => cInactivated),
        relativeJump            => 0,
        doneEnable              => cInactivated,
        errorEnable             => cInactivated,
        command                 => s_UNDEF,
        memAccess               => s_UNDEF
        --busProtocol             => cInitBusProtocol
    );
    --***********************************************************************--
    -- SIGNALS:
    --***********************************************************************--
    file stimulifile            : text open read_mode is gStimuliFile;

    signal Reg, NextReg         : tRegSet;
    signal InterpreterState     : tInterpreterStates := s_READOUT;
    signal fsmTrigger           : std_logic := '0';

    shared variable vLine       : line;
    shared variable vReadString : string(1 to cMaxLineLength);
    shared variable vCmd        : tCommand;
    shared variable vJump       : boolean := FALSE;

begin

    FSM: process(InterpreterState, Reg, iEnable, iAck, iReaddata, fsmTrigger )
        variable vLine      : line;
        variable vAddress   : std_logic_vector(gAddrWidth-1 downto 0);
        variable vMemAccess : tMemoryAccess;
        variable vNrBytes   : natural;
    begin
        --default assignment:
         -- NextReg <= Reg;
        -- is not possible as the current signal driver would to be overwritten -> latches

        case InterpreterState is
            when s_READOUT =>

                if endfile(stimulifile) then        -- EOF
                    InterpreterState <= s_FINISHED;
                elsif iEnable = cActivated then     -- !EOF and iEnable
                    InterpreterState <= s_INIT_REG;
                    if vJump = TRUE then                   -- skip intructions
                        for i in 0 to Reg.relativeJump-1 loop
                            readline(stimulifile, vLine);
                            if endfile(stimulifile) then
                                InterpreterState <= s_ERROR;    -- file is not terminated by an FIN instruction
                                exit;
                            end if;
                        end loop;
                    else
                        readline(stimulifile, vLine);                   -- read instruction
                    end if;
                end if;

            when s_INIT_REG =>
                InterpreterState    <= s_INSTR_FETCH;

                vReadString := (others => ' ');
                read(vLine, vReadString(vLine'range));
                -- interpret vCmd:
                vCmd                := instruction2Command(vReadString);
                vJump               := FALSE;
                -- reset output register
                NextReg.address     <= (others => 'X');
                NextReg.writeData   <= (others => 'X');
                NextReg.byteEnable  <= (others => 'X');
                NextReg.compareValue<= (others => 'X');
                NextReg.memAccess   <= s_UNDEF;
                NextReg.command     <= s_UNDEF;
                NextReg.errorEnable <= cInactivated;
                NextReg.doneEnable  <= cInactivated;
                NextReg.writeEnable <= cInactivated;
                NextReg.readEnable  <= cInactivated;
                NextReg.selectEnable<= cInactivated;
                NextReg.relativeJump<= 0;

            when s_INSTR_FETCH  =>
                InterpreterState    <= s_READOUT;  -- read a new line!
                if vCmd /= s_UNDEF then
                    NextReg.command     <= vCmd;
                    vMemAccess          := instruction2MemAccess(vReadString);
                    NextReg.memAccess   <= vMemAccess;
                end if;

                case vCmd is
                    when s_WRITE =>
                        vAddress            := instruction2Value(vReadString, nrBytes2MemAccess(gDataWidth/8), 1);
                        NextReg.byteEnable  <= MemAccess2ByteEnable(vMemAccess, vAddress );
                        NextReg.address     <= vAddress;
                        NextReg.writeData   <= instruction2Value(vReadString, vMemAccess, 2);
                        NextReg.writeEnable <= cActivated;
                        NextReg.selectEnable<= cActivated;
                        InterpreterState    <= s_REGISTER;

                    when s_READ =>
                        vAddress            := instruction2Value(vReadString, nrBytes2MemAccess(gDataWidth/8), 1);
                        NextReg.byteEnable  <= MemAccess2ByteEnable(vMemAccess, vAddress );
                        NextReg.address     <= vAddress;
                        NextReg.readEnable  <= cActivated;
                        NextReg.selectEnable<= cActivated;
                        InterpreterState    <= s_REGISTER;

                    when s_JMPEQ | s_JMPNEQ | s_ASSERT =>
                        vAddress            := instruction2Value(vReadString, nrBytes2MemAccess(gDataWidth/8), 1);
                        NextReg.byteEnable  <= MemAccess2ByteEnable(vMemAccess, vAddress );
                        NextReg.address     <= vAddress;
                        NextReg.readEnable  <= cActivated;
                        NextReg.selectEnable<= cActivated;
                        NextReg.compareValue<= instruction2Value(vReadString, vMemAccess, 2);
                        InterpreterState    <= s_REGISTER;

                        if vCmd /= s_ASSERT then
                            NextReg.relativeJump <= to_integer(unsigned(instruction2Value(vReadString, nrBytes2MemAccess(gDataWidth/8), 3)));
                        end if;

                    when s_ERROR =>
                        InterpreterState    <= s_ERROR;
                        NextReg.errorEnable <= cActivated;

                    when s_FINISHED =>
                        InterpreterState    <= s_FINISHED;
                        NextReg.doneEnable  <= cActivated;

                    when s_NOP =>
                        InterpreterState    <= s_REGISTER;  -- wait explicit a cycle!

                    when s_UNDEF  =>

                    when others =>
                        assert false report "Undefined tCommand!" severity ERROR;

                end case;
                -- now the values of the NextReg should be stable

            when s_REGISTER =>
                    if fsmTrigger'event then
                        -- now stable values are registered
                        InterpreterState <= s_WAIT;
                    end if;

            when s_WAIT =>
                    if iAck = cActivated then
                        InterpreterState <= s_COMPARE; -- time to set Reg to correct value!!
                    elsif Reg.command = s_NOP then
                        InterpreterState <= s_READOUT;
                    end if;

            when s_COMPARE =>
                -- compare, if neccessary, actual with compare value!
                InterpreterState    <= s_READOUT;

                case Reg.command is
                    when s_JMPEQ  =>
                        vJump := compareReadValue(iReaddata, Reg.compareValue, Reg.byteEnable);
                    when s_JMPNEQ =>
                        if compareReadValue(iReaddata, Reg.compareValue, Reg.byteEnable) = FALSE  then
                            vJump := TRUE;
                        end if;
                    when s_ASSERT =>
                        if compareReadValue(iReaddata, Reg.compareValue, Reg.byteEnable) = FALSE then
                            InterpreterState <= s_ERROR;
                        end if;
                    when others   =>
                end case;

                if vJump = TRUE and Reg.relativeJump = 0 then
                    -- steady state:
                    InterpreterState <= s_FINISHED;
                end if;

            when s_ERROR        =>
                NextReg.errorEnable <= cActivated;
                InterpreterState    <= s_FINISHED;
            when s_FINISHED     =>
                NextReg.doneEnable <= cActivated;

            when others =>
                    assert false report "Undefined tInterpreterStates!" severity ERROR;
        end case; -- InterpreterState

    end process FSM;

    --***********************************************************************--
    -- REGISTERING:
    --***********************************************************************--
    Registering: process(iClk, iRst)
    begin
        if iRst = cActivated then
            Reg <= cInitRegSet;
        elsif iClk'event and iClk = cActivated then
            Reg <= NextReg;
            fsmTrigger <= not fsmTrigger;
        end if;
    end process Registering;

    --***********************************************************************--
    -- OUTPUT ASSIGNMENTS:
    --***********************************************************************--
    oWrite      <= Reg.writeEnable;
    oRead       <= Reg.readEnable;
    oSelect     <= Reg.selectEnable;
    oAddress    <= Reg.address;
    oByteenable <= Reg.byteEnable;
    oWritedata  <= Reg.writeData;
    oError      <= Reg.errorEnable;
    oDone       <= Reg.doneEnable;

end bhv;