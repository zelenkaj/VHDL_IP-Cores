-------------------------------------------------------------------------------
--! @file busMasterPkg.vhd
--
--! @brief Packapge for the busMaster
--
--! @details In this file the supported instructions of the busMasterBhv are
--! defined, as well as many important functions to parse and interpret the
--! commands.
--! Below there is an short overfiew of the command's structure and how they
--! affect.
--
-------------------------------------------------------------------------------
-- Packapge : busMasterPkg
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
use STD.textio.all;

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_textio.all;

--***********************************************************************--
-- DEFINITION OF THE INTERPRETER:
-- Implemented commands:
--     *key*    | *word*|   *syntax*
--  s_WRITE     | WR    |   cmd-access-address-value-(don'care)
--  s_READ      | RD    |   cmd-access-address-(don'care)
--  s_JMPEQ     | JEQ   |   cmd-access-address-value-rel.offset-(don'care)
--  s_JMPNEQ    | JNQ   |   cmd-access-address-value-rel.offset-(don'care)
--  s_ASSERT    | ASS   |   cmd-access-address-value-(don'care)
--  s_ERROR     | ERR   |   cmd-(don'care)
--  s_FINISHED  | FIN   |   cmd-(don'care)
--  s_NOP       | NOP   |   cmd-(don'care)
-- Memory access:
--  *key*   | *word*
--  BYTE    | b
--  WORD    | w
--  DWORD   | d
--
-- (hint: rel.offset = realtive offset from current instruction (positive natural!)
-- (hint: to add further instruction modify: tBusMasterStates,cInstruction2StateMap
--     and sStateIntstruction-FSM in busMasterBhv.vhd)
-- (hint: only one instruction per line is supported!)
--***********************************************************************--

package busMasterPkg is
    --***********************************************************************--
    -- TYPES, RECORDS and CONSTANTS:
    --***********************************************************************--

    constant cMaxLineLength : natural := 100;
    constant cMaxBitWidth   : natural := 32;

     type tCommand is (
        s_WRITE,     -- write some data on a specific address.
        s_READ,      -- reads some date from a specific address.
        s_JMPEQ,     -- compares to a value at a specific address and jumps
                     -- over the next instruction. value3 = relative jump forward, $ stay in line
        s_JMPNEQ,
        s_ASSERT,    -- reads some data from a specific address and compares it
                     -- with a specific value, if not equal -> _ERROR.
        s_ERROR,     -- sets the error flag.
        s_FINISHED,  -- sets the done flag.
        s_NOP,       -- no operation will be executed.
        s_UNDEF
     );
     ---------------------------------------------------------------------------
     type tMemoryAccess is (s_BYTE, s_WORD, s_DWORD, s_UNDEF);
     ---------------------------------------------------------------------------
     type tCommandKeyValue is record
        Key   : string(1 to 4);
        Value : tCommand;
     end record tCommandKeyValue;
    ---------------------------------------------------------------------------
     type tCommand2CmdMap is array(natural range <>) of tCommandKeyValue;
    ---------------------------------------------------------------------------
     constant cInstruction2CmdMap : tCommand2CmdMap := (
                0=> ( Key => "WR  ", Value => s_WRITE ),
                1=> ( Key => "RD  ", Value => s_READ ),
                2=> ( Key => "JEQ ", Value => s_JMPEQ ),
                3=> ( Key => "JNQ ", Value => s_JMPNEQ ),
                4=> ( Key => "ASS ", Value => s_ASSERT ),
                5=> ( Key => "ERR ", Value => s_ERROR ),
                6=> ( Key => "FIN ", Value => s_FINISHED ),
                7=> ( Key => "NOP ", Value => s_NOP )
     );
    ---------------------------------------------------------------------------
     type tAccessKeyValue is record
        Key : string(1 to 2);
        Value : tMemoryAccess;
        nrBytes : natural;
     end record;
     ---------------------------------------------------------------------------
    type tCommand2AccessMap is array(natural range <>) of tAccessKeyValue;
    ---------------------------------------------------------------------------
     constant cInstruction2AccessMap : tCommand2AccessMap := (
                0=> ( Key => "b ", Value => s_BYTE  , nrBytes => 1),
                1=> ( Key => "w ", Value => s_WORD  , nrBytes => 2),
                2=> ( Key => "d ", Value => s_DWORD , nrBytes => 4)
     );

    ---------------------------------------------------------------------------
    type tBusProtocol is record
        command     : tCommand;
        memAccess   : tMemoryAccess;
        address     : std_logic_vector(31 downto 0);
        value1      : std_logic_vector(31 downto 0);
        value2      : std_logic_vector(31 downto 0);
    end record tBusProtocol;

    type tIndexPosition is (
        s_BEGIN,
        s_END
    );
    --***********************************************************************--
    -- FUNCTION DECLARATIONS:
    --***********************************************************************--

    -- elements are separeted by one or more wide spaces!
    function getIndexOfElement(str : string; nrElement : natural; position : tIndexPosition) return natural;
    function strcmp(dest : string; src : string) return boolean;
    ---------------------------------------------------------------------------
    function instruction2Command(instr : string) return tCommand;
    function instruction2MemAccess(instr  : string) return tMemoryAccess;
    function MemAccess2ByteEnable(memAccess : tMemoryAccess; addr : std_logic_vector) return std_logic_vector;
    function instruction2Value(instr : string; dataType : tMemoryAccess; nrValue : natural) return std_logic_vector;
    function MemAccess2nrBytes(memAccess : tMemoryAccess) return natural;
    function nrBytes2MemAccess( nrBytes : natural) return tMemoryAccess;
    function compareReadValue(dest :std_logic_vector; src : std_logic_vector; byteEnable : std_logic_vector) return boolean;

end package busMasterPkg;

package body busMasterPkg is
    -- package body from std_logic_textio:
    -- Hex Read and Write procedures.
    procedure Char2QuadBits(C: Character;
                RESULT: out Bit_Vector(3 downto 0);
                GOOD: out Boolean;
                ISSUE_ERROR: in Boolean) is
    begin
        case c is
            when '0' => result :=  x"0"; good := TRUE;
            when '1' => result :=  x"1"; good := TRUE;
            when '2' => result :=  x"2"; good := TRUE;
            when '3' => result :=  x"3"; good := TRUE;
            when '4' => result :=  x"4"; good := TRUE;
            when '5' => result :=  x"5"; good := TRUE;
            when '6' => result :=  x"6"; good := TRUE;
            when '7' => result :=  x"7"; good := TRUE;
            when '8' => result :=  x"8"; good := TRUE;
            when '9' => result :=  x"9"; good := TRUE;
            when 'A' => result :=  x"A"; good := TRUE;
            when 'B' => result :=  x"B"; good := TRUE;
            when 'C' => result :=  x"C"; good := TRUE;
            when 'D' => result :=  x"D"; good := TRUE;
            when 'E' => result :=  x"E"; good := TRUE;
            when 'F' => result :=  x"F"; good := TRUE;

            when 'a' => result :=  x"A"; good := TRUE;
            when 'b' => result :=  x"B"; good := TRUE;
            when 'c' => result :=  x"C"; good := TRUE;
            when 'd' => result :=  x"D"; good := TRUE;
            when 'e' => result :=  x"E"; good := TRUE;
            when 'f' => result :=  x"F"; good := TRUE;
            when others =>
               if ISSUE_ERROR then
                   assert FALSE report
                    "HREAD Error: Read a '" & c &
                       "', expected a Hex character (0-F).";
               end if;
               good := FALSE;
        end case;
    end;

    --------------------------------------------------------------------------
    function getIndexOfElement(str : string; nrElement : natural; position : tIndexPosition) return natural is
        variable vPos, vCounter, vHigh, vLow        : natural;
    begin
        vCounter := 0;
        vPos := 0;
        vHigh := str'right;
        vLow := str'left;
        if nrElement = 0 then
            return vPos;
        elsif position = s_BEGIN then
            -- corner case
            if nrElement = 1 and str(1) /= ' ' then
                return 1;
            end if;
            for i in vLow to vHigh-1 loop

                if str(i) = ' ' and str(i+1) /= ' ' then
                    vCounter := vCounter + 1;
                end if;
                vPos := i;
                exit when (vCounter = nrElement-1);
            end loop;
            vPos := vPos+1;
        elsif position = s_END then

            for i in vLow+1 to vHigh loop
                if str(i) = ' ' and str(i-1) /= ' ' then
                    vCounter := vCounter + 1;
                end if;
                vPos := i;
                exit when (vCounter = nrElement);
            end loop;
            vPos := vPos - 1;
        end if;
        return vPos;
    end getIndexOfElement;

    ---------------------------------------------------------------------------
    function strcmp(dest : string; src : string) return boolean is
    begin
        if( src'right > dest'right ) then
            return FALSE;
        end if;
        for i in src'range loop

            -- ignore wide spaces in the src -> specific for cInstruction2CmdMap
            if ( dest(i) /= src(i) ) then -- src(i) /= ' ' and
                return FALSE;
            end if;

        end loop;
        return TRUE;
    end strcmp;

    ---------------------------------------------------------------------------
    function instruction2Command(instr : string) return tCommand is
        variable vCmd : tCommand;
        variable vIndex_1, vIndex_2 : natural;
        variable vFoundString : string(1 to cMaxLineLength);
    begin
        vFoundString := (others => ' ');
        vCmd := s_UNDEF;
        vIndex_1 := getIndexOfElement(instr, 1, s_BEGIN );
        vIndex_2 := getIndexOfElement(instr, 1, s_END );
        vFoundString(1 to (vIndex_2-vIndex_1)+1) := instr(vIndex_1 to vIndex_2);

        for i in cInstruction2CmdMap'range loop
            if strcmp(vFoundString, cInstruction2CmdMap(i).Key) then
                return cInstruction2CmdMap(i).Value;
            end if;
        end loop;

        return s_UNDEF;
    end function instruction2Command;

    ---------------------------------------------------------------------------
    function instruction2MemAccess(instr  : string) return tMemoryAccess is
        variable vMemAccess : tMemoryAccess;
        variable vIndex_1, vIndex_2 : natural;
        variable vFoundString : string(1 to cMaxLineLength);
    begin
        vFoundString := (others => ' ');
        vMemAccess := s_UNDEF;
        vIndex_1 := getIndexOfElement(instr, 2, s_BEGIN);
        vIndex_2 := getIndexOfElement(instr, 2, s_END);
        vFoundString(1 to (vIndex_2-vIndex_1)+1) := instr(vIndex_1 to vIndex_2);

        for i in cInstruction2AccessMap'range loop
            if strcmp(vFoundString, cInstruction2AccessMap(i).Key) then
                return cInstruction2AccessMap(i).Value;
            end if;
        end loop;

        return vMemAccess;
    end function instruction2MemAccess;

    ---------------------------------------------------------------------------
     function MemAccess2ByteEnable(memAccess : tMemoryAccess; addr : std_logic_vector ) return std_logic_vector is
        variable vByteEnalbe : std_logic_vector(3 downto 0);
     begin
        case memAccess is
            when s_DWORD =>
                vByteEnalbe := "1111";
            when s_WORD =>
                if addr(1) = '0' then
                    vByteEnalbe := "0011";
                elsif addr(1) = '1' then
                    vByteEnalbe := "1100";
                end if;
            when s_BYTE =>
                -- this section can be replaced by a std shift left operation...
                if addr(1 downto 0) = "00" then
                    vByteEnalbe := "0001";
                elsif addr(1 downto 0) = "01" then
                    vByteEnalbe := "0010";
                elsif addr(1 downto 0) = "10" then
                    vByteEnalbe := "0100";
                elsif addr(1 downto 0) = "11" then
                    vByteEnalbe := "1000";
                end if;
            when others =>
               vByteEnalbe := "----";
        end case;

        return vByteEnalbe;
     end MemAccess2ByteEnable;

     ---------------------------------------------------------------------------
     function instruction2Value(instr : string; dataType : tMemoryAccess; nrValue : natural) return std_logic_vector is
        variable vValue : std_logic_vector(cMaxBitWidth-1 downto 0);
        variable vResult : Bit_Vector(3 downto 0);
        variable vIndex_1, vIndex_2, nrBytes : natural;
        variable isGood : boolean;
     begin
        vValue  := (others => 'X');
        vIndex_1 := getIndexOfElement(instr, nrValue+2, s_BEGIN);
        vIndex_2 := getIndexOfElement(instr, nrValue+2, s_END);

        nrBytes := MemAccess2nrBytes(dataType);

        if (vIndex_2 - vIndex_1) > ((cMaxBitWidth/4)-1) then   -- limited to 4*8 Bits!
            return vValue;
        end if;

        for i in 0 to vIndex_2-vIndex_1 loop
            Char2QuadBits( instr(vIndex_2-i), vResult, isGood, FALSE );
            if i > (nrBytes*2-1) then
                vValue( (i+1)*4 -1 downto i*4) :=  "XXXX";
            else
                if isGood then
                    vValue( (i+1)*4 -1 downto i*4) :=  to_stdlogicvector(vResult);
                else
                    vValue( (i+1)*4 -1 downto i*4) :=  "----";
                end if;
            end if;
        end loop;

        return vValue;
     end instruction2Value;

     ---------------------------------------------------------------------------
     function MemAccess2nrBytes(memAccess : tMemoryAccess) return natural is
     begin
        for i in cInstruction2AccessMap'range loop
            if cInstruction2AccessMap(i).Value = memAccess then
                return cInstruction2AccessMap(i).nrBytes;
            end if;
        end loop;
        return 0;

     end MemAccess2nrBytes;

     ---------------------------------------------------------------------------
     function nrBytes2MemAccess( nrBytes : natural) return tMemoryAccess is
     begin
        for i in cInstruction2AccessMap'range loop
            if cInstruction2AccessMap(i).nrBytes = nrBytes then
                return cInstruction2AccessMap(i).value;
            end if;
        end loop;
        return s_UNDEF;

     end nrBytes2MemAccess;

    ---------------------------------------------------------------------------
    function compareReadValue(dest : std_logic_vector; src : std_logic_vector; byteEnable : std_logic_vector) return boolean is
        variable result : boolean := FALSE;
        variable nrBytes : natural := 0;
    begin
            result := TRUE;
            for j in byteEnable'range loop
                if byteEnable(j) = '1' then
                    nrBytes := nrBytes + 1;
                end if;
            end loop;
            for i in 0 to nrBytes-1 loop
                if ( dest((i+1)*8-1 downto i*8) /= src((i+1)*8-1 downto i*8) ) then
                    result := FALSE;
                end if;
            end loop;
        return result;
    end compareReadValue;

end package body busMasterPkg;
