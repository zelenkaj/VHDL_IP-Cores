-------------------------------------------------------------------------------
--! @file parallelMaster-bhv-ea.vhd
--
--! @brief Parallel Master Model
--
--! @details This model will convert avalon master interface signals to Parallel
--! Master interface signals. This model wont use any timing for generate
--! signals.
-------------------------------------------------------------------------------
--
--    (c) B&R, 2012
--    (c) Kalycito Infotech Pvt Ltd
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
-- Version History
-------------------------------------------------------------------------------
-- 2014-01-13   Vinod PA    Added Parallel master Model
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_signed.all;
library work;
--! use global library
use work.global.all;
--! use host interface package for specific types
use work.hostInterfacePkg.all;

entity parallelMaster is
generic (
    --! Data bus width
    gDataWidth  : natural := 16;
    --! Address and Data bus are multiplexed (0 = FALSE, otherwise = TRUE)
    gMultiplex  : natural := 0
 );

port (
   --! Avalon Address
   iAvalonAddr                 : in  std_logic_vector (31 downto 0);
   --! Avalon Byte Enable
   iAvalonBE                   : in  std_logic_vector (3 downto 0);
   --! Avalon Read Enable
   iAvalonRead                 : in  std_logic;
   --! Avalon Write enable
   iAvalonWrite                : in  std_logic;
   --! Avlon Write Data
   iAvalonWriteData            : in  std_logic_vector (31 downto 0);
   --! Avalon Read Data
   oAvalonReadData             : out std_logic_vector (31 downto 0);
   --! Avalon Read Data Valid Signal
   oAvalonReadValid            : out std_logic;
   --! Avalon Wait request
   oAvalonWaitReq              : out std_logic;
   -- Parallel Interface
   --! Chipselect
   oParHostChipselect          : out std_logic := cInactivated;
   --! Read strobe
   oParHostRead                : out std_logic := cInactivated;
   --! Write strobe
   oParHostWrite               : out std_logic := cInactivated;
   --! Address Latch enable (Multiplexed only)
   oParHostAddressLatchEnable  : out std_logic := cInactivated;
   --! High active Acknowledge
   iParHostAcknowledge         : in std_logic := cInactivated;
   --! Byteenables
   oParHostByteenable          : out
      std_logic_vector(gDataWidth/cByte-1 downto 0) := (others => cInactivated);
   --! Address bus (Demultiplexed, word-address)
   oParHostAddress             : out
      std_logic_vector(15 downto 0) := (others => cInactivated);
   --! Data bus out (Demultiplexed)
   iParHostData                : in
      std_logic_vector(gDataWidth-1 downto 0) := (others => cInactivated);
   --! Data bus in (Demultiplexed)
   oParHostData                : out
      std_logic_vector(gDataWidth-1 downto 0) := (others => cInactivated);
   --! Data bus outenable (Demultiplexed)
   iParHostDataEnable          : in std_logic;
   --! Address/Data bus out (Multiplexed, word-address))
   iParHostAddressData         : in
      std_logic_vector(gDataWidth-1 downto 0) := (others => cInactivated);
   --! Address/Data bus in (Multiplexed, word-address))
   oParHostAddressData         : out
      std_logic_vector(gDataWidth-1 downto 0) := (others => cInactivated);
   --! Address/Data bus outenable (Multiplexed, word-address))
   iParHostAddressDataEnable   : in std_logic;
   -- Clock/Reset sources
   --! Clock Source input
   iClk                        : in std_logic:= cInactivated;
   --! Reset Source input
   iRst                        : in std_logic:= cInactivated
 );
end parallelMaster;

architecture bhv of parallelMaster is

--! State for FSM
type tState  is (
                 sINIT,
                 sWRITE,
                 sREAD,
                 sIDLE,
                 sSTOP
                );
--! Current FSM State
signal  tStateCurrent      :    tState      ;
--! Next FSM State
signal  tStateNext         :    tState      ;
--SIGNALS
--! Latched Address
signal latchaddress        : std_logic_vector(gDataWidth-1 downto 0) ;
--!latched Data
signal latchdata           : std_logic_vector(gDataWidth-1 downto 0) ;
--! Temp Storage of Latch Address
signal latchaddress_temp   : std_logic_vector(gDataWidth-1 downto 0) ;
--! Temp storage of Latch Data
signal latchdata_temp      : std_logic_vector(gDataWidth-1 downto 0) ;

--!DWord Read or Write Operations
signal DwordRdWr            : std_logic:= cInactivated;
--! DWord Read or Write Operations
signal DwordRdWr_next       : std_logic:= cInactivated;
--! Wait/hold count for write/read operations
signal Count                : std_logic_vector (31 downto 0) := x"00000000";
signal Count_next           : std_logic_vector (31 downto 0) := x"00000000";
--! Lower Address bits
signal AddrL                : std_logic_vector (15 downto 0) := x"0000" ;
--! Higer Address bits
signal AddrM                : std_logic_vector (15 downto 0) := x"0000" ;
--! Lower ReadData Bits
signal RDataL               : std_logic_vector (15 downto 0) := x"0000" ;
--! Higher ReadData Bits
signal RDataM               : std_logic_vector (15 downto 0) := x"0000" ;
--! Lower Write Data Bits
signal WDataL               : std_logic_vector (15 downto 0) := x"0000" ;
--! Higher Write Data Bits
signal WDataM               : std_logic_vector (15 downto 0) := x"0000" ;
--! Lower Byte Enable bits
signal BeL                  : std_logic_vector (1 downto 0) := "00" ;
--! Higher Byte Enable Bits
signal BeM                  : std_logic_vector (1 downto 0) := "00" ;
--! Delay Time Count signal
signal countT : std_logic_vector (3 downto 0) := "0000" ;
begin

latchaddress        <= x"00ee";
latchdata           <= x"1234";
latchaddress_temp   <= latchaddress;
latchdata_temp      <= latchdata;

AddrL               <= iAvalonAddr (15 downto 0) ;
AddrM               <= iAvalonAddr (31 downto 16) ;

WDataL              <= iAvalonWriteData (15 downto 0);
WDataM              <= iAvalonWriteData (31 downto 16);

BeL                 <= iAvalonBE (1 downto 0) ;
BeM                 <= iAvalonBE (3 downto 2) ;

-- Master FSM
--! Process for Sequenctial Logics
SeqFSM:process (iClk)
begin
 if rising_edge (iClk) then
  if(iRst = cActivated ) then
    tStateCurrent <= sINIT    ;
    DwordRdWr    <= cInactivated ;
    Count        <= x"00000000" ;
  else
    tStateCurrent <= tStateNext ;
    DwordRdWr    <= DwordRdWr_next ;
    Count        <= Count_next;
  end if;
 end if;
end process;
--! Combinational Logics for FSM
CombFSM:process (
               iAvalonRead,
               iAvalonWrite,
               iAvalonAddr,
               iAvalonBE,
               iAvalonWriteData,
               tStateCurrent,
               iParHostAcknowledge,
               iParHostData,
               iParHostDataEnable,
               iParHostAddressData,
               iParHostAddressDataEnable,
               DwordRdWr,
               latchaddress_temp,
               latchdata_temp,
               BeM,
               BeL,
               AddrL,
               latchaddress,
               latchdata,
               Count,
               WDataM,
               WDataL
            )
    begin
        tStateNext <= tStateCurrent ;
        Count_next <= Count ;
        case (tStateCurrent) is
         when sINIT =>
                    oParHostChipselect  <= cInactivated;
                    oParHostRead        <= cInactivated;
                    oParHostWrite       <= cInactivated;
                    oParHostAddress     <= iAvalonAddr (15 downto 0);
                    oParHostByteenable  <= "00";
                    oParHostData        <= x"0000";
                    oParHostChipselect  <= cInactivated;
                    oParHostWrite       <= cInactivated;
                    oParHostAddressData <= iAvalonAddr (15 downto 0);
                    oParHostAddressLatchEnable <= cInactivated;

                    countT <= x"0" ;
                    if (iAvalonBE = x"F" ) then
                        DwordRdWr_next <= cActivated ;
                    else
                        DwordRdWr_next <= cInactivated ;
                    end if;

                    if(iAvalonRead = cActivated) then
                     tStateNext       <= sREAD ;
                     oAvalonWaitReq   <= cActivated ;
                    elsif  (iAvalonWrite = cActivated) then
                     tStateNext       <= sWRITE ;
                     oAvalonWaitReq   <= cActivated ;
                    else
                     tStateNext       <= sINIT ;
                     oAvalonWaitReq   <= cInactivated ;
                    end if ;


         when sWRITE =>
                    if(gMultiplex = cFalse) then
                        oParHostChipselect <= cActivated;
                        oParHostWrite      <= cActivated;

                        if(DwordRdWr = cInactivated) then
                           oParHostAddress    <= AddrL;
                           oParHostByteenable <= BeL;
                           oParHostData       <= WDataL;
                         else
                           oParHostAddress    <= AddrL + x"0001";
                           oParHostByteenable <= BeM;
                           oParHostData       <= WDataM;
                         end if;
                        --Keep it same for atleast 3 clock cycles
                        if( countT = x"2") then
                            if(iParHostAcknowledge = cActivated) then
                                countT  <= x"0" ;
                                tStateNext  <= sIDLE ;
                            else
                                tStateNext  <= sWRITE ;
                            end if;
                        else
                            tStateNext  <= sWRITE ;
                            countT  <=
                                     std_logic_vector(unsigned(countT) + 1);
                        end if;
                    --TODO: Cross check this part again
                    elsif (gMultiplex = cTrue) then
                        oParHostChipselect         <= cActivated;
                        oParHostWrite              <= cActivated;
                        oParHostAddressLatchEnable <= cActivated;

                        if (latchaddress_temp = latchaddress) then
                         oParHostAddressData        <= latchaddress;
                         oParHostAddressLatchEnable <= cInactivated;
                        end if;

                        if (latchdata_temp = latchdata) then
                         oParHostAddressData        <= latchdata;
                         oParHostAddressLatchEnable <= cInactivated;
                        end if;

                        if(iParHostAcknowledge = cActivated) then
                            tStateNext <= sIDLE ;
                        else
                            tStateNext <= sWRITE ;
                        end if;
                    end if;

        when sIDLE  =>
                    oParHostChipselect         <= cInactivated;
                    oParHostWrite              <= cInactivated;
                    oParHostAddress            <= iAvalonAddr (15 downto 0);
                    oParHostByteenable         <= "00";
                    oParHostData               <= x"0000";
                    oParHostChipselect         <= cInactivated;
                    oParHostWrite              <= cInactivated;
                    oParHostAddressData        <= iAvalonAddr (15 downto 0);
                    oParHostAddressLatchEnable <= cInactivated;

                    if(DwordRdWr = cInactivated) then
                        tStateNext      <= sINIT ;
                        oAvalonWaitReq  <= cInactivated ;
                    else
                        tStateNext      <= sWRITE ;
                        DwordRdWr_next  <= cInactivated ;
                    end if;

         when sREAD  =>
                    if(gMultiplex = cFalse) then
                        oParHostChipselect <= cActivated;
                        oParHostRead       <= cActivated;

                        if(DwordRdWr = cInactivated) then
                           oParHostAddress  <= AddrL;

                           if (iAvalonBE = x"C" ) then
                             oParHostByteenable  <= BeM;
                             RDataM              <= iParHostData;
                           else
                              RDataL             <= iParHostData;
                              oParHostByteenable <= BeL;
                           end if;
                         else
                           oParHostAddress    <= AddrL + x"0001";
                           oParHostByteenable <= BeM;
                           RDataM             <= iParHostData;
                         end if;

                        if( countT = x"2") then
                          if(iParHostAcknowledge = cActivated and
                            (not iParHostDataEnable = cActivated)) then

                            countT <= x"0" ;
                            tStateNext <= sSTOP ;
                          else
                            tStateNext <= sREAD ;
                          end if;
                       else
                            tStateNext <= sREAD ;
                            countT <=
                                    std_logic_vector(unsigned(countT) + 1);
                        end if;

                    --TODO: Cross check this part again
                    elsif (gMultiplex = cTrue) then
                        oParHostChipselect         <= cActivated ;
                        oParHostRead               <= cActivated ;
                        oParHostAddressData        <= x"00ee";
                        oParHostAddressLatchEnable <= cActivated;
                        if(iParHostAcknowledge = cActivated and
                           iParHostAddressDataEnable = cActivated) then
                            tStateNext             <= sStop ;
                        else
                            tStateNext             <= sREAD ;
                        end if;
                    end if;

         when sSTOP =>
                    oParHostChipselect         <= cInactivated;
                    oParHostRead               <= cInactivated;
                    oParHostWrite              <= cInactivated;
                    oParHostAddress            <= x"0000";
                    oParHostByteenable         <= "00";
                    oParHostData               <= x"0000";
                    oParHostChipselect         <= cInactivated;
                    oParHostWrite              <= cInactivated;
                    oParHostAddressData        <= x"0000";
                    oParHostAddressLatchEnable <= cInactivated;
                    Count_next                 <= Count +  x"00000001" ;

                    if(DwordRdWr = cInactivated) then
                        tStateNext     <= sINIT ;
                        oAvalonWaitReq <= cInactivated ;
                    else
                        tStateNext     <= sREAD ;
                        DwordRdWr_next <= cInactivated ;
                    end if;

         when others => null;
        end case;
    end process;

oAvalonReadData     <=  RDataM & RDataL ;

end bhv;
