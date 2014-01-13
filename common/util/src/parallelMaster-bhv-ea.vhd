library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.std_logic_signed.all;
--use ieee.std_logic_unsigned.all;
--use ieee.arith.all ;
--! use global library
use work.global.all;
--! use host interface package for specific types
use work.hostInterfacePkg.all;

entity parallel_master is
    generic (
        --! Data bus width
        gDataWidth  : natural := 16;
        --! Address and Data bus are multiplexed (0 = FALSE, otherwise = TRUE)
        gMultiplex  : natural := 0
    );

port (

        -- Avalon Interface
       iAvalonAddr      :   in std_logic_vector    (31 downto 0)   ;
       iAvalonBE   :   in std_logic_vector    (3 downto 0)    ;
       iAvalonRead         :   in std_logic                         ;
       iAvalonWrite        :   in std_logic                         ;
       iAvalonWriteData    :   in std_logic_vector   (31 downto 0)  ;
       oAvalonReadData     :   out  std_logic_vector   (31 downto 0);
       oAvalonReadValid    :  out std_logic ;
       oAvalonWaitReq  :   out  std_logic                       ;
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
       oParHostByteenable          : out std_logic_vector(gDataWidth/cByte-1 downto 0) := (others => cInactivated);
       --! Address bus (Demultiplexed, word-address)
       oParHostAddress             : out std_logic_vector(15 downto 0) := (others => cInactivated);
       --! Data bus out (Demultiplexed)
       iParHostData                : in std_logic_vector(gDataWidth-1 downto 0) := (others => cInactivated);
       --! Data bus in (Demultiplexed)
       oParHostData                : out std_logic_vector(gDataWidth-1 downto 0) := (others => cInactivated);
       --! Data bus outenable (Demultiplexed)
       iParHostDataEnable          : in std_logic;
       --! Address/Data bus out (Multiplexed, word-address))
       iParHostAddressData         : in std_logic_vector(gDataWidth-1 downto 0) := (others => cInactivated);
       --! Address/Data bus in (Multiplexed, word-address))
       oParHostAddressData         : out std_logic_vector(gDataWidth-1 downto 0) := (others => cInactivated);
       --! Address/Data bus outenable (Multiplexed, word-address))
       iParHostAddressDataEnable   : in std_logic;
       -- Clock/Reset sources
       --! Clock Source input
       iClk                        : in std_logic:= cInactivated;
       --! Reset Source input
       iRst                        : in std_logic:= cInactivated

    );
end parallel_master;

architecture Bhv of parallel_master is

type state  is (sINIT,sWRITE,sREAD,sIDLE,sSTOP);
signal  StateCurrent    :    state      ;
signal  StateNext       :    state      ;
--SIGNALS
signal latchaddress :  std_logic_vector(gDataWidth-1 downto 0) ;
signal latchdata :  std_logic_vector(gDataWidth-1 downto 0) ;
signal temp,temp1 : std_logic_vector(gDataWidth-1 downto 0) ;

--signal noReadWrite : std_logic_vector (31 downto 0):= x"00000000";
signal DwordRdWr : std_logic:= '0';
signal nDwordRdWr : std_logic:= '0';
signal Count     : std_logic_vector (31 downto 0) := x"00000000";
signal AddrL : std_logic_vector (15 downto 0) := x"0000" ;
signal AddrM : std_logic_vector (15 downto 0) := x"0000" ;
signal RDataL : std_logic_vector (15 downto 0) := x"0000" ;
signal RDataM : std_logic_vector (15 downto 0) := x"0000" ;

signal WDataL : std_logic_vector (15 downto 0) := x"0000" ;
signal WDataM : std_logic_vector (15 downto 0) := x"0000" ;
signal BeL : std_logic_vector (1 downto 0) := "00" ;
signal BeM : std_logic_vector (1 downto 0) := "00" ;

signal countT : std_logic_vector (3 downto 0) := "0000" ;


begin
latchaddress <= x"00ee";
latchdata<=x"1234";
temp <= latchaddress;
temp1 <= latchdata;

-- Handling Word/Dword Read Operations
--DwordRdWr <= '1' when iAvalonBE = x"F" else
--             '0' ;
--
AddrL     <= iAvalonAddr (15 downto 0) ;
AddrM     <= iAvalonAddr (31 downto 16) ;

WDataL    <= iAvalonWriteData (15 downto 0);
WDataM    <= iAvalonWriteData (31 downto 16);

BeL       <= iAvalonBE (1 downto 0) ;
BeM       <= iAvalonBE (3 downto 2) ;

--noReadWrite <= x"00000010" ;
-- Master FSM
-- Sequenctial Logics
process (iClk,iRst)
begin
 if rising_edge (iClk) then
  if(iRst = '1') then
    StateCurrent <= sINIT    ;
    DwordRdWr    <= '0' ;
  else
    StateCurrent <= StateNext ;
    DwordRdWr    <= nDwordRdWr ;
  end if;
 end if;
end process;
-- Combinational Logics
    process (
               iAvalonRead,
               iAvalonWrite,
               iAvalonAddr,
               iAvalonBE,
               iAvalonWriteData,
               StateCurrent,
               iParHostAcknowledge,
               iParHostData,
               iParHostDataEnable,
               iParHostAddressData,
               iParHostAddressDataEnable,
               DwordRdWr,
               temp,
               temp1

            )
    begin
        StateNext <= StateCurrent ;
        case (StateCurrent) is
         when sINIT =>
                    oParHostChipselect <= '0';
                    oParHostRead <= '0';
                    oParHostWrite <= '0';
                    oParHostAddress <= iAvalonAddr (15 downto 0);
                    oParHostByteenable <= "00";
                    oParHostData <= x"0000";
                    oParHostChipselect <= '0';
                    oParHostWrite <= '0';
                    oParHostAddressData <= iAvalonAddr (15 downto 0);
                    oParHostAddressLatchEnable <= '0';

                          countT <= x"0" ;

                    if (iAvalonBE = x"F" ) then
                        nDwordRdWr <= '1' ;
                          else
                        nDwordRdWr <= '0' ;
                    end if;

                    if(iAvalonRead = '1') then
                    StateNext <= sREAD ;
                    oAvalonWaitReq <= '1' ;
                    elsif  (iAvalonWrite = '1') then
                    StateNext <= sWRITE ;
                    oAvalonWaitReq <= '1' ;
                    else
                    StateNext <= sINIT ;
                    oAvalonWaitReq <= '0' ;
                    end if ;


         when sWRITE =>
                    if(gMultiplex = 0) then
                        oParHostChipselect <= '1';
                        oParHostWrite <= '1';

                        if(DwordRdWr = '0') then
                           oParHostAddress <= AddrL;
                           oParHostByteenable <= BeL;
                           oParHostData <= WDataL;
                         else
                           oParHostAddress <= AddrL + x"0001";
                           oParHostByteenable <= BeM;
                           oParHostData <= WDataM;
                         end if;
                                 --Keep it same for atleast 3 clock cycles
                                 if( countT = x"3") then
                                    if(iParHostAcknowledge = '1') then
                                        countT <= x"0" ;
                                        StateNext <= sIDLE ;
                                    else
                                        StateNext <= sWRITE ;
                                    end if;
                                else
                                    StateNext <= sWRITE ;
                                    countT <= std_logic_vector(unsigned(countT) + 1);
                                end if;
                    --TODO: Cross check this part again
                    elsif (gMultiplex = 1) then
                        oParHostChipselect <= '1';
                        oParHostWrite <= '1';
                        oParHostAddressLatchEnable <= '1';
                        if (temp = latchaddress) then
                        oParHostAddressData <= latchaddress;
                        oParHostAddressLatchEnable <= '0';
                        end if;
                        if (temp1 = latchdata) then
                        oParHostAddressData <= latchdata;
                        oParHostAddressLatchEnable <= '0';
                        end if;

                        if(iParHostAcknowledge = '1') then
                            StateNext <= sIDLE ;
                        else
                            StateNext <= sWRITE ;
                        end if;
                    end if;

        when sIDLE  =>
                    oParHostChipselect <= '0';
                    oParHostWrite <= '0';
                    oParHostAddress <= iAvalonAddr (15 downto 0);
                    oParHostByteenable <= "00";
                    oParHostData <= x"0000";
                    oParHostChipselect <= '0';
                    oParHostWrite <= '0';
                    oParHostAddressData <= iAvalonAddr (15 downto 0);
                    oParHostAddressLatchEnable <= '0';

                    if(DwordRdWr = '0') then
                        StateNext <= sINIT ;
                        oAvalonWaitReq <= '0' ;
                    else
                        StateNext <= sWRITE ;
                        nDwordRdWr <= '0' ;
                    end if;

         when sREAD  =>
                    if(gMultiplex = 0) then
                        oParHostChipselect <= '1';
                        oParHostRead <= '1';

                        if(DwordRdWr = '0') then
                           oParHostAddress <= AddrL;
                                    if (iAvalonBE = x"C" ) then
                             oParHostByteenable <= BeM;
                             RDataM <= iParHostData;
                                    else
                                      RDataL <= iParHostData;
                                      oParHostByteenable <= BeL;
                                    end if;
                         else
                           oParHostAddress <= AddrL + x"0001";
                           oParHostByteenable <= BeM;
                           RDataM <= iParHostData;
                         end if;

                                if( countT = x"3") then
                          if(iParHostAcknowledge = '1' and (not iParHostDataEnable = '1')) then
                          --if(iParHostAcknowledge = '1') then
                                    countT <= x"0" ;
                            StateNext <= sSTOP ;
                          else
                            StateNext <= sREAD ;
                          end if;
                               else
                                    StateNext <= sREAD ;
                                    countT <= std_logic_vector(unsigned(countT) + 1);
                                end if;


                    --TODO: Cross check this part again
                    elsif (gMultiplex = 1) then
                        oParHostChipselect <= '1' after 20 ns;
                        oParHostRead <= '1';
                        oParHostAddressData <= x"00ee";
                        oParHostAddressLatchEnable <= '1';
                        if(iParHostAcknowledge = '1' and iParHostAddressDataEnable = '1') then
                        StateNext <= sStop ;
                        else
                        StateNext <= sREAD ;
                        end if;
                    end if;

         when sSTOP =>
                    oParHostChipselect <= '0';
                    oParHostRead <= '0';
                    oParHostWrite <= '0';
                    oParHostAddress <= x"0000";
                    oParHostByteenable <= "00";
                    oParHostData <= x"0000";
                    oParHostChipselect <= '0';
                    oParHostWrite <= '0';
                    oParHostAddressData <= x"0000";
                    oParHostAddressLatchEnable <= '0';
                    Count <= Count +  x"00000001" ;

                    if(DwordRdWr = '0') then
                        StateNext <= sINIT ;
                        oAvalonWaitReq <= '0' ;
                    else
                        StateNext <= sREAD ;
                        nDwordRdWr <= '0' ;
                    end if;


         when others => null;
        end case;
    end process;

oAvalonReadData <= RDataM & RDataL ;

end Bhv;



