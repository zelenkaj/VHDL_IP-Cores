-------------------------------------------------------------------------------
--! @file axi_lite_slave_wrapperRtl.vhd
--
--! @brief AXI lite slave wrapper on avalon slave interface signals
--
--! @details AXI lite slave will convert AXI slave interface singal to Avalon
--! interface signals.
--
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library work;
use work.global.all;
--TODO: update description
-------------------------------------------------------------------------------
--! @brief
--! @details  AXI lite slave wrapper will recieve singls from AXI bus and
--! provide proper inputs for a avlon interface to perform the same action
--! initiated by axi master
-------------------------------------------------------------------------------
entity axi_lite_slave_wrapper is
generic
    (
     --! Base Lower address for the AXI lite slave interface
     gBaseAddr         : std_logic_vector(31 downto 0) := x"00000000";
     --! Base Higher address for the AXI lite slave interface
     gHighAddr         : std_logic_vector(31 downto 0) := x"0000ffff";
     --! Address width for AXI bus interface
     gAddrWidth : integer                       := 32;
     --! Data width for AXI bus interface
     gDataWidth : integer                       := 32
    );

port
    (
    --! Global Clock for AXI
    iAclk            :   in  std_logic                                        ;
    --! Global Reset for AXI
    inAReset         :   in  std_logic                                        ;
    --! Address for Write Address Channel
    iAwaddr    :   in  std_logic_vector (gAddrWidth -1 downto 0);
    --! Protection for Write Address Channel
    iAwprot    :   in  std_logic_vector ( 2 downto 0)                   ;
    --! AddressValid for Write Address Channel
    iAwvalid   :   in  std_logic                                        ;
    --! AddressReady for Write Address Channel
    oAwready   :   out std_logic                                        ;

    --! WriteData for Write Data Channel
    iWdata     :   in  std_logic_vector (gDataWidth-1 downto 0) ;
    --! WriteStrobe for Write Data Channel
    iWstrb     :   in  std_logic_vector (gDataWidth/8-1 downto 0);
    --! WriteValid for Write Data Channel
    iWvalid    :   in  std_logic                                        ;
    --! WriteReady for Write Data Channel
    oWready    :   out std_logic                                        ;

    --! WriteResponse for Write Response Channel
    oBresp     :   out std_logic_vector (1 downto 0)                    ;
    --! ResponseValid for Write Response Channel
    oBvalid    :   out std_logic                                        ;
    --! ResponaseReady for Write Response Channel
    iBready    :   in  std_logic                                        ;

    --! ReadAddress for Read Adddress Channel
    iAraddr    :   in  std_logic_vector (gAddrWidth -1 downto 0);
    --! ReadAddressProtection for Read Address Channel
    iArprot    :   in  std_logic_vector (2 downto 0)                    ;
    --! ReadAddressValid for Read Address Channel
    iArvalid   :   in  std_logic                                        ;
    --! ReadAddressReady for Read Address Channel
    oArready   :   out std_logic                                        ;

    --! ReadData for Read Data Channel
    oRdata     :   out std_logic_vector (gDataWidth-1 downto 0) ;
    --! ReadResponse for Read Data Channel
    oRresp     :   out std_logic_vector (1 downto 0)                    ;
    --! ReadValid for Read Data Channel
    oRvalid    :   out std_logic                                        ;
    --! ReadReady for Read Dara Channel
    iRready    :   in  std_logic                                        ;

    --! Address to Avalon Slave Interface
    oAvsAddress      :   out std_logic_vector (gAddrWidth-1 downto 0)  ;
    --! Byte Enable for Avalon Slave interface
    oAvsByteenable   :   out std_logic_vector (gDataWidth/8-1 downto 0);
    --! Write Data for Avalon Slave interface
    oAvsWritedata    :   out std_logic_vector (gDataWidth-1 downto 0) ;
    --! Read Data for Avalon Slave interface
    iAvsReaddata     :   in  std_logic_vector (gDataWidth-1 downto 0) ;
    --! Read signal for Avalon Slave interface
    oAvsRead         :   out std_logic          ;
    --! Write signal for Avalon Slave interface
    oAvsWrite        :   out std_logic          ;
    --! WaitRequest for Avalon slave interface
    iAvsWaitrequest  :   in  std_logic
    );

end axi_lite_slave_wrapper;

architecture Rtl of axi_lite_slave_wrapper is

type state is (
                sIDLE,
                sREAD,
                sREAD_DONE,
                sWRITE,
                sWRITE_DONE,
                sWRRES_DONE,
                sDELAY
                ) ;


--Avalon Interface designs
signal  address    : std_logic_vector (gAddrWidth-1 downto 0)      ;
signal  chip_sel   : std_logic                                             ;
signal  byte_enable: std_logic_vector (gDataWidth/8-1 downto 0)    ;

--Signals for FSM
signal  tStateCurrent   :  state     ;
signal  tStateNext      :  state     ;

--Internal Signals
signal avalonRead          : std_logic                       ;
signal avalonReadDataLatch : std_logic_vector (31 downto 0)  ;
signal avalonWrite         : std_logic                       ;

signal axiWriteData : std_logic_vector (31 downto 0)  ;
signal axiDataValid : std_logic                       ;

signal writeStart   : std_logic ;
signal write_sel    : std_logic;
signal readStart    : std_logic ;
signal read_sel     : std_logic;


begin

-- TODO: Check weather we need to add clock sync to make sure data & control
--       signal crossing should be in same clock domains
--


--Avalon Slave Interface Singals
oAvsAddress      <=  address             ;
oAvsByteenable   <=  byte_enable         ;

oAvsRead   <= avalonRead ;
avalonRead <= cActivated   when (readStart = cActivated  and
                                (tStateCurrent = sIDLE))       else
              cActivated   when (tStateCurrent = sREAD)        else
              cInactivated when (tStateCurrent = sREAD_DONE)  else
              cInactivated ;

oAvsWrite   <= avalonWrite ;
avalonWrite <=  cActivated when ((tStateCurrent = sWRITE) and
                                 (iWvalid = cActivated)) else
                cActivated when ((tStateCurrent = sIDLE)  and
                                 (axiDataValid = cActivated)) else
                cActivated when (tStateCurrent = sWRITE_DONE)  else
                cInactivated ;

oAvsWritedata   <= axiWriteData ;
axiWriteData    <= iWdata when (axiDataValid = cActivated) else
                   axiWriteData;


-- AXI Lite Write Data Signals

oBvalid  <= cActivated when ((tStateCurrent = sWRITE_DONE) and
                                  (iAvsWaitrequest = cInactivated))  else
            cActivated when ((tStateCurrent = sWRRES_DONE ))     else
            cInactivated    ;

oAwready <= cActivated when ((tStateCurrent = sIDLE) and
                             (writeStart = cActivated)) else
            cInactivated ;

oWready  <= cActivated when (tStateCurrent = sWRITE)        else
            cActivated when ((tStateCurrent = sIDLE) and
                              (axiDataValid = cActivated))  else
            cInactivated ;

-- AXI lite Read Data Signals
oArready <= cActivated when (tStateCurrent = sIDLE and
                            (readStart = cActivated))        else
                   cInactivated ;

oRvalid  <= cActivated when ((iAvsWaitrequest = cInactivated) and
                                  (tStateCurrent = sREAD))              else
            cActivated when ((tStateCurrent = sREAD_DONE))         else
            cInactivated ;

oRdata         <= avalonReadDataLatch ;
avalonReadDataLatch <= iAvsReaddata  when (iAvsWaitrequest = cInactivated) else
                       avalonReadDataLatch ;

--TODO: Check the possibility of Error Response signals
oBresp      <=   "00"        ;
oRresp      <=   "00"        ;

-- Address Decoder
chip_sel <= read_sel or write_sel ;

--TODO: Cleanup codes
write_sel <= cActivated when(iAwaddr(31 downto 16) = gBaseAddr(31 downto 16)) else
             cInactivated ;

read_sel  <= cActivated when(iAraddr(31 downto 16) = gBaseAddr(31 downto 16)) else
             cInactivated ;

--TODO: Revert it to Full address range to generalize the design
address (19 downto 2) <= iAraddr (19 downto 2) when (readStart = cActivated
                                               and (tStateCurrent = sIDLE)) else
                         iAwaddr (19 downto 2) when (writeStart = cActivated
                                               and (tStateCurrent = sIDLE)) else
                         address (19 downto 2) ;


writeStart   <= chip_sel and iAwvalid ;
readStart    <= chip_sel and iArvalid ;
axiDataValid <= iWvalid ;

byte_enable  <= x"F"        when (readStart = cActivated  and
                                 (tStateCurrent = sIDLE)) else
                iWstrb when (writeStart = cActivated and
                                 (tStateCurrent = sIDLE)) else
                byte_enable ;

-- Main Control FSM for converting AXI lite signals to Avalon
--TODO: Explain logic if possible with Diagram in doxygen
--! Clock Based Process for state changes
SEQ_LOGIC_FSM:process
       (
          iAclk
        )
    begin
    if(rising_edge(iAclk))  then
        if( inAReset = cnActivated )   then
            tStateCurrent     <= sIDLE ;
        else
            tStateCurrent     <= tStateNext ;
        end if;
    end if;
 end process;

--TODO: Explain logic if possible with Diagram in doxygen
--! Control based Process for state updation
COM_LOGIC_FSM:    process
        (
           tStateCurrent ,
           chip_sel     ,
           writeStart,
           readStart,
           iAwvalid,
           iArvalid,
           iRready ,
           iWvalid,
           iBready,
           iAvsWaitrequest
        )
    begin
    --Default values
    tStateNext       <=  tStateCurrent    ;

    case (tStateCurrent) is
            when sIDLE =>
              if(chip_sel = cActivated ) then
                   --Write Operations
                  if(iAwvalid    =   cActivated) then
                   if(iWvalid = cActivated) then
                    if(iAvsWaitrequest = cInactivated) then
                     tStateNext <= sWRRES_DONE ;
                    else
                     tStateNext <= sWRITE_DONE ;
                    end if;
                   else
                    tStateNext <= sWRITE ;
                   end if;
                   --Read Operations
                  elsif (iArvalid    =   cActivated) then
                       if(iAvsWaitrequest = cInactivated) then
                          tStateNext     <= sREAD_DONE ;
                      else
                        tStateNext     <= sREAD ;
                      end if;
                  else
                   tStateNext  <= sIDLE ;
                  end if;
              else
                   tStateNext <= sIDLE ;
              end if;

            when sREAD =>
               -- Read Valid gets assert Here
               if(iAvsWaitrequest = cInactivated) then
                 if( iRready = cActivated ) then
                  tStateNext     <= sIDLE ;
                 else
                  tStateNext     <= sREAD_DONE ;
                 end if;
              else
                tStateNext     <= sREAD ;
              end if;

             when sREAD_DONE =>
                if( iRready = cActivated ) then
                 tStateNext     <= sIDLE ;
                else
                 tStateNext     <= sREAD_DONE ;
                end if;

             when sWRITE =>
               if(iWvalid = cActivated ) then
                   if(iAvsWaitrequest = cInactivated) then
                       if(iBready = cActivated) then
                        tStateNext     <= sIDLE ;
                       else
                        tStateNext     <= sWRRES_DONE ;
                       end if;
                    else
                        tStateNext     <= sWRITE_DONE ;
                    end if;
               else
                  tStateNext     <= sWRITE ;
               end if;

             when sWRITE_DONE =>
                if(iAvsWaitrequest = cInactivated) then
                   if(iBready = cActivated) then
                    tStateNext     <= sIDLE ;
                   else
                    tStateNext     <= sWRRES_DONE ;
                   end if;
                else
                    tStateNext     <= sWRITE_DONE ;
                end if;

             when sWRRES_DONE =>

                if(iBready = cActivated) then
                    tStateNext     <= sIDLE ;
                else
                    tStateNext     <= sWRRES_DONE ;
                end if;

              when sDELAY =>
               tStateNext <= sIDLE ;

            when others => null ;

        end case;
    end process;

end Rtl;