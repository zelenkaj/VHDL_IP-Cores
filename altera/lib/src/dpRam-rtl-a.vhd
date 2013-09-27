--! @file dpRam-bhv-a.vhd
--
--! @brief Dual Port Ram Register Transfer Level Architecture
--
--! @details This is the DPRAM intended for synthesis on Altera platforms only.
--!          Timing as follows [clk-cycles]: write=0 / read=1
--
-------------------------------------------------------------------------------
-- Architecture : rtl
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

--! use altera_mf library
library altera_mf;
use altera_mf.altera_mf_components.all;

architecture rtl of dpRam is
begin

    altsyncram_component : altsyncram
        generic map (
            address_reg_b                   => "CLOCK1",
            byteena_reg_b                   => "CLOCK1",
            byte_size                       => 8,
            clock_enable_input_a            => "BYPASS",
            clock_enable_input_b            => "BYPASS",
            clock_enable_output_a           => "BYPASS",
            clock_enable_output_b           => "BYPASS",
            indata_reg_b                    => "CLOCK1",
            init_file                       => "UNUSED",
            intended_device_family          => "Cyclone IV",
            lpm_type                        => "altsyncram",
            numwords_a                      => gNumberOfWords,
            numwords_b                      => gNumberOfWords,
            operation_mode                  => "BIDIR_DUAL_PORT",
            outdata_aclr_a                  => "NONE",
            outdata_aclr_b                  => "NONE",
            outdata_reg_a                   => "CLOCK0",
            outdata_reg_b                   => "CLOCK1",
            power_up_uninitialized          => "FALSE",
            read_during_write_mode_port_a   => "NEW_DATA_WITH_NBE_READ",
            read_during_write_mode_port_b   => "NEW_DATA_WITH_NBE_READ",
            widthad_a                       => logDualis(gNumberOfWords),
            widthad_b                       => logDualis(gNumberOfWords),
            width_a                         => gWordWidth,
            width_b                         => gWordWidth,
            width_byteena_a                 => gWordWidth/8,
            width_byteena_b                 => gWordWidth/8,
            wrcontrol_wraddress_reg_b       => "CLOCK1"
        )
        port map (
            wren_a          => iWriteEnable_A,
            clock0          => iClk_A,
            wren_b          => iWriteEnable_B,
            clock1          => iClk_B,
            byteena_a       => iByteenable_A,
            byteena_b       => iByteenable_B,
            address_a       => iAddress_A,
            address_b       => iAddress_B,
            data_a          => iWritedata_A,
            data_b          => iWritedata_B,
            q_a             => oReaddata_A,
            q_b             => oReaddata_B
        );

end architecture rtl;