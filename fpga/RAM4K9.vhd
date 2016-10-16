--+-----------------------------------+-------------------------------------+--
--|                      ___   ___    | (c) 2013-2014 William R Sowerbutts  |--
--|   ___  ___   ___ ___( _ ) / _ \   | will@sowerbutts.com                 |--
--|  / __|/ _ \ / __|_  / _ \| | | |  |                                     |--
--|  \__ \ (_) | (__ / / (_) | |_| |  | A Z80 FPGA computer, just for fun   |--
--|  |___/\___/ \___/___\___/ \___/   |                                     |--
--|                                   |              http://sowerbutts.com/ |--
--+-----------------------------------+-------------------------------------+--
--| A 4096 entry deep, 9-bit wide RAM using two Xilinx RAMB16BWER devices.  |--
--+-------------------------------------------------------------------------+--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
library UNISIM;
use UNISIM.VComponents.all;

entity RAM4K9 is
	port(
		clk			: in  std_logic;
        reset       : in  std_logic;
		write  		: in  std_logic;
		address		: in  std_logic_vector(11 downto 0);
		data_in		: in  std_logic_vector(8 downto 0);
		data_out	: out std_logic_vector(8 downto 0)
	);
end RAM4K9;

architecture behaviour of RAM4K9 is

    signal ram0_data_out : std_logic_vector(8 downto 0);
    signal ram1_data_out : std_logic_vector(8 downto 0);
    signal ram0_select : std_logic;
    signal ram1_select : std_logic;

    signal ram0_doa   : std_logic_vector(31 downto 0);
    signal ram0_dopa  : std_logic_vector(3 downto 0);
    signal ram1_doa   : std_logic_vector(31 downto 0);
    signal ram1_dopa  : std_logic_vector(3 downto 0);

begin

    -- multiplex between our two BRAMs using the top bit as a chip select
    ram1_select <= address(11);
    ram0_select <= not ram1_select;

    data_out <= ram1_data_out when ram1_select='1' else
                ram0_data_out;

    -- in 9-bit wide mode, we use DI[7:0], DIP[0], ADDR[13:3], and WE[3:0] all get connected to one write enable

    ram0: RAMB16BWER
    generic map (
        DATA_WIDTH_A => 9,
        DOA_REG => 0,
        RSTTYPE => "SYNC",
        SIM_DEVICE => "SPARTAN6",
        WRITE_MODE_A => "WRITE_FIRST"
    )
    port map (
        CLKA => clk,
        ADDRA(13 downto 3) => address(10 downto 0),
        ADDRA(2 downto 0) => "000",
        DIA(31 downto 8) => "000000000000000000000000",
        DIA(7 downto 0) => data_in(7 downto 0),
        DIPA(3 downto 1) => "000",
        DIPA(0) => data_in(8),
        DOA(31 downto 0) => ram0_doa,
        DOPA(3 downto 0) => ram0_dopa,
        ENA => ram0_select,
        WEA(0) => write,
        WEA(1) => write,
        WEA(2) => write,
        WEA(3) => write,
        RSTA => reset,
        REGCEA => '0',
        -- unused port B
        ADDRB => (others => '0'),
        CLKB => clk,
        DIB => (others => '0'),
        DIPB => (others => '0'),
        ENB => '0',
        WEB => (others => '0'),
        RSTB => '0',
        REGCEB => '0'
    );
    -- doing this here stops the simulator from complaining about partially associated formals.
    ram0_data_out <= ram0_dopa(0) & ram0_doa(7 downto 0);


    ram1: RAMB16BWER
    generic map (
        DATA_WIDTH_A => 9,
        DOA_REG => 0,
        RSTTYPE => "SYNC",
        SIM_DEVICE => "SPARTAN6",
        WRITE_MODE_A => "WRITE_FIRST"
    )
    port map (
        CLKA => clk,
        ADDRA(13 downto 3) => address(10 downto 0),
        ADDRA(2 downto 0) => "000",
        DIA(31 downto 8) => "000000000000000000000000",
        DIA(7 downto 0) => data_in(7 downto 0),
        DIPA(3 downto 1) => "000",
        DIPA(0) => data_in(8),
        DOA(31 downto 0) => ram1_doa,
        DOPA(3 downto 0) => ram1_dopa,
        ENA => ram1_select,
        WEA(0) => write,
        WEA(1) => write,
        WEA(2) => write,
        WEA(3) => write,
        RSTA => reset,
        REGCEA => '0',
        -- unused port B
        ADDRB => (others => '0'),
        CLKB => clk,
        DIB => (others => '0'),
        DIPB => (others => '0'),
        ENB => '0',
        WEB => (others => '0'),
        RSTB => '0',
        REGCEB => '0'
    );
    -- doing this here stops the simulator from complaining about partially associated formals.
    ram1_data_out <= ram1_dopa(0) & ram1_doa(7 downto 0);

end;
