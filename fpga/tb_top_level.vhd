LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
 
ENTITY tb_top_level IS
END tb_top_level;
 
ARCHITECTURE behavior OF tb_top_level IS 
 
    component top_level is
    Port ( sysclk_32m      : in    std_logic;
           leds            : out   std_logic_vector(4 downto 0);
           reset_button    : in    std_logic;
           console_select  : in    std_logic;

           -- UART0 (to FTDI USB chip, no flow control)
           serial_rx       : in    std_logic;
           serial_tx       : out   std_logic;

           -- UART0 (to MAX3232 level shifter chip, hardware flow control)
           uart1_rx        : in    std_logic;
           uart1_cts       : in    std_logic;
           uart1_tx        : out   std_logic;
           uart1_rts       : out   std_logic;

           -- SPI flash chip
           flash_spi_cs    : out   std_logic;
           flash_spi_clk   : out   std_logic;
           flash_spi_mosi  : out   std_logic;
           flash_spi_miso  : in    std_logic;

           -- SD card socket
           sdcard_spi_cs   : out   std_logic;
           sdcard_spi_clk  : out   std_logic;
           sdcard_spi_mosi : out   std_logic;
           sdcard_spi_miso : in    std_logic;

           -- SDRAM chip
           SDRAM_CLK   : out   std_logic;
           SDRAM_CKE   : out   std_logic;
           SDRAM_CS    : out   std_logic;
           SDRAM_nRAS  : out   std_logic;
           SDRAM_nCAS  : out   std_logic;
           SDRAM_nWE   : out   std_logic;
           SDRAM_DQM   : out   std_logic_vector( 1 downto 0);
           SDRAM_ADDR  : out   std_logic_vector (12 downto 0);
           SDRAM_BA    : out   std_logic_vector( 1 downto 0);
           SDRAM_DQ    : inout std_logic_vector (15 downto 0)
       );
    end component;

	COMPONENT sdram_model
	PORT(
		CLK : IN std_logic;
		CKE : IN std_logic;
		CS_N : IN std_logic;
		RAS_N : IN std_logic;
		CAS_N : IN std_logic;
		WE_N : IN std_logic;
		DQM : IN std_logic_vector(1 downto 0);
        BA  : in  STD_LOGIC_VECTOR (1 downto 0);
		ADDR : IN std_logic_vector(12 downto 0);       
		DQ : INOUT std_logic_vector(15 downto 0)
		);
	END COMPONENT;

   --Inputs
   signal sysclk_32m      : std_logic := '0';
   signal serial_rx       : std_logic := '1';
   signal serial_tx       : std_logic := '1';
   signal uart1_rx        : std_logic := '1';
   signal uart1_tx        : std_logic := '1';
   signal uart1_cts       : std_logic := '1';
   signal uart1_rts       : std_logic := '1';
   signal reset_button    : std_logic := '0';
   signal console_select  : std_logic := '1';
   signal flash_spi_miso  : std_logic := '1';
   signal sdcard_spi_miso : std_logic := '1';

	--BiDirs
   signal SDRAM_DQ        : std_logic_vector(15 downto 0);

 	--Outputs
   signal SDRAM_CLK       : std_logic;
   signal SDRAM_CKE       : std_logic;
   signal SDRAM_CS_N      : std_logic;
   signal SDRAM_RAS_N     : std_logic;
   signal SDRAM_CAS_N     : std_logic;
   signal SDRAM_WE_N      : std_logic;
   signal SDRAM_BA        : std_logic_vector( 1 downto 0);
   signal SDRAM_DQM       : std_logic_vector( 1 downto 0);
   signal SDRAM_ADDR      : std_logic_vector(12 downto 0);
   signal leds            : std_logic_vector(4 downto 0);
   signal flash_spi_cs    : std_logic;
   signal flash_spi_clk   : std_logic;
   signal flash_spi_mosi  : std_logic;
   signal sdcard_spi_cs   : std_logic;
   signal sdcard_spi_clk  : std_logic;
   signal sdcard_spi_mosi : std_logic;

   -- Clock period definitions
   constant clk_period : time := 31.25 ns;
   
BEGIN
 
	-- Instantiate the Unit Under Test (UUT)
    uut: top_level PORT MAP (
          sysclk_32m        => sysclk_32m,
          leds              => leds,
          serial_rx         => serial_rx,
          serial_tx         => serial_tx,
          uart1_rx          => uart1_rx,
          uart1_tx          => uart1_tx,
          uart1_rts         => uart1_rts,
          uart1_cts         => uart1_cts,
          reset_button      => reset_button,
          console_select    => console_select,
          flash_spi_cs      => flash_spi_cs,
          flash_spi_clk     => flash_spi_clk,
          flash_spi_mosi    => flash_spi_mosi,
          flash_spi_miso    => flash_spi_miso,
          sdcard_spi_cs     => sdcard_spi_cs,
          sdcard_spi_clk    => sdcard_spi_clk,
          sdcard_spi_mosi   => sdcard_spi_mosi,
          sdcard_spi_miso   => sdcard_spi_miso,
          SDRAM_CLK         => SDRAM_CLK,
          SDRAM_CKE         => SDRAM_CKE,
          SDRAM_CS          => SDRAM_CS_N,
          SDRAM_nRAS        => SDRAM_RAS_N,
          SDRAM_nCAS        => SDRAM_CAS_N,
          SDRAM_nWE         => SDRAM_WE_N,
          SDRAM_DQM         => SDRAM_DQM,
          SDRAM_ADDR        => SDRAM_ADDR,
          SDRAM_BA          => SDRAM_BA,
          SDRAM_DQ          => SDRAM_DQ
        );

    -- a simulated SDRAM chip to talk to the SDRAM controller
    -- (does not simulate the full 8MB!)
	Inst_sdram_model: sdram_model PORT MAP(
		CLK   => SDRAM_CLK,
		CKE   => SDRAM_CKE,
		CS_N  => SDRAM_CS_N,
		RAS_N => SDRAM_RAS_N,
		CAS_N => SDRAM_CAS_N,
		WE_N  => SDRAM_WE_N,
		DQM   => SDRAM_DQM,
		ADDR  => SDRAM_ADDR,
        BA    => SDRAM_BA,
		DQ    => SDRAM_DQ
	);

   -- Make the clock wiggle at the appropriate frequency
   clk_process :process
   begin
		sysclk_32m <= '0';
		wait for clk_period/2;
		sysclk_32m <= '1';
		wait for clk_period/2;
   end process;
 
   -- Stimulus process
   stim_proc: process
   begin		
      -- hold reset state for 100 ns.
      wait for 100 ns;	

      wait for clk_period*2000;

      -- insert stimulus here 

      -- -- 'h'
      -- serial_rx <= '0'; wait for 8680 ns;  -- start bit

      -- serial_rx <= '0'; wait for 8680 ns;
      -- serial_rx <= '0'; wait for 8680 ns;
      -- serial_rx <= '0'; wait for 8680 ns;
      -- serial_rx <= '1'; wait for 8680 ns;

      -- serial_rx <= '0'; wait for 8680 ns;
      -- serial_rx <= '1'; wait for 8680 ns;
      -- serial_rx <= '1'; wait for 8680 ns;
      -- serial_rx <= '0'; wait for 8680 ns;

      -- serial_rx <= '1'; wait for 8680 ns; -- stop bit

      -- -- 'e'
      -- serial_rx <= '0'; wait for 8680 ns;  -- start bit

      -- serial_rx <= '1'; wait for 8680 ns;
      -- serial_rx <= '0'; wait for 8680 ns;
      -- serial_rx <= '1'; wait for 8680 ns;
      -- serial_rx <= '0'; wait for 8680 ns;

      -- serial_rx <= '0'; wait for 8680 ns;
      -- serial_rx <= '1'; wait for 8680 ns;
      -- serial_rx <= '1'; wait for 8680 ns;
      -- serial_rx <= '0'; wait for 8680 ns;

      -- serial_rx <= '1'; wait for 8680 ns; -- stop bit

      -- -- 'l'
      -- serial_rx <= '0'; wait for 8680 ns;  -- start bit

      -- serial_rx <= '0'; wait for 8680 ns;
      -- serial_rx <= '0'; wait for 8680 ns;
      -- serial_rx <= '1'; wait for 8680 ns;
      -- serial_rx <= '1'; wait for 8680 ns;

      -- serial_rx <= '0'; wait for 8680 ns;
      -- serial_rx <= '1'; wait for 8680 ns;
      -- serial_rx <= '1'; wait for 8680 ns;
      -- serial_rx <= '0'; wait for 8680 ns;

      -- serial_rx <= '1'; wait for 8680 ns; -- stop bit

      -- -- 'p'
      -- serial_rx <= '0'; wait for 8680 ns;  -- start bit

      -- serial_rx <= '0'; wait for 8680 ns;
      -- serial_rx <= '0'; wait for 8680 ns;
      -- serial_rx <= '0'; wait for 8680 ns;
      -- serial_rx <= '0'; wait for 8680 ns;

      -- serial_rx <= '1'; wait for 8680 ns;
      -- serial_rx <= '1'; wait for 8680 ns;
      -- serial_rx <= '1'; wait for 8680 ns;
      -- serial_rx <= '0'; wait for 8680 ns;

      -- serial_rx <= '1'; wait for 8680 ns; -- stop bit

      wait;
   end process;
END;
