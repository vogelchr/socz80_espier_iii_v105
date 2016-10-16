library ieee;
use ieee.std_logic_1164.all;
use IEEE.numeric_std.all;

entity seven_seg is
    port (
       clk           : in  std_logic;
       reset         : in  std_logic;
       cpu_address   : in  std_logic_vector(2 downto 0);
       data_in       : in  std_logic_vector(7 downto 0);
       data_out      : out std_logic_vector(7 downto 0);
       enable        : in  std_logic;
       read_notwrite : in  std_logic;
	   digit         : out std_logic_vector(3 downto 0);
	   segment       : out std_logic_vector(7 downto 0)
    );
end seven_seg;

architecture behavioral of seven_seg is
	type decode_table is array (0 to 15) of std_logic_vector(6 downto 0);
	constant hex_decode : decode_table := (
		--       a
		--      ===
		--  f||     ||b
		--   || g   ||
		--      ===
		--  e||     ||c
		--   ||     ||    slv(6..0) = b"gfedcba"
		--      ===
		--       d
		-- 0          1           2           3
		b"0111111", b"0000110", b"1011011", b"1001111",
		-- 4          5           6           7
		b"1100110", b"1101101", b"1111101", b"0000111",
		-- 8          9           a           b
		b"1111111", b"1101111", b"1110111", b"1111100",
		-- c          d           e           f
		b"1011000", b"1011110", b"1111001", b"1110001"
	);
	signal curr_digit : unsigned(1 downto 0) := (others => '0');
	signal next_digit : unsigned(curr_digit'range);
	signal next_nibble : unsigned(digit'range);

	signal ctr        : unsigned(15 downto 0) := (others => '0');
	constant CTR_ZERO : unsigned(ctr'range) := (others => '0');

	signal ss_data    : unsigned(15 downto 0) := (others => '0');
	signal dp         : std_logic_vector(3 downto 0) := (others => '0');
begin

clk_reset: process(clk)
	begin
		if rising_edge(clk) then
			ctr <= ctr + 1;
			if ctr = CTR_ZERO then
				curr_digit <= next_digit;
				-- must be synchronous to use distributed ram
				segment(6 downto 0) <= hex_decode(to_integer(next_nibble));
				segment(7)          <= dp(to_integer(next_digit));
			end if;
		end if;
	end process;

	next_digit <= curr_digit + 1;
	next_nibble <=
		ss_data(15 downto 12) when next_digit = x"0" else
		ss_data(11 downto 8)  when next_digit = x"1" else
		ss_data(7 downto 4)   when next_digit = x"2" else
		ss_data(3 downto 0)   when next_digit = x"3";

	-- digits are low-active (cathode)
	-- segments are high active
	digit(0) <= '0' when curr_digit = x"0" else '1';
	digit(1) <= '0' when curr_digit = x"1" else '1';
	digit(2) <= '0' when curr_digit = x"2" else '1';
	digit(3) <= '0' when curr_digit = x"3" else '1';

    --- blatantly copied from the GPIO block 
    with cpu_address select
        data_out <=
            std_logic_vector(ss_data( 7 downto 0)) when "000",
            std_logic_vector(ss_data(15 downto 8)) when "001",
            x"0" & dp                              when "010",
            x"ba"                                  when others;

    clk_proc: process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                ss_data <= (others => '0');
                dp      <= (others => '0');
            else
                if enable = '1' and read_notwrite = '0' then
                    case cpu_address is
                        when "000" => ss_data(7 downto 0) <= unsigned(data_in);
                        when "001" => ss_data(15 downto 8) <= unsigned(data_in);
                        when "010" => dp <= data_in(3 downto 0);
                        when others => -- no change
                    end case;
                end if;
            end if;
        end if;
    end process;
end behavioral;
