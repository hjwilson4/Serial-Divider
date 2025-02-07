------------------------------------------------------------------------------
--
--  Test Bench for Serial Divider
--
--  This is a test bench for the serial divider entity. The test bench passes 
--  in stimulus values to the serial divider which then returns the observed 
--  signals HexDigit/DecoderBits (DecoderEn is always high) after calculating. 
--  The test bench contains arrays with expected outputs for quotient and 
--  remainder which are tested against the results of the serial divider. 
--  This test bench additionally checks that the divider preserves the value 
--  of the dividend/divisor after calculation. The test bench also tests the 
--  segment decoding of the serial divider. 
--
--  Revision History:
--     12/1/22  Hector Wilson              Initial revision.
--     12/1/22  Hector Wilson              Created 2 test cases to test design.
--     12/4/22  Hector Wilson              Finished assignement added remainder 
--                                         testing. 
--     12/5/22  Hector Wilson              Added more test cases. Updated 
--                                         comments. 
--     12/6/22  Hector Wilson              Added segment decoding testing.
--
------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity serialdivider_tb is
end serialdivider_tb;

architecture Behavioral of serialdivider_tb is
    component serialdivider
        port (
            nReset       :  in   std_logic;
            nCalculate   :  in   std_logic;
            Divisor      :  in   std_logic;
            KeypadRdy    :  in   std_logic;
            Keypad       :  in   std_logic_vector(3 downto 0);
            HexDigit     :  out  std_logic_vector(3 downto 0);
            DecoderEn    :  out  std_logic;
            DecoderBits  :  out  std_logic_vector(3 downto 0);
            segmenta     :  out  std_logic;
            segmentb     :  out  std_logic;
            segmentc     :  out  std_logic;
            segmentd     :  out  std_logic;
            segmente     :  out  std_logic;
            segmentf     :  out  std_logic;
            segmentg     :  out  std_logic;
            CLK          :  in   std_logic
        );
    end component;
    
    -- Stimulus signals
    signal  Keypad : std_logic_vector(3 downto 0);
    signal  Divisor, KeypadRdy, CLK : std_logic := '0';
    signal  nReset, nCalculate : std_logic := '1';
    
    -- Observed signals
    signal  HexDigit, DecoderBits : std_logic_vector(3 downto 0);
    signal  DecoderEn : std_logic;
    signal  segmenta, segmentb, segmentc, segmentd, 
            segmente, segmentf, segmentg : std_logic := '0';
    
    -- For testing segment decoding
    signal  SegmentDigit : std_logic_vector(0 to 6); -- output from divider 
    signal  TestSeg : std_logic_vector (0 to 63);  -- test cases
    
    -- Segment decoding for each hex digit
    constant zero : std_logic_vector(0 to 6) := "1111110";
    constant one  : std_logic_vector(0 to 6) := "0110000";
    constant two  : std_logic_vector(0 to 6) := "1101101";
    constant three: std_logic_vector(0 to 6) := "1111001";
    constant four : std_logic_vector(0 to 6) := "0110011";
    constant five : std_logic_vector(0 to 6) := "1011011";
    constant six  : std_logic_vector(0 to 6) := "1011111";
    constant seven: std_logic_vector(0 to 6) := "1110000";
    constant eight: std_logic_vector(0 to 6) := "1111111";
    constant nine : std_logic_vector(0 to 6) := "1111011";
    constant A    : std_logic_vector(0 to 6) := "1110111";
    constant B    : std_logic_vector(0 to 6) := "0011111";
    constant C    : std_logic_vector(0 to 6) := "1001110";
    constant D    : std_logic_vector(0 to 6) := "0111101";
    constant E    : std_logic_vector(0 to 6) := "1001111";
    constant F    : std_logic_vector(0 to 6) := "1000111";
    
    -- Expected segment decoding output
    signal ExpectedSeg : std_logic_vector (0 to 111) := 
                         zero & one & two & three 
                       & four & five & six & seven
                       & eight & nine & A & B & C 
                       & D & E & F;
   
    
    -- Test values
    constant TestCases : integer := 20; -- 20 test cases
    constant DigitSize : integer := 4; -- each hex digit contains 4 bits
    constant N : integer := 4; -- 4 hex digits per register 
    signal  TestDividend : std_logic_vector(0 to (TestCases*16)-1);
    signal  TestDivisor  : std_logic_vector(0 to (TestCases*16)-1);
    signal  TestQuotient : std_logic_vector(0 to (TestCases*16)-1);
    signal  TestRemainder: std_logic_vector(0 to (TestCases*16)-1);

begin
-- set up clock signal (1MHz)
CLK <= not CLK after 1 us /2;

-- set up unit under test (serial divider)
UUT : serialdivider 
    port map(
        nReset => nReset,
        nCalculate => nCalculate,
        Divisor => Divisor,
        KeypadRdy => KeypadRdy,
        Keypad => Keypad,
        HexDigit => HexDigit,
        DecoderEn => DecoderEn,
        DecoderBits => DecoderBits,
        segmenta => segmenta,
        segmentb => segmentb,
        segmentc => segmentc,
        segmentd => segmentd,
        segmente => segmente,
        segmentf => segmentf,
        segmentg => segmentg,
        CLK => CLK
    );
    -- test hex digits for segment decoder (0..1..2......E..F)
    TestSeg <= X"0" & X"1" & X"2" & X"3" & X"4" & X"5"
             & X"6" & X"7" & X"8" & X"9" & X"A" & X"B"
             & X"C" & X"D" & X"E" & X"F";
    
    -- store output segments as array for testing purposes
    SegmentDigit <= segmenta & segmentb & segmentc & segmentd & segmente & segmentf & segmentg;
    
    -- initialize test dividends (20 test cases)
    TestDividend   <= X"0089" & X"00E5" & X"0014" & X"1000" & X"4444" 
                    & X"a88a" & X"F000" & X"cccc" & X"7764" & X"4444"
                    & X"8989" & X"F000" & X"cccc" & X"8888" & X"9b15"
                    & X"8a2c" & X"0000" & X"f1a6" & X"0004" & X"dcba";
    -- initialize test divisors (20 test cases)
    TestDivisor    <= X"000b" & X"0005" & X"0081" & X"FFFF" & X"0001" 
                    & X"21df" & X"0002" & X"6661" & X"a843" & X"1111"
                    & X"00bb" & X"00a2" & X"cc0c" & X"1010" & X"4a29"
                    & X"8a2c" & X"FFFF" & X"5511" & X"0000" & X"abcd";
    -- initialize test quotients (20 test cases)
    TestQuotient   <= X"000c" & X"002d" & X"0000" & X"0000" & X"4444" 
                    & X"0004" & X"7800" & X"0002" & X"0000" & X"0004"
                    & X"00bc" & X"017b" & X"0001" & X"0008" & X"0002"
                    & X"0001" & X"0000" & X"0002" & X"FFFF" & X"0001";
    -- initialize test remainders (20 test cases)
    TestRemainder  <= X"0005" & X"0004" & X"0014" & X"1000" & X"0000" 
                    & X"210e" & X"0000" & X"000a" & X"7764" & X"0000"
                    & X"0035" & X"002a" & X"00c0" & X"0808" & X"06c3"
                    & X"0000" & X"0000" & X"4784" & X"0004" & X"30ed";
    
process 
    variable i : integer;
begin 
    nReset <= '0';  -- initial system reset
    wait for 50 ms;
    nReset <= '1';
    wait for 50 ms;
    
    -- first test segment decoding. Load in each possible hex digit (0..F) 
    -- into dividend and test output segment to be correct.
    for i in 0 to TestSeg'Right/(DigitSize) loop
        Divisor <= '0';
        KeypadRdy <= '1';
        Keypad <= TestSeg(4*i to 4*i+3);
        wait for 50 ms;
        KeypadRdy <= '0';
        Keypad <= "0000";
        wait until DecoderBits = X"3";
        wait for 0.5 ms; -- let divider calculate segments
        assert (std_match(SegmentDigit, not ExpectedSeg(7*i to 7*i+6))) 
            report "Test Failure Segment Decoding Incorrect"
            severity ERROR;
        wait for 50 ms;
    end loop;
    
    -- Now begin testing, loop through each test case
    for i in 0 to TestQuotient'Right/(DigitSize*N) loop
        -- load in each digit of Dividend for this test case
        for j in 0 to N-1 loop  -- loop through 4 hex digits
            Divisor <= '0';
            KeypadRdy <= '1';
            Keypad <= TestDividend(16*i+DigitSize*j to 16*i+DigitSize*j+3);
            wait for 50 ms;
            KeypadRdy <= '0';
            Keypad <= "0000";
            wait for 50 ms;
        end loop;
        
        -- load in each digit of Divisor for this test case
        for j in 0 to N-1 loop  -- loop through 4 hex digits
            Divisor <= '1';
            KeypadRdy <= '1';
            Keypad <= TestDivisor(16*i+DigitSize*j to 16*i+DigitSize*j+3);
            wait for 50 ms;
            KeypadRdy <= '0';
            Keypad <= "0000";
            wait for 50 ms;
        end loop;
        
        -- perform calculation and output quotient (divisor = 0)
        Divisor <= '0';
        nCalculate <= '0';
        wait for 50 ms; -- compute
        nCalculate <= '1'; -- stop calculating
        wait for 50 ms;  -- next, check results
        
        -- make sure dividend is preserved after calculation
        wait until DecoderBits = X"3";
        assert (std_match(HexDigit,TestDividend(16*i+12 to 16*i+15))) 
            report "Test Failure Dividend Not Preserved"
            severity ERROR;
        wait until DecoderBits = X"2";
        assert (std_match(HexDigit,TestDividend(16*i+8 to 16*i+11))) 
            report "Test Failure Dividend Not Preserved"
            severity ERROR;
        wait until DecoderBits = X"1";
        assert (std_match(HexDigit,TestDividend(16*i+4 to 16*i+7))) 
            report "Test Failure Dividend Not Preserved"
            severity ERROR;
        wait until DecoderBits = X"0";
        assert (std_match(HexDigit,TestDividend(16*i to 16*i+3))) 
            report "Test Failure Dividend Not Preserved"
            severity ERROR;
        
        -- make sure divisor is preserved after calculation
        wait until DecoderBits = X"7";
        assert (std_match(HexDigit,TestDivisor(16*i+12 to 16*i+15))) 
            report "Test Failure Divisor Not Preserved"
            severity ERROR;
        wait until DecoderBits = X"6";
        assert (std_match(HexDigit,TestDivisor(16*i+8 to 16*i+11))) 
            report "Test Failure Divisor Not Preserved"
            severity ERROR;
        wait until DecoderBits = X"5";
        assert (std_match(HexDigit,TestDivisor(16*i+4 to 16*i+7))) 
            report "Test Failure Divisor Not Preserved"
            severity ERROR;
        wait until DecoderBits = X"4";
        assert (std_match(HexDigit,TestDivisor(16*i to 16*i+3))) 
            report "Test Failure Divisor Not Preserved"
            severity ERROR;
        
        -- test calculated quotient
        wait until DecoderBits = X"b";
        assert (std_match(HexDigit,TestQuotient(16*i+12 to 16*i+15))) 
            report "Test Failure Quotient Incorrect"
            severity ERROR;
        wait until DecoderBits = X"a";
        assert (std_match(HexDigit,TestQuotient(16*i+8 to 16*i+11))) 
            report "Test Failure Quotient Incorrect"
            severity ERROR;
        wait until DecoderBits = X"9";
        assert (std_match(HexDigit,TestQuotient(16*i+4 to 16*i+7))) 
            report "Test Failure Quotient Incorrect"
            severity ERROR;
        wait until DecoderBits = X"8";
        assert (std_match(HexDigit,TestQuotient(16*i to 16*i+3))) 
            report "Test Failure Quotient Incorrect"
            severity ERROR;
        
        wait for 50 ms;
        Divisor <= '1';    -- switch output to remainder (divisor = 1)
        wait for 50 ms;    -- now check if remainder is correct
        
        -- test calculated remainder
        wait until DecoderBits = X"b";
        assert (std_match(HexDigit,TestRemainder(16*i+12 to 16*i+15))) 
            report "Test Failure Remainder Incorrect"
            severity ERROR;
        wait until DecoderBits = X"a";
        assert (std_match(HexDigit,TestRemainder(16*i+8 to 16*i+11))) 
            report "Test Failure Remainder Incorrect"
            severity ERROR;
        wait until DecoderBits = X"9";
        assert (std_match(HexDigit,TestRemainder(16*i+4 to 16*i+7))) 
            report "Test Failure Remainder Incorrect"
            severity ERROR;
        wait until DecoderBits = X"8";
        assert (std_match(HexDigit,TestRemainder(16*i to 16*i+3))) 
            report "Test Failure Remainder Incorrect"
            severity ERROR;
         
         wait for 50 ms;
         -- go to next test case
        
    end loop;
    wait;
end process;

end Behavioral;
