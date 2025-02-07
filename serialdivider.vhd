----------------------------------------------------------------------------
--
--  Serial Divider
--
--  This file contains a serial divider entity. The serial divider consists 
--  of a main controller finite state machine that is idle by default. In the  
--  first 32 clock cycles, the FSM enters the init state inwhich the remainder 
--  is cleared the dividend is transfered to a seperate register. Next, the 
--  FSM cycles between run1, run2, run3 a total of 16 times (n=16 bits). run1 
--  (1 clock cycle) shifts the MSB of the dividend into the LSB of the 
--  remainder. run2 performs the serial addition of remainder = remainder +- 
--  divisor and runs for 17 clock cycles (16+1 bits). run3 (1 clock cycle) 
--  shifts the carry signal into the output quotient. After 16 such loops, the 
--  quotient is calculated and if the carry flag is set, then remainder is as 
--  well. If after 16 loops, the carry is not set, then the remainder must be 
--  fixed by doing another round of remainder = remainder + divisor for 17 clocks. 
--
--  The serial divider operates around a shift register DigitBits(47..0) which 
--  holds the dividend (low 16 bits), divisor (middle 16 bits), and the output 
--  (high 16 bits). The output can be toggled between displaying the quotient 
--  or the remainder. This shift register shifts digits every 1 ms to ensure 
--  proper multiplexing of the LED display. However, calculations are only 
--  performed when CurDigit = 3, meaning the DigitBits shift register is in 
--  the correct configuration explained above: Output | Divisor | Dividend. 
--  The multiplexing shift order is 3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, then 8.
--
--  The timing of the FSM operates using a multiplex counter (MuxCntr) which 
--  increments up to 3FF and then resets back to 0 (loops every 1 ms). For the 
--  regular quotient calculation, all stages of the FSM fit inside the (1 ms) 
--  time period before looping to the next digit for multiplexing. In the event 
--  the remainder needs to be fixed, the FSM waits until we are back at current 
--  digit = 3 to calculate the fixed remainder. This means whenever the current 
--  digit is at 3, the FSM can either do normal division calculation or 
--  fix remainder calculation but never both. 
--
--  Inputs:
--  The nCalculate input is the switch (active low) on the board used to toggle 
--  the division to happen. The Divisor switch toggles between the dividend 
--  divisor. That is, when divisor = 0, keypad inputs are shifted into the 
--  dividend and when divisor = 1, keypad inputs are shifted into the divisor. 
--  KeypadRdy input indicates the keypad has been pressed and Keypad(3..0) 
--  contains the resulting digit that will be shifted into the respective 
--  register. Lastly, clock input is a 1MHz clock.  
--
--  Outputs:
--  HexDigit(3..0) output contains the Hex digit currently being multiplexed to 
--  a 7-segment LED display. HexDigit(3..0) is sent into a 16V8 IC for segment 
--  decoding. When the 16V8 is used for segment decoding, the segmenta..segmentg 
--  outputs must be tri-stated. Alternatively, the segmenta..segmentg outputs 
--  are already segment decoded versions of HexDigit(3..0) and can be output 
--  to the LEDs instead of using the 16V8. 
--
--  DecoderEn output toggles the 29M16 for digit decoding where DecoderBits(3..0) 
--  is the digit being decoded. Since this file/CPLD doesn't contain digit decoding, 
--  DecoderEn remains high at all times to keep the 29M16 active. 
--  
--
--  Revision History:
--     12/1/22  Hector Wilson    Initial revision (adapted from addsub16.vhd)
--     12/2/22  Hector Wilson    Worked on design, added MuxCntr tracking signals 
--     12/3/22  Hector Wilson    Has semi-working model of serial divider with 
--                               some occasional bugs. Quotient is correct.
--     12/4/22  Hector Wilson    Removed bugs. Added remainder calculations 
--                               to the design. Enabled preservation of dividend 
--                               and divisor during operation (i.e. LEDs dont change). 
--     12/5/22  Hector Wilson    Added segment decoding & comments. 
--     12/6/22  Hector Wilson    Revised/added comments. 
--
----------------------------------------------------------------------------
-- libraries
library  ieee;
use  ieee.std_logic_1164.all;
use  ieee.numeric_std.all;

--
--  Serial Divider entity declaration
--
--  The entity takes input from the keypad and displays it on the 7-segment
--  LEDs. When the calculate button is pressed, both the quotient and remainder 
--  are calculated with the Divisor switch used to select which of the two to 
--  display. The input values and quotient/remainder are displayed
--  in hexadecimal.  For both input values (dividend/divisor) only the last 4 hex 
--  digits (16-bits) are used.
--
--  Inputs:
--     nReset                  - active low reset signal (for testing only)
--                               tied high in hardware
--     nCalculate              - calculate the quotient+remainder (active low)
--     Divisor                 - input the divisor (high) or dividend (low) 
--                               also used to toggle between displaying the 
--                               quotient (low) or remainder (high)
--     KeypadRdy               - signals there is a key available
--     Keypad(3 downto 0)      - keypad input
--     CLK                     - the clock (1 MHz)
--
--  Outputs:
--     HexDigit(3 downto 0)    - hex digit to display (to segment decoder)
--                               *later replaced with segment decoding outputs 
--                               below (for the actual Hardware)*
--     DecoderEn               - enable for the 4:12 digit decoder
--     segmenta                - segment a of the LED display 
--        .
--        .
--        .
--     segmentg                - segment g of the LED display
--     DecoderBits(3 downto 0) - digit to display (to 4:12 decoder)


entity  serialdivider  is

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

end  serialdivider;


--
--  Serial Divider architecture
--

architecture  Behavioral  of  serialdivider  is

    -- keypad signals
    signal  HaveKey     :  std_logic;           -- have a key from the keypad
    signal  KeypadRdyS  :  std_logic_vector(2 downto 0); -- keypad ready synchronization

    -- LED/Divisor multiplexing signals
    signal  MuxCntr : unsigned(9 downto 0) := (others => '0');-- multiplex counter (to
                                                              -- divide 1 MHz to 1 KHz)
    signal  DigitClkEn  :  std_logic := '0';           -- enable for the digit clock
                                                
    -- multiplex tracker signals used to show what state FSM is in (based on MuxCntr)
    signal  init, run1, run2_16, run2_17, run3, done, fixrem16, fixrem17 :  std_logic := '0';
    
    -- operation signal indicating whether addition or subtraction is being performed in 
    -- the 1-bit adder/subtractor (initialized to addition)
    signal  op : std_logic := '0';
    
    signal  CurDigit  :  std_logic_vector(3 downto 0) := "0000"; -- current mux digit

    --  signals to select shift register operation
    --     ShiftOp = 0  ==>  hold
    --     ShiftOp = 1  ==>  keypad input shift
    --     ShiftOp = 2  ==>  display shift
    --     ShiftOp = 3  ==>  shift remainder clear (init of FSM)
    --     ShiftOp = 4  ==>  shift remainder and dividend (run1 of FSM)
    --     ShiftOp = 5  ==>  shift carry into quotient (run3 of FSM)
    --     ShiftOp = 6  ==>  calculate (run2 of FSM) shift
    --     ShiftOp = 7  ==>  calculate (run2 of FSM) shift (bit17)
    --     ShiftOp = 8  ==>  division complete (done state of FSM)
    --     ShiftOp = 9  ==>  fix remainder shift
    --     ShiftOp = 10 ==>  fix remainder shift (bit17)
    --     ShiftOp = 11 ==>  shift output between quotient/remainder
    signal    ShiftOp       :  std_logic_vector(3 downto 0);
    constant  ShiftOpHOLD   :  std_logic_vector(3 downto 0) := "0000";
    constant  ShiftOpKEYIN  :  std_logic_vector(3 downto 0) := "0001";
    constant  ShiftOpSHIFT  :  std_logic_vector(3 downto 0) := "0010";
    constant  ShiftRemCLR   :  std_logic_vector(3 downto 0) := "0011";
    constant  ShiftREMDIV   :  std_logic_vector(3 downto 0) := "0100";
    constant  ShiftCQUO     :  std_logic_vector(3 downto 0) := "0101";
    constant  ShiftOpCALC   :  std_logic_vector(3 downto 0) := "0110";
    constant  ShiftOpCALC2  :  std_logic_vector(3 downto 0) := "0111";
    constant  ShiftDIVDONE  :  std_logic_vector(3 downto 0) := "1000";
    constant  ShiftFixREM   :  std_logic_vector(3 downto 0) := "1001";
    constant  ShiftFixREM2  :  std_logic_vector(3 downto 0) := "1010";
    constant  ShiftSWREM    :  std_logic_vector(3 downto 0) := "1011";

    --  12 stored hex digits (48 bits) in a shift register 
    --  When current digit = 3, DigitBits has Output | Divisor | Dividend
    signal  DigitBits  :  std_logic_vector(47 downto 0) := (others => '0');
    
    -- Seperate register used for computing the remainder 
    signal  Remainder  :  std_logic_vector(16 downto 0) := (others => '0');
    -- Seperate register used for holding the dividend 
    -- to prevent multiplexing while calculating (preserves LED display)
    signal  Dividend   :  std_logic_vector(15 downto 0) := (others => '0');
    -- Flag signal indicating division in progress
    signal  CalcFlag   :  std_logic := '0';
    -- Flag signal indicating fix remainder in progress
    signal  FixFlag    :  std_logic := '0';

    --  adder/subtracter signals
    signal  CalcResultBit  :  std_logic;        -- sum/difference output
    signal  CalcCarryOut   :  std_logic;        -- carry/borrow out
    signal  CarryFlag      :  std_logic;        -- stored carry flag


begin

    -- one-bit adder/subtracter (operation determined by op signal)
    -- adds/subtracts low bits of the dividend/divisor generating
    -- CalcResultBit and CalcCarryOut. 
    -- Note: when we are in run2_17 state of the FSM we set the divisor bit (DigitBits(16))
    --       to 0 since the divisor is only 16 bits and this is the 17th bit calculation. 
    CalcResultBit <= (DigitBits(16) and not run2_17) xor op xor Remainder(0) xor CarryFlag;
    CalcCarryOut  <= (Remainder(0) and CarryFlag) or
                     (((DigitBits(16) and not run2_17) xor op) and Remainder(0)) or
                     (((DigitBits(16) and not run2_17) xor op) and CarryFlag);
     
    -- This process handles changing of the operation between addition/subtraction for the 
    -- 1 bit adder/subtraction above. Additionally, this process handles storing the carry 
    -- from the adder into the specific CarryFlag signal. 
    process(CLK)
    begin

        if (rising_edge(CLK)) then

            -- initialize carry flag before each calculation starts during the init 
            -- state of the divider FSM (ShiftOp = ShiftRemCLR)
            if (ShiftOp = ShiftRemCLR) then
                CarryFlag <= '1';  -- preset carry during init
                op <= '1';         -- preset op to subtraction during init
            end if;

            -- set carry flag to the carry out when calculating
            if (ShiftOp = ShiftOpCALC or ShiftOp = ShiftOpCALC2 or ShiftOp = ShiftFixREM) then
                CarryFlag <= CalcCarryOut;
            end if;
            
            -- switch operation whenever FSM is in run3 and Carry xor op = 1.
            -- if FSM is currently in fix remainder state then hold operation
            -- at addition (op = 0).
            if (run3 = '1' and (CarryFlag xor op) = '1') then 
                if (std_match(ShiftOp, ShiftFixREM)) then 
                    op <= '0';
                else 
                    op <= not op;   
                end if;
            end if;

            -- otherwise carry flag holds its value
        end if;

    end process;



    -- This process handles edge (and key) detection on KeypadRdy for shifting 
    -- user keypad input into a register.
    process(CLK)
    begin

        if rising_edge(CLK) then

            -- shift the keypad ready signal to synchronize and edge detect
            KeypadRdyS  <=  KeypadRdyS(1 downto 0) & KeypadRdy;

            -- have a key if have one already that hasn't been processed or a
            -- new one is coming in (rising edge of KeypadRdy), reset if on
            -- the last clock of Digit 3 or Digit 7 (depending on position of
            -- Divisor switch) and held otherwise
            if  (std_match(KeypadRdyS, "01-")) then
                -- set HaveKey on rising edge of synchronized KeypadRdy
                HaveKey <=  '1';
            elsif ((DigitClkEn = '1') and (CurDigit = "0011") and (Divisor = '0')) then
                -- reset HaveKey if on Dividend and at end of digit 3
                HaveKey <=  '0';
            elsif ((DigitClkEn = '1') and (CurDigit = "0111") and (Divisor = '1')) then
                -- reset HaveKey if on Divisor and at end of digit 7
                HaveKey <=  '0';
            else
                -- otherwise hold the value
                HaveKey <=  HaveKey;
            end if;

        end if;

    end process;



    -- This process sets up the mux counter (MuxCntr) which has a rate of 
    -- approximately 1 KHz (1 MHz / 1024). MuxCntr is used for determining 
    -- what shift operation is being performed at any given time. Loops back 
    -- to 0 every 1 ms.
    process(CLK)
    begin

        -- count on the rising edge (clear on reset)
        if rising_edge(CLK) then
            if (nReset = '0') then
                MuxCntr <= (others => '0');
            else
                MuxCntr <= MuxCntr + 1;
            end if;
        end if;

    end process;


    -- the multiplex counter is also used for controlling the operation of
    -- the circuit - DigitClkEn signals the end of a multiplexed digit
    -- (MuxCntr = 3FF). The other states shown below are the operating 
    -- stages of the serial divider FSM. 

    -- DigitClkEn --> once every 1 ms for multiplexing
    DigitClkEn  <=  '1'  when (MuxCntr = "1111111111")  else
                    '0';
    -- RUN1 --> 1 clock cycle (looped x16)
    run1        <=  '1'  when (std_match(MuxCntr, "----011111"))  else 
                    '0';
    -- INIT --> 32 clock cycles (no loop) 
    init        <=  '1'  when (std_match(MuxCntr, "00000-----"))  else 
                    '0';
    -- RUN2_16 --> first 16 bits of division --> 16 clock cycles (looped x16)
    run2_16     <=  '1'  when (std_match(MuxCntr, "----10----"))  else 
                    '0';
    -- RUN2_17 --> last (17th) bit of division --> 1 clock cycle (looped x16)
    run2_17     <=  '1'  when (std_match(MuxCntr, "----110000"))  else 
                    '0';
    -- RUN3 --> 1 clock cycle (looped x16)
    run3        <=  '1'  when (std_match(MuxCntr, "----110001"))  else 
                    '0';
    -- DONE --> end of calculation (one occurance)
    done        <=  '1'  when (std_match(MuxCntr, "1111110010"))  else
                    '0';
    -- FIXREM16 --> first 16 bits --> 16 clock cycles (no loop)
    fixrem16    <=  '1'  when (std_match(MuxCntr, "000010----"))  else
                    '0';
    -- FIXREM17 --> last (17th) bit of remainder --> 1 clock cycle (no loop)
    fixrem17    <=  '1'  when (std_match(MuxCntr, "0000110000"))  else 
                    '0'; 



    -- This process creates a counter for outputing the correct digit.
    -- Order is 3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, then 8
    -- Reset counter to 3, only increment if DigitClkEn is active (every 1 ms)
    process (CLK)
    begin

        if (rising_edge(CLK)) then

            -- reset the decoder to 3 on reset
            if (nReset = '0') then
                CurDigit <= "0011";

            -- create the appropriate count sequence
            elsif (DigitClkEn = '1') then
                CurDigit(0) <= not CurDigit(0);
                CurDigit(1) <= CurDigit(1) xor not CurDigit(0);
                if (std_match(CurDigit, "0-00")) then
                    CurDigit(2) <= not CurDigit(2);
                end if;
                if (std_match(CurDigit, "-100") or std_match(CurDigit, "1-00")) then
                    CurDigit(3) <= not CurDigit(3);
                end if;

            -- otherwise hold the current value
            else
                CurDigit <= CurDigit;

            end if;
        end if;

    end process;


    -- always enable the digit decoder since using 29M16 and not CPLD to handle digit decoding. 
    DecoderEn  <=  '1';

    -- output the current digit to the digit decoder
    DecoderBits  <=  CurDigit;


    -- The digit being displayed at any instant is always the low nibble of our digit bits 
    -- shift register
    --
    -- when using the 16V8 for segment decoding, the hex digit to output is just the low 
    -- nibble of the shift register
    HexDigit  <=  DigitBits(3 downto 0);
    -- when using the CPLD for segment decoding, below is the logic minimized equations 
    -- converting each hex value(0-F) to 7-segment LED display. 
    segmenta  <=  not ((DigitBits(3) and not DigitBits(2) and not DigitBits(1)) or
		       (not DigitBits(3) and DigitBits(2) and DigitBits(0)) or 
		       (DigitBits(2) and DigitBits(1)) or
		       (DigitBits(3) and not DigitBits(0)) or
 		       (not DigitBits(3) and DigitBits(1)) or
		       (not DigitBits(2) and not DigitBits(0)));
    segmentb  <=  not ((not DigitBits(3) and not DigitBits(1) and not DigitBits(0)) or 
		       (DigitBits(0) and (DigitBits(3) xor DigitBits(1))) or
		       (not DigitBits(2) and not DigitBits(1)) or 
		       (not DigitBits(2) and not DigitBits(0)));
    segmentc  <=  not ((not DigitBits(3) and not DigitBits(1)) or 
		       (not DigitBits(3) and DigitBits(0)) or 
		       (not DigitBits(1) and DigitBits(0)) or 
		       (DigitBits(3) xor DigitBits(2)));
    segmentd  <=  not ((not DigitBits(3) and not DigitBits(2) and not DigitBits(0)) or 
		       (DigitBits(0) and (DigitBits(2) xor DigitBits(1))) or 
		       (DigitBits(2) and DigitBits(1) and not DigitBits(0)) or 
		       (DigitBits(3) and not DigitBits(1)));
    segmente  <=  not ((DigitBits(3) and DigitBits(2)) or 
		       (DigitBits(3) and DigitBits(1)) or 
		       (not DigitBits(2) and not DigitBits(0)) or 
		       (DigitBits(1) and not DigitBits(0)));	
    segmentf  <=  not ((not DigitBits(3) and DigitBits(2) and not DigitBits(1)) or 
		       (DigitBits(3) and DigitBits(1)) or 
		       (DigitBits(3) and not DigitBits(2)) or 
		       (DigitBits(2) and not DigitBits(0)) or 
		       (not DigitBits(1) and not DigitBits(0)));
    segmentg  <=  not ((not DigitBits(3) and DigitBits(2) and not DigitBits(1)) or
		       (DigitBits(3) and DigitBits(0)) or 
		       (DigitBits(3) and not DigitBits(2)) or 
		       (DigitBits(1) and not DigitBits(0)) or 
		       (not DigitBits(2) and DigitBits(1)));


    -- SHIFT REGISTER COMMANDS ==>
    -- ShiftDisplay: when DigitClkEn = 1 (multiplexes display such that each digit is 
    --                                   displayed for 1 ms)
    -- ShiftOpKEYIN: shift keypad input into respective dividend or divisor register  
    --
    -- ShiftOpCALC: run2 of divider FSM (remainder = remainder +- divisor)
    -- ShiftOpCALC2: run2 of divider FSM (remainder = remainder +- divisor) for bit 17 
    -- ShiftREMDIV:  run1 of divider FSM (shifts remainder and dividend)
    -- ShiftRemCLR:  init of divider FSM (clears remainder) 
    -- ShiftCQUO:    run3 of divider FSM (shifts carry into quotient)
    -- ShiftDIVDONE: finished stage of divider FSM (clears CalcFlag preventing other ops)
    -- 
    -- ShiftFixREM: fix remainder of divider FSM (remainder = remainder + divisor) 
    -- ShiftFixREM2: fix remainder of divider FSM (remainder = remainder + divisor) bit 17 
    --
    -- ShiftSWREM: shift remainder in ... quotient out (uses same init mux tracker 
    --                                                  signal as the divider FSM 
    --                                                  *wont be active simultaneously). 
    --
    
    ShiftOp  <=  ShiftOpSHIFT  when (DigitClkEn = '1')  else
                 ShiftOpKEYIN  when ((std_match(MuxCntr, "11111----0")) and
                                     (nCalculate = '1') and
                                     (HaveKey = '1') and
                                     (std_match(MuxCntr, "-----1011-") and
                                      (((CurDigit = "0011") and (Divisor = '0')) or
                                       ((CurDigit = "0111") and (Divisor = '1')))))  else
                 ShiftOpCALC   when ((run2_16 = '1') and
                                     (CalcFlag = '1') and
                                     (FixFlag = '0') and
                                     (CurDigit = "0011"))  else
                 ShiftOpCALC2  when ((run2_17 = '1') and 
                                     (CalcFlag = '1') and 
                                     (FixFlag = '0') and
                                     (CurDigit = "0011"))  else
                 ShiftREMDIV   when ((run1 = '1') and 
                                     (CalcFlag = '1') and 
                                     (FixFlag = '0') and
                                     (CurDigit = "0011"))  else 
                 ShiftRemCLR   when ((init = '1') and 
                                     (nCalculate = '0') and 
                                     (FixFlag = '0') and
                                     (CurDigit = "0011"))  else 
                 ShiftCQUO     when ((run3 = '1') and 
                                     (CalcFlag = '1') and
                                     (FixFlag = '0') and 
                                     (CurDigit = "0011"))  else
                 ShiftDIVDONE  when ((done = '1') and 
                                     (CalcFlag = '1') and 
                                     (CurDigit = "0011")) else
                 ShiftFixREM   when ((FixFlag = '1') and
                                     (fixrem16 = '1') and 
                                     (CurDigit = "0011")) else
                 ShiftFixREM2  when ((FixFlag = '1') and 
                                     (fixrem17 = '1') and 
                                     (CurDigit = "0011")) else
                 ShiftSWREM    when ((Divisor = '1') and 
                                     (init = '1') and 
                                     (CalcFlag = '0') and
				                     (nCalculate = '1') and
                                     (CurDigit = "0011")) else

                 ShiftOpHOLD;


    -- This next process implements instructions for each state of the FSM and defines the serial 
    -- divider as well as other shift operations detailed below. 
    --
    -- SHIFT REGISTER ==> DigitBits(47..0)
    --    @ CurDigit = 3:
    --    bits 15-0    dividend
    --    bits 31-16   divisor
    --    bits 47-32   result (quotient or remainder)
    --
    -- GENERIC OPERATIONS ==>
    --    ShiftOpHold:    DigitBits(47..0) = DigitBits(47..0)
    --                          * holds shift register (default state)
    --
    --    ShiftOpKeyIn:   DigitBits(47..0) = DigitBits(47..16) |
    --                                       DigitBits(11..0)  | Keypad(3..0)
    --                          * Shifts in keypad input
    --
    --    ShiftOpDisplay: DigitBits(47..0) = DigitBits(3..0) | DigitBits(47..4)
    --                          * Multiplex display (shift every 1 ms)
    --
    --
    -- DIVIDER OPERATIONS ==>
    --
    --    ShiftRemCLR:  Remainder(16..0) = 0 | Remainder(16..1)  -- clear remainder
    --                  CalcFlag = 1         -- signals commencement of division op)
    --
    --                  For first 16 clock cycles, store dividend in seperate register
    --                      Dividend(15..0) = Dividend(14..0) | DigitBits(15)
    --                      DigitBits(15..0) = DigitBits(14..0) | DigitBits(15)
    --                               * we store dividend in its own register to preserve 
    --                                 the LED display while multiplexing digit 3. 
    --
    --    ShiftREMDIV:  Remainder(16..0) = Remainder(15..0) | Dividend(15)
    --                  Dividend(15..0) = Dividend(14..0) | Dividend(15)
    --                               * Shift MSB of dividend into LSB of remainder
    -- 
    --    ShiftOpCalc:  DigitBits(31..16) = DigitBits(16) | DigitBits(31..17)
    --                  Remainder(16..0) = CalcResultBit | Remainder(16..1)
    --                               * Remainder = Remainder +- divisor
    --
    --    ShiftOpCalc2: Remainder(16..0) = CalcResultBit | Remainder(16..1)
    --                               * Remainder = Remainder +- divisor (dont shift bit 17)
    --
    --    ShiftCQUO:    DigitBits(47..32) = DigitBits(46..32) | CarryFlag
    --                               * Shift carry into Quotient
    --
    --    ShiftDIVDONE: CalcFlag = 0 * finished with calculation (unless fixing remainder)
    --                               * if carry is not set, set carry flag and fix remainder
    --
    -- FIX REMAINDER OPERATIONS ==>
    --    ShiftFixREM:  DigitBits(31..16) = DigitBits(16) | DigitBits(31..17)
    --                  Remainder(16..0) = CalcResultBit | Remainder(16..1)
    --                               * Remainder = Remainder + divisor 
    -- 
    --    ShiftFixREM2: Remainder(16..0) = CalcResultBit | Remainder(16..1)
    --                               * Remainer = Remainder + divisor (dont shift bit 17)
    -- 
    -- TOGGLE OUTPUT OPERATIONS ==>
    --    ShiftSWREM:   DigitBits(47..32) = Remainder(0) | DigitBits(47..33) 
    --                  Remainder(15..0) = Remainder(0) | Remainder(15..1)
    --                               * switches out quotient for remainder in the LED display
    process(CLK)
    begin

        -- shift on the rising edge
        if rising_edge(CLK) then
            case  ShiftOp  is
                -- generic operations
                when ShiftOpHOLD =>
                    DigitBits <= DigitBits;
                when ShiftOpKEYIN =>
                    DigitBits <= DigitBits(47 downto 16) &
                                 DigitBits(11 downto 0) & Keypad;
                when ShiftOpSHIFT =>
                    DigitBits <= DigitBits(3 downto 0) & DigitBits(47 downto 4);
                    
                -- division operations
                when ShiftOpCALC =>
                    DigitBits(31 downto 16) <= DigitBits(16) & DigitBits(31 downto 17);
                    Remainder <= CalcResultBit & Remainder(16 downto 1);
                    
                when ShiftOpCALC2 => 
                    Remainder <= CalcResultBit & Remainder(16 downto 1);
                    
                when ShiftRemCLR => 
                    Remainder <= '0' & Remainder(16 downto 1);      -- Clear remainder during init
                    CalcFlag <= '1';
                    if (std_match(MuxCntr, "000000----")) then 
                        Dividend <= Dividend(14 downto 0) & DigitBits(15);
                        DigitBits(15 downto 0) <= DigitBits(14 downto 0) & DigitBits(15);
                    end if;
                    
                when ShiftREMDIV => 
                    Remainder <= Remainder(15 downto 0) & Dividend(15);
                    Dividend(15 downto 0) <= Dividend(14 downto 0) & Dividend(15);
                    
                when ShiftCQUO   => 
                    DigitBits(47 downto 32) <= DigitBits(46 downto 32) & CarryFlag;
                    
                when ShiftDIVDONE=> 
                    CalcFlag <= '0'; 
                    if FixFlag = '0' then 
                        FixFlag <= not CarryFlag; -- determine is remainder needs to be fixed
                    else 
                        FixFlag <= '0';
                    end if;
                
                -- fix remainder operations
                when ShiftFixREM => 
                    DigitBits(31 downto 16) <= DigitBits(16) & DigitBits(31 downto 17);
                    Remainder <= CalcResultBit & Remainder(16 downto 1);
                when ShiftFixREM2 => 
                    Remainder <= CalcResultBit & Remainder(16 downto 1);
                    FixFlag <= '0';
                    
                -- toggle output operations
                when ShiftSWREM =>    
                    DigitBits(47 downto 32) <= Remainder(0) & DigitBits(47 downto 33);
                    Remainder(15 downto 0) <= Remainder(0) & Remainder(15 downto 1);   

              
                when others =>
            end case;
        end if;

    end process;
end  Behavioral;