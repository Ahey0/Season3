-- Clock controll --
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_Unsigned.all;
Entity Clock_controll_50MHZ_T_50HZ is 
  port(
        clkin   : in  std_logic     ;
        EN      : in  std_logic     ;
        reset   : in  std_logic     ;
        clkout  : out std_logic     
       );
End Entity;
Architecture Clock_Controll_50MHZ_T_50HZ_Behave of Clock_controll_50MHZ_T_50HZ is
signal clkout_s : std_logic := '0' ;
Begin 
Process (clkin)
variable CH : integer range 0 to 1000000 := 0  ; 
    begin
     if EN='1' then
        if reset='0' then 
         if clkin'event and clkin='1' then
             if CH = 1000000 then 
                CH := 0 ;
                clkout_s <= Not clkout_s ;
             else
                CH := CH + 1 ;
             end if;  
         end if ;
        else 
         CH := 0 ;
        end if;
      end if;
end process;
clkout <= clkout_s ;
End Architecture;
-- Converter --
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;
Entity CONVERTER is
port(
     INS      : in  std_logic_vector(15 downto 0)  ;
	  datadig1 : out integer  range 0 to 9          ;
	  datadig2 : out integer  range 0 to 9          ;
	  datadig3 : out integer  range 0 to 9          ;
	  datadig4 : out integer  range 0 to 9          ;
	  datadig5 : out integer  range 0 to 9          ;
	  clk      : in  std_logic
	  );
End Entity;
Architecture CONVERTER_Behave of CONVERTER is
signal data1     :  integer range 0 to 65535        ;
signal data2     :  integer range 0 to 65535        ;
signal data3     :  integer range 0 to 65535        ;
signal data4     :  integer range 0 to 65535        ;
signal data5     :  integer range 0 to 65535        ;
signal data6     :  integer range 0 to 65535        ;
signal OUTS     :  std_logic_vector(15 downto 0)  ;
Begin
		  OUTS      <= INS ;
		  data1      <= conv_integer(unsigned(OUTS));
		  datadig1  <= data1 rem 10  ;
		  data2      <= data1/10     ;
        datadig2  <= data2 rem 10  ;
		  data3      <= data2/10     ;
    	  datadig3  <= data3 rem 10  ;
        data4      <= data3/10     ;
		  datadig4  <= data4 rem 10  ;
		  data5      <= data4/10     ; 
		  datadig5  <= data5 rem 10  ;
End Architecture;
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use ieee.std_logic_arith.all;
Entity SDST is
port(
     INS     : in  std_logic_vector(15 downto 0)  ;
	  dig1    : out std_logic_vector(6 downto 0)   ;
	  dig2    : out std_logic_vector(6 downto 0)   ;
	  dig3    : out std_logic_vector(6 downto 0)   ;
	  dig4    : out std_logic_vector(6 downto 0)   ;
	  dig5    : out std_logic_vector(6 downto 0)   ;
	  clk     : in  std_logic
	  );
End Entity;
Architecture SDST_Behave of SDST is
component CONVERTER is
port(
     INS      : in  std_logic_vector(15 downto 0)  ;
	  datadig1 : out integer  range 0 to 9          ;
	  datadig2 : out integer  range 0 to 9          ;
	  datadig3 : out integer  range 0 to 9          ;
	  datadig4 : out integer  range 0 to 9          ;
	  datadig5 : out integer  range 0 to 9          ;
	  clk      : in  std_logic
	  );
End Component;
signal S1,S2,S3  :  integer  range 0 to 9          ;
signal S4,S5     :  integer  range 0 to 9          ;
Begin
U1: CONVERTER port map (INS,S1,S2,S3,S4,S5,clk)    ;
process(INS,S1,S2,S3,S4,S5,clk)
 Begin
  if clk'event and clk='1' then 
      Case S1 IS 
		    When 0 =>
			     dig1<=Not "0111111" ;
			 when 1 => 
	           dig1<=Not "0000110" ;
			 when 2 =>
	           dig1<=Not "1011011" ;
		    when 3 =>
	           dig1<=Not "1001111" ;
		    when 4 =>
	           dig1<=Not "1100110" ;
		  	 when 5 =>
              dig1<=Not "1101101" ;
			 when 6 =>
	           dig1<=Not "1111101" ;
			 when 7 =>
	           dig1<=Not "0000111" ;
			 when 8 => 
	           dig1<=Not "1111111" ;
			 when 9 =>
	           dig1<=Not "1101111" ;		
		 End case;
		Case S2 IS 
		    When 0 =>
			     dig2<=Not "0111111" ;
			 when 1 => 
	           dig2<=Not "0000110" ;
			 when 2 =>
	           dig2<=Not "1011011" ;
		    when 3 =>
	           dig2<=Not "1001111" ;
		    when 4 =>
	           dig2<=Not "1100110" ;
		  	 when 5 =>
              dig2<=Not "1101101" ;
			 when 6 =>
	           dig2<=Not "1111101" ;
			 when 7 =>
	           dig2<=Not "0000111" ;
			 when 8 => 
	           dig2<=Not "1111111" ;
			 when 9 =>
	           dig2<=Not "1101111" ;		
		 End case; 
		 Case S3 IS 
		    When 0 =>
			     dig3<=Not "0111111" ;
			 when 1 => 
	           dig3<=Not "0000110" ;
			 when 2 =>
	           dig3<=Not "1011011" ;
		    when 3 =>
	           dig3<=Not "1001111" ;
		    when 4 =>
	           dig3<=Not "1100110" ;
		  	 when 5 =>
              dig3<=Not "1101101" ;
			 when 6 =>
	           dig3<=Not "1111101" ;
			 when 7 =>
	           dig3<=Not "0000111" ;
			 when 8 => 
	           dig3<=Not "1111111" ;
			 when 9 =>
	           dig3<=Not "1101111" ;		
		 End case;
		Case S4 IS 
		    When 0 =>
			     dig4<=Not "0111111" ;
			 when 1 => 
	           dig4<=Not "0000110" ;
			 when 2 =>
	           dig4<=Not "1011011" ;
		    when 3 =>
	           dig4<=Not "1001111" ;
		    when 4 =>
	           dig4<=Not "1100110" ;
		  	 when 5 =>
              dig4<=Not "1101101" ;
			 when 6 =>
	           dig4<=Not "1111101" ;
			 when 7 =>
	           dig4<=Not "0000111" ;
			 when 8 => 
	           dig4<=Not "1111111" ;
			 when 9 =>
	           dig4<=Not "1101111" ;		
		 End case; 
		 Case S5 IS 
		    When 0 =>
			     dig5<=Not "0111111" ;
			 when 1 => 
	           dig5<=Not "0000110" ;
			 when 2 =>
	           dig5<=Not "1011011" ;
		    when 3 =>
	           dig5<=Not "1001111" ;
		    when 4 =>
	           dig5<=Not "1100110" ;
		  	 when 5 =>
              dig5<=Not "1101101" ;
			 when 6 =>
	           dig5<=Not "1111101" ;
			 when 7 =>
	           dig5<=Not "0000111" ;
			 when 8 => 
	           dig5<=Not "1111111" ;
			 when 9 =>
	           dig5<=Not "1101111" ;		
		 End case;
		End if;
End process;
End Architecture;
Library ieee;
Use ieee.std_logic_1164.all;
Use ieee.std_logic_unsigned.all;
Entity Seg is 
 port(
      Inp     : in  Std_logic_vector(7 downto 0)   ;
		HOL     : in  std_logic                      ;
		LoadD   : in  std_logic                      ;
	   dig1    : out std_logic_vector(6 downto 0)   ;
	   dig2    : out std_logic_vector(6 downto 0)   ;
	   dig3    : out std_logic_vector(6 downto 0)   ;
	   dig4    : out std_logic_vector(6 downto 0)   ;
	   dig5    : out std_logic_vector(6 downto 0)   ;
	   clk     : in  std_logic
	  );
End Entity ;
Architecture Seg_behave Of Seg is 
Component  SDST is
port(
     INS     : in  std_logic_vector(15 downto 0)  ;
	  dig1    : out std_logic_vector(6 downto 0)   ;
	  dig2    : out std_logic_vector(6 downto 0)   ;
	  dig3    : out std_logic_vector(6 downto 0)   ;
	  dig4    : out std_logic_vector(6 downto 0)   ;
	  dig5    : out std_logic_vector(6 downto 0)   ;
	  clk     : in  std_logic
	  );
End component;
Component  Clock_controll_50MHZ_T_50HZ is 
  port(
        clkin   : in  std_logic     ;
        EN      : in  std_logic     ;
        reset   : in  std_logic     ;
        clkout  : out std_logic     
       );
End component;
Signal clkc   : std_logic:='0'                 ;
signal H_data : std_logic_vector(7  downto 0)  ;
signal L_data : std_logic_vector(7  downto 0)  ;
Signal M_data : std_logic_vector(15 downto 0)  ;
Begin
Process (LoadD,HOL)
 Begin 
		    if loadD='1' then 
			    if HOL='0' then 
				   L_Data <= INp ;
				 Elsif HOL='1' then	
		         H_Data <= INp ;
				 End if ;
			End if;
	End process;
M_Data <= H_Data & L_data ;
QW1 : SDST  port map (M_Data,dig1,dig2,dig3,dig4,dig5,clkc);
QW2 : Clock_controll_50MHZ_T_50HZ port map (clk,'1','0',clkc);
End Architecture;
	  