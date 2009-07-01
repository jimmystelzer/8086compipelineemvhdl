------------------------------------------------------------------
-- Processador Intel 8086 simplificado arquiteturado em pipeline
------------------------------------------------------------------
-- Desenvolvedores: Jimmy Pinto Stelzer, Bruno Goulart e Bruno Paes
------------------------------------------------------------------

------------------------
-- Definiçõeses Gerais
------------------------
library IEEE;
use IEEE.std_logic_1164.all;

package p_intel_8086 is
	type instructions is
		(JMP,CMP,JZ,LOOPNZ,HLT,ADD,MOVRR,MOVRI,MOVRM,NOP);
	type microinstruction is record
		op:	instructions; 	-- meneumonicos	
		rsCode: STD_LOGIC_VECTOR (31 downto 0);
		rdCode: STD_LOGIC_VECTOR (31 downto 0);
		param: STD_LOGIC_VECTOR (31 downto 0);
		modc: std_logic;
		w: std_logic;
		ex: std_logic;
		mem: std_logic;
		wb: std_logic;
	end record;
end p_intel_8086;

-------------------------------------
-- Registrador de 16bits para AX, BX, CX, DX, SP, BP, SI, DI, ES, DS, SS e ES.
-------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;

entity reg16 is
	generic( INIT_VALUE : STD_LOGIC_VECTOR(15 downto 0) := (others=>'0') );
	port(  clock, reset, enable : in std_logic;
		D : in  STD_LOGIC_VECTOR (15 downto 0);
		Q : out STD_LOGIC_VECTOR (15 downto 0)
	);
end reg16;

architecture reg16 of reg16 is
begin
	process(clock, reset)
	begin
		if reset = '1' then
			Q <= INIT_VALUE(15 downto 0);
		elsif clock'event and clock = '0' then
			if enable = '1' then
				Q <= D;
			end if;
		end if;
	end process;
end reg16;

-------------------------------------
-- Banco de Registradores
-------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;   
use work.p_intel_8086.all;

entity reg_bank is
	port( clock, reset, enable : in std_logic;
		RsCode, RdCode : in std_logic_vector( 2 downto 0);
		RD : in std_logic_vector(15 downto 0);
		R1, R2: out std_logic_vector(15 downto 0));
end reg_bank;

architecture reg_bank of reg_bank is
	type bank is array(0 to 7) of std_logic_vector(15 downto 0);
	signal reg : bank;
begin
	g1: for i in 0 to 7 generate
			rx: entity work.reg16 port map(clock=>clock, reset=>reset, enable=>'1', D=>RD, Q=>reg(i));
	end generate g1;
	R1 <= reg(CONV_INTEGER(RsCode));
	R2 <= reg(CONV_INTEGER(RdCode));
end reg_bank;

-------------------------------------
-- ULA
-------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.std_logic_arith.all;
use work.p_intel_8086.all;

entity alu is
	port( op1, op2, param : in std_logic_vector(15 downto 0);
		dest : out std_logic_vector(15 downto 0);   
		opalu : in instructions;
		w : in std_logic
	);
end alu;

architecture alu of alu is 
	signal flags : std_logic_vector(15 downto 0) := (others=>'0');
	signal soma : std_logic_vector(15 downto 0) := (others=>'0');
begin
	-- FLAGS
	soma <= (op1+op2);
	-- ZF
	flags(6) <= '1' when (op2-op1)=0 and (opalu=ADD or opalu=CMP or opalu=LOOPNZ) else
				'0' when (op2-op1)/=0 and (opalu=ADD or opalu=CMP or opalu=LOOPNZ);
	-- SF
	flags(7) <= '1' when soma<0 and opalu=ADD else
				'0' when soma>0 and opalu=ADD;
	-- CF
	flags(0) <= '1' when soma>255 and w='0' and opalu=ADD else
				'0' when soma<255 and w='0' and opalu=ADD;
	-- OF
	flags(11) <= '1' when (soma<-128 or (op1+op2)>127) and w='0' and opalu=ADD else
				 '0' when (soma>-128 and (op1+op2)<127) and w='0' and opalu=ADD;
	-- AF
	flags(4) <= '1' when soma>65535 and w='1' and opalu=ADD else
				'0' when soma<65535 and w='1' and opalu=ADD;
	-- PF
	flags(2) <= '1' when CONV_INTEGER(soma) mod 2=0 and w='0' and opalu=ADD else
				'0' when CONV_INTEGER(soma) mod 2/=0 and w='0' and opalu=ADD;
	--(JMP,CMP,JZ,LOOPNZ,HLT,ADD,MOVRR,MOVRI,MOVRM,NOP)
	dest <=  
		op1 + op2		when opalu=ADD else
		op2				when opalu=MOVRR else
		param			when opalu=MOVRI else
		op1 - 2			when opalu=LOOPNZ else -- CX--
		(others=>'1')	when opalu=JZ and flags(6)='1' else -- seta dest
		(others=>'0'); --nop, hlt, cmp, movrm, jmp,

end alu;

-----------------------------------------
--  Control
-----------------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
use work.p_intel_8086.all;

entity control_unit is
	port(clock, reset : in std_logic;          
		uins : out microinstruction;
		ir : in std_logic_vector(39 downto 0)
		);
end control_unit;
                   
architecture control_unit of control_unit is
	signal i : instructions;      
begin 
	i <=	MOVRI	when ir(39 downto 36)="1011" else
			MOVRR	when ir(39 downto 34)="100010" else
			ADD		when ir(39 downto 34)="000000" else
			CMP		when ir(39 downto 34)="100000" else
			MOVRM	when ir(39 downto 33)="0110011" else
			HLT		when ir(39 downto 32)="11110100" else
			LOOPNZ	when ir(39 downto 32)="11100000" else
			JZ		when ir(39 downto 32)="01110100" else
			JMP		when ir(39 downto 32)="11101001" else
			NOP;
	uins.op <= i;
	
	-- execution
	uins.ex <=	'1'	when i/=NOP and i/=JMP else
				'0';
	-- write back
	uins.wb <=	'0'	when i=NOP or i=HLT or i=JZ or i=CMP or i=JMP else
				'1';
	-- memory
	uins.mem <=	'1'	when i=MOVRM else
				'0';

	-- w
	--uins.w	<=	
		--rsCode: STD_LOGIC_VECTOR (31 downto 0);
		--rdCode: STD_LOGIC_VECTOR (31 downto 0);
		--param: STD_LOGIC_VECTOR (31 downto 0);
		--modc: std_logic;
end control_unit;