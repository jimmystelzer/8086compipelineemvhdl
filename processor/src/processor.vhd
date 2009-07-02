------------------------------------------------------------------
-- Processador Intel 8086 arquiteturado em pipeline	e simplificado
------------------------------------------------------------------
-- Desenvolvedores: Jimmy Pinto Stelzer, Bruno Goulart e Bruno Paes
-- Baseado no exemplo de implementação do MIPS dos professores Fernando Moraes e Ney Calazans.
------------------------------------------------------------------

------------------------
-- Definições Gerais
------------------------
library IEEE;
use IEEE.std_logic_1164.all;

package p_intel_8086 is
	type instructions is
		(JMP,CMP,JZ,LOOPNZ,HLT,ADD,MOVRR,MOVRI,MOVRM,NOP);
	type microinstruction is record
		op:	instructions; 	-- meneumonicos	
		rsCode: STD_LOGIC_VECTOR (2 downto 0);
		rdCode: STD_LOGIC_VECTOR (2 downto 0);
		param: STD_LOGIC_VECTOR (31 downto 0);
		modc: STD_LOGIC_VECTOR (1 downto 0);
		w: std_logic;
		ex: std_logic;
		mem: std_logic;
		wb: std_logic;
	end record;
end p_intel_8086;		  

-------------------------------------
-- Registrador do pipeline
-------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use work.p_intel_8086.all;

entity regpl is
	generic( INIT_VALUE : STD_LOGIC_VECTOR(83 downto 0) := (others=>'0') );
	port(  clock, reset, enable : in std_logic;
		D : in  STD_LOGIC_VECTOR (83 downto 0);
		I : in instructions;
		O : out instructions; 
		Q : out STD_LOGIC_VECTOR (83 downto 0)
	);
end regpl;

architecture regpl of regpl is
begin
	process(clock, reset)
	begin
		if reset = '1' then
			Q <= INIT_VALUE(83 downto 0);
			O <= NOP;
		elsif clock'event and clock = '0' then
			if enable = '1' then
				Q <= D;
				O <= I;
			end if;
		end if;
	end process;
end regpl;

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
		w : in std_logic;
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
	R1 <= 	reg(CONV_INTEGER(RsCode)) when w='1' else --AX CX DX BX SP BP SI DI
			"00000000" & reg(0)(7 downto 0) when CONV_INTEGER(RsCode)=0 and w='0' else	 --AL
			"00000000" & reg(1)(7 downto 0) when CONV_INTEGER(RsCode)=0 and w='0' else	 --CL
			"00000000" & reg(2)(7 downto 0) when CONV_INTEGER(RsCode)=0 and w='0' else	 --DL
			"00000000" & reg(3)(7 downto 0) when CONV_INTEGER(RsCode)=0 and w='0' else	 --BL
			"00000000" & reg(4)(15 downto 8) when CONV_INTEGER(RsCode)=0 and w='0' else --AH
			"00000000" & reg(5)(15 downto 8) when CONV_INTEGER(RsCode)=0 and w='0' else --CH
			"00000000" & reg(6)(15 downto 8) when CONV_INTEGER(RsCode)=0 and w='0' else --DH
			"00000000" & reg(7)(15 downto 8) when CONV_INTEGER(RsCode)=0 and w='0';     --BH
	R2 <=	reg(CONV_INTEGER(RdCode)) when w='1' else --AX CX DX BX SP BP SI DI
			"00000000" & reg(0)(7 downto 0) when CONV_INTEGER(RdCode)=0 and w='0' else	 --AL
			"00000000" & reg(1)(7 downto 0) when CONV_INTEGER(RdCode)=0 and w='0' else	 --CL
			"00000000" & reg(2)(7 downto 0) when CONV_INTEGER(RdCode)=0 and w='0' else	 --DL
			"00000000" & reg(3)(7 downto 0) when CONV_INTEGER(RdCode)=0 and w='0' else	 --BL
			"00000000" & reg(4)(15 downto 8) when CONV_INTEGER(RdCode)=0 and w='0' else --AH
			"00000000" & reg(5)(15 downto 8) when CONV_INTEGER(RdCode)=0 and w='0' else --CH
			"00000000" & reg(6)(15 downto 8) when CONV_INTEGER(RdCode)=0 and w='0' else --DH
			"00000000" & reg(7)(15 downto 8) when CONV_INTEGER(RdCode)=0 and w='0';     --BH
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
	port( op1, op2 : in std_logic_vector(15 downto 0);
		param : in std_logic_vector(31 downto 0);
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
		param(31 downto 16)			when opalu=MOVRI else
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
	signal di : std_logic;
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
	uins.w	<=	ir(35) when i=MOVRI else
				ir(32) when i=MOVRM or i=CMP or i=ADD or i=MOVRR else 
				--'1' when i=LOOPNZ else	--?
				'X'; --Unknown
	-- mod
	uins.modc <=	ir(31 downto 30) when i=MOVRM or i=CMP or i=ADD or i=MOVRR else
					"XX"; -- Unknown
	-- di
	di <=	ir(33) when i=CMP or i=ADD or i=MOVRR else
			'X';
		
	-- rdCode
	uins.rdCode <=	ir(34 downto 32) when i=MOVRI else
					ir(26 downto 24) when i=MOVRM or (di='1' and (i=CMP or i=ADD or i=MOVRR)) else
					ir(29 downto 27) when di='0' and (i=CMP or i=ADD or i=MOVRR) else
					"001" when i=LOOPNZ else	
					(others=>'X');
	-- rsCode
	uins.rsCode <=	ir(26 downto 24) when i=MOVRM or (di='0' and (i=CMP or i=ADD or i=MOVRR)) else
					ir(29 downto 27) when di='1' and (i=CMP or i=ADD or i=MOVRR) else
					(others=>'X');
	
	-- param	-- usado para carregar as informações como offset(-low|-high), seg(-low|-high), data(-low|-high), addr(-low|-high), disp
	uins.param	<=	"00000000" & "00000000" & "00000000" & ir(31 downto 24) when (i=JZ or i=LOOPNZ) and ir(31)='0' else
					"11111111" & "11111111" & "11111111" & ir(31 downto 24) when (i=JZ or i=LOOPNZ) and ir(31)='1' else
					"00000000" & "00000000" & "00000000" & ir(23 downto 16) when i=CMP and ir(23)='0' else
					"11111111" & "11111111" & "11111111" & ir(23 downto 16) when i=CMP and ir(23)='1' else
					"00000000" & ir(7 downto 0) & ir(15 downto 8) & ir(23 downto 16) when i=MOVRM and ir(23)='0' else
					"11111111" & ir(7 downto 0) & ir(15 downto 8) & ir(23 downto 16) when i=MOVRM and ir(23)='1' else	
					ir(7 downto 0) & ir(15 downto 8) & ir(23 downto 16) & ir(31 downto 24) when i=MOVRI else
					(others=>'X');

--39 38 37 36 35 34 33 32 31 30 29 28 27 26 25 24
--1  0  1  1  w  d  d  d							MOVRI		Param(7-0 & 15-8 & 23-16 & 31-24) se w = 1 else Param(31-24)																	
--0  1  1  0  0  1  1  w  m  m  s  s  s  d  d  d	MOVRM		Param(7-0 & 15-8 & 23-16)																	
--1  0  0  0  0  0  di w  m  m  s  s  s  d  d  d	CMP			Param(23-16) se di = 0 inverte sss e ddd																	
--0  0  0  0  0  0  di w  m  m  s  s  s  d  d  d	ADD			Param() se di = 0 inverte sss e ddd																	
--1  0  0  0  1  0  di w  m  m  s  s  s  d  d  d	MOVRR		Param() se di = 0 inverte sss e ddd																	
--1  1  1  1  0  1  0  0							HLT																			
--1  1  1  0  0  0  0  0							LOOPNZ		Param(31-24) | w = 1 | rd = 001																	
--0  1  1  1  0  1  0  0							JZ			Param(31-24) | w=0																	
--1  1  1  0  1  0  0  1							JMP			Param(7-0 & 15-8 & 23-16 & 31-24)																	
--													NOP																					
																												
end control_unit;

---------------------------
-- BIU - Bus Interface Unit (simplificada)
---------------------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;

entity biu is
	port(	addr	: in    std_logic_vector(19 downto 0); -- endereço
			clock	: in    std_logic;					
			reset	: in    std_logic;
			mode	: in    std_logic;					-- modo -> 0 = leitura; 1 = escrita
			mtype	: in    std_logic;					-- tipo -> 0 = instruções; 1 = dados;
			memc	: inout std_logic_vector(0 to 1023); -- 1024bits -> 128bytes -- entrada e saida da memoria de instruções
			memd	: inout std_logic_vector(0 to 1023); -- 1024bits -> 128bytes -- entrada e saida da memoria de dados
			datain	: in	std_logic_vector(39 downto 0); -- dados a serem gravados
			dataout	: out   std_logic_vector(39 downto 0)  -- dados lidos
		);
end biu;

architecture biu of biu is
	signal datai : std_logic_vector(39 downto 0);
	signal posi	 : integer;
	signal posf	 : integer;
begin
	gen : process(clock,reset)
	begin
		if (reset = '1') then
			datai <= (others=>'X');
			posi <= 0;
			posf <= 0;
		elsif(clock'event and clock='1') then
			case mtype is
				when '0' =>	-- memoria de instruções
					if mode='0' then -- leitura	
						posi <= CONV_INTEGER(addr);
						posf <= CONV_INTEGER(addr) + 39;
						datai <= memc(posi to posf);
					elsif mode='1' then -- escrita
						posi <= CONV_INTEGER(addr);
						posf <= CONV_INTEGER(addr) + 39;
						memc(posi to posf) <= datain;
					end if; 
				when '1' =>	-- memoria de dados
					if mode='0' then -- leitura	
						posi <= CONV_INTEGER(addr);
						posf <= CONV_INTEGER(addr) + 39;
						datai <= memd(posi to posf);
					elsif mode='1' then -- escrita
						posi <= CONV_INTEGER(addr);
						posf <= CONV_INTEGER(addr) + 39;
						memd(posi to posf) <= datain;
					end if;
				when others =>
						datai <= (others=>'X');
			end case;
		end if;
	end process;
	dataout<= datai;
end biu;

--------------------------------
-- Chip
--------------------------------
library IEEE;
use IEEE.Std_Logic_1164.all;
use work.p_intel_8086.all;

entity chip is
      port(  clock, reset :	in std_logic;
             memc : inout std_logic_vector(0 to 1023); -- 1024bits -> 128bytes -- entrada e saida da memoria de instruções
			 memd : inout std_logic_vector(0 to 1023) -- 1024bits -> 128bytes -- entrada e saida da memoria de dados
          );
end chip;	

architecture chip of chip is
	signal IP,addr: std_logic_vector(19 downto 0) := (others=>'0');
	signal mode,mtype,wrb,walu,enablerb: std_logic := 'X';
	signal datain,dataout,ir: std_logic_vector(39 downto 0) := (others=>'X');
	signal uins: microinstruction;
	signal opalu: instructions;
	signal RD,R1,R2,op1,op2,dest : std_logic_vector(15 downto 0);
	signal param : std_logic_vector(31 downto 0);
	signal RsCode, RdCode : std_logic_vector(2 downto 0);
begin

	biu: entity work.biu port map(addr=>addr, clock=>clock, reset=>reset, mode=>mode, mtype=>mtype, memc=>memc, memd=>memd,datain=>datain,dataout=>dataout);
	ctrl: entity work.control_unit port map(clock=>clock, reset=>reset, uins=>uins, ir=>ir);
   	alu: entity work.alu port map(op1=>op1, op2=>op2, param=>param, dest=>dest, opalu=>opalu, w=>walu);
	rb: entity work.reg_bank port map(clock=>clock, reset=>reset, enable=>enablerb, w=>wrb, RsCode=>RsCode, RdCode=>RdCode, RD=>RD, R1=>R1,R2=>R2);


--entity regpl is
--	port(  clock, reset, enable : in std_logic;
--		D : in  STD_LOGIC_VECTOR (83 downto 0);
--		I : in instructions;
--		O : out instructions; 
--		Q : out STD_LOGIC_VECTOR (83 downto 0)
--	);

end chip;