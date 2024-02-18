local: ctran.pp PsionOOLexer.pas
	fpc ctran.pp

all: local go32v2

go32v2: ctran.pp PsionOOLexer.pas
	fpc -Pi386 -Tgo32v2 ctran.pp

