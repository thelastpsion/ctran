local: src/ctran.pp src/PsionOOParser.pas src/StringThings.pas src/PsionSDKApp.pas src/PsionOOCatDiagnostics.pas
	mkdir -p bin/local/
	fpc -gl -FEbin/local/ src/ctran.pp
	cp bin/local/ctran .

all: local go32v2

go32v2: src/ctran.pp src/PsionOOParser.pas src/StringThings.pas src/PsionSDKApp.pas src/PsionOOCatDiagnostics.pas
	mkdir -p bin/go32v2/
	fpc -Pi386 -Tgo32v2 -FEbin/go32v2/ src/ctran.pp
	cp bin/go32v2/ctran.exe .

