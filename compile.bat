@del spaceshooter.o
@del spaceshooter.nes
@del spaceshooter.map.txt
@del spaceshooter.labels.txt
@del spaceshooter.nes.ram.nl
@del spaceshooter.nes.0.nl
@del spaceshooter.nes.1.nl
@del *.nes.dbg
@echo.
@echo Compiling...
\cc65\bin\ca65 spaceshooter.s -g -o spaceshooter.o
@IF ERRORLEVEL 1 GOTO failure
@echo.
@echo Linking...
\cc65\bin\ld65 -o spaceshooter.nes -C spaceshooter.cfg spaceshooter.o -m spaceshooter.map.txt -Ln spaceshooter.labels.txt --dbgfile spaceshooter.nes.dbg
@IF ERRORLEVEL 1 GOTO failure
@echo.
@echo Success!
@GOTO endbuild
:failure
@echo.
@echo Build error!
:endbuild