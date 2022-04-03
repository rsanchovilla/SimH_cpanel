@echo off

..\..\s19tools\asm6800 -l -h microbasic

copy microbasic_c.txt+microbasic.hex microbasic.s19 > nul
del microbasic.hex > nul
echo.

pause
