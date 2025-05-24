@echo off

set datadeck=NONE
if exist PACT_prog_Src_Data.txt set datadeck=PACT_prog_Src_Data.txt

set userlib=NONE
if exist PACT_prog_Src_ASMB.txt set userlib=PACT_prog_Src_ASMB.txt

..\i701 run_PACT_debug.ini  PACT_prog_Src.txt  LOG  %datadeck%  %userlib%
if errorlevel 1 goto :err1

echo.
echo =========== All Ok
echo.
pause
goto end

:err1
echo.
echo =========== ERROR STOP
echo.
pause
goto end

:end


