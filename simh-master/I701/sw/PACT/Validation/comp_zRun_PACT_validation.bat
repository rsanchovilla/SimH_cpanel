@echo off
cd ..

call :Test Test_SC       
call :Test Test_Primes
call :Test Test_EXP
call :Test Test_EXP2
call :Test Test_LN
call :Test Test_LN2
call :Test Test_ARCTAN
call :Test Test_ARCTAN2
call :Test Test_SIN_COS
call :Test Test_SQRT
call :Test Test66        LOG
call :Test Test65  
call :Test Test64        LOG
call :Test Test63
call :Test Test62
call :Test Test61
call :Test Test60
call :Test Test59
call :Test Test58
call :Test Test57
call :Test Test56
call :Test Test55
call :Test Test54
call :Test Test53
call :Test Test52
call :Test Test51
call :Test Test50
call :Test Test49
call :Test Test48
call :Test Test47
call :Test Test46
call :Test Test45
call :Test Test44
call :Test Test43
call :Test Test42
call :Test Test41
call :Test Test40
call :Test Test39
call :Test Test38
call :Test Test37
call :Test Test36
call :Test Test35
call :Test Test34
call :Test Test33
call :Test Test32
call :Test Test31
call :Test Test30
call :Test Test29
call :Test Test28
call :Test Test27
call :Test Test26
call :Test Test25
call :Test Test24
call :Test Test23
call :Test Test22
call :Test Test21
call :Test Test20
call :Test Test19
call :Test Test18
call :Test Test17
call :Test Test16
call :Test Test15
call :Test Test14
call :Test Test13
call :Test Test12
call :Test Test11
call :Test Test10
call :Test Test9
call :Test Test8
call :Test Test7
call :Test Test6
call :Test Test5
call :Test Test4
call :Test Test3
call :Test Test2
call :Test Test1

echo.
echo ========= All Tests Ok
echo.
pause
goto :end

:err1
echo.
echo =========== ERROR STOP
echo.
goto :end

:err3
echo ERROR: PACT test program "pact_%1.txt" does not exists
pause
goto end

:err4
copy Tmp\print.txt Validation\pact_%1_print_Nok.txt   > nul 
echo ERROR: Program execution log is not the same 
pause
goto :end

:Test
echo.
echo ========= %1
echo.

if not exist Validation\pact_%1.txt goto err3

set DataDeck=NONE
if exist Validation\pact_%1_Data.txt set DataDeck=Validation\pact_%1_Data.txt

set LogMode=NOLOG
if "%2"=="LOG" set LogMode=LOG

..\i701 run_pact_debug.ini  Validation\pact_%1.txt  %LogMode%  %DataDeck%   EXIT 
if errorlevel 1 goto :err1

if not exist Validation\pact_%1_print_ok.txt     copy   Tmp\print.txt  Validation\pact_%1_print_ok.txt  
 
fc /L /A /W Tmp\print.txt   Validation\pact_%1_print_ok.txt  
if errorlevel 1 goto :err4
goto :end


:end


