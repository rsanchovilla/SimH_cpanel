@echo off

..\i701 build_PACT_compiler.ini
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


