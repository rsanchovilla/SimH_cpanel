cd img
for %%a in (*.png) do ..\nconvert -overwrite -out bmp -truecolors -o "%%~na.bmp" "%%~na.png" 

pause