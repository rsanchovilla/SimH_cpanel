rem This run.bat file converts source bitmaps for control panels on .jpg files
rem to be used by SimH Control Panel Definition Files
rem Using Nconvert v7.00 (http://www.xnview.com)

set conv=NConvert\nconvert.exe

cd ..

%conv% -overwrite -out jpeg -q 95 -resize 512 601 -o IBM704_State0_512p.jpg     bak\IBM704_State0.png
%conv% -overwrite -out jpeg -q 95 -resize 512 601 -o IBM704_State1_512p.jpg     bak\IBM704_State1.png
%conv% -overwrite -out jpeg -q 95 -resize 512 601 -o IBM704_State0B_512p.jpg    bak\IBM704_State0B.png
%conv% -overwrite -out jpeg -q 95 -resize 512 601 -o IBM704_State1B_512p.jpg    bak\IBM704_State1B.png

%conv% -overwrite -out jpeg -q 95 -resize 25%% 25%% -o IBM727_panel_25p.jpg     bak\IBM727_panel.png
%conv% -overwrite -out jpeg -q 95 -resize 25%% 25%% -o IBM727_tape_25p.jpg      bak\IBM727_tape.png

%conv% -overwrite -out jpeg -q 95 -resize 512 601 -o IBM709_State0_512p.jpg     bak\IBM709_State0.png
%conv% -overwrite -out jpeg -q 95 -resize 512 657 -o IBM709_State1_512p.jpg     bak\IBM709_State1.png
%conv% -overwrite -out jpeg -q 95 -resize 512 601 -o IBM709_State0B_512p.jpg    bak\IBM709_State0B.png
%conv% -overwrite -out jpeg -q 95 -resize 512 657 -o IBM709_State1B_512p.jpg    bak\IBM709_State1B.png

%conv% -overwrite -out jpeg -q 95 -resize 25%% 25%% -o IBM729_panel_25p.jpg     bak\IBM729_panel.png
%conv% -overwrite -out jpeg -q 95 -resize 25%% 25%% -o IBM729_tape_25p.jpg      bak\IBM729_tape.png

%conv% -overwrite -out jpeg -q 95 -resize 512 601 -o IBM766_State0_512p.jpg     bak\IBM766_State0.png
%conv% -overwrite -out jpeg -q 95 -resize 512 601 -o IBM766_State0B_512p.jpg    bak\IBM766_State0B.png
%conv% -overwrite -out jpeg -q 95 -resize 512 601 -o IBM766_State1_512p.jpg     bak\IBM766_State1.png
%conv% -overwrite -out jpeg -q 95 -resize 512 601 -o IBM766_State1B_512p.jpg    bak\IBM766_State1B.png

%conv% -overwrite -out jpeg -q 95 -resize 25%% 25%% -o IBM7090_State0B_25p.jpg  bak\IBM7090_State0B.png
%conv% -overwrite -out jpeg -q 95 -resize 25%% 25%% -o IBM7090_State1B_25p.jpg  bak\IBM7090_State1B.png

%conv% -overwrite -out jpeg -q 95 -resize 25%% 25%% -o IBM7617_State0B_25p.jpg  bak\IBM7617_State0B.png
%conv% -overwrite -out jpeg -q 95 -resize 25%% 25%% -o IBM7617_State1B_25p.jpg  bak\IBM7617_State1B.png

%conv% -overwrite -out jpeg -q 95 -resize 25%% 25%% -o IBM7631_State0B_25p.jpg  bak\IBM7631_State0B.png
%conv% -overwrite -out jpeg -q 95 -resize 25%% 25%% -o IBM7631_State1B_25p.jpg  bak\IBM7631_State1B.png

%conv% -overwrite -out jpeg -q 95 -resize 75%% 75%% -o IBM2302_cabinet_75p.jpg  bak\IBM2302_cabinet.png
%conv% -overwrite -out jpeg -q 95 -resize 75%% 75%% -o IBM2302_head_75p.jpg     bak\IBM2302_head.png

%conv% -overwrite -out jpeg -q 95 -resize 80%% 80%% -o IBM711_panel_80p.jpg     bak\IBM711_panel.png

