unit FMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, ExtDlgs,
  OCRMain;

type
  TFoMain = class(TForm)
    BStart: TButton;
    ImSrc: TImage;
    ImDisp: TPaintBox;
    Timer300ms: TTimer;
    BLoad: TButton;
    BDoOCR: TButton;
    LProgress: TLabel;
    PageControl: TPageControl;
    TabLog: TTabSheet;
    TLog: TMemo;
    TabMatch: TTabSheet;
    TMatch: TMemo;
    TabText: TTabSheet;
    TOcrText: TMemo;
    LMsg: TLabel;
    OpenInSrcDialog: TOpenPictureDialog;
    BDoOCRall: TButton;
    LPage: TLabel;
    BPgUp: TButton;
    BPgDn: TButton;
    BStep: TButton;
    TabOptions: TTabSheet;
    BPgGoto: TButton;
    BPgReload: TButton;
    CkKeepOCRed: TCheckBox;
    TLinPattern: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    CkAdjustPage: TCheckBox;
    procedure FormResize(Sender: TObject);
    procedure ImDispPaint(Sender: TObject);
    procedure ImDispMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImDispMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ImDispMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure BStartClick(Sender: TObject);
    procedure ImDispDblClick(Sender: TObject);
    procedure Timer300msTimer(Sender: TObject);
    procedure BDoOCRClick(Sender: TObject);
    procedure ImDispClick(Sender: TObject);
    procedure TOcrTextKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TOcrTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TOcrTextMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BLoadClick(Sender: TObject);
    procedure BPgUpClick(Sender: TObject);
    procedure BPgDnClick(Sender: TObject);
    procedure BDoOCRallClick(Sender: TObject);
    procedure BStepClick(Sender: TObject);
    procedure BPgGotoClick(Sender: TObject);
    procedure BPgReloadClick(Sender: TObject);
    procedure TLinPatternChange(Sender: TObject);
  private
    { Private declarations }
    MouseDown: boolean;                    // mouse pos and image pos at MouseDown event
    MouseDownX, MouseDownY, MouseDownPosX, MouseDownPosY: integer;
    ImgDispMouseX,ImgDispMouseY: integer;  // current mouse pos over ImDest
    ZoomX, ZoomY: double;                  // zoom to conver ImgDispMouseX/Y to coordinates on scanned bitmap image
    CurrentSelectedSymb: integer; // symbol currently selected (box color flashing)
    bOCRStop: boolean; // set by Stop button to make progress return false
    bStepFlag: boolean; // set by step button
    cStepChar: char; 
    bOcrTextInsertMode: boolean; // insert mode on Ocr Text flag
    ImgFileList: TStringList;
  public
    { Public declarations }
    PosX, PosY: integer; // offset from original bitmap top of page to be shown
    Zoom: integer;       // 1000 = 1x, 500 = 0.5x
    SelectedSymb: integer; // symbol to select (box color flashing)
    SelectedPage: integer; // page number beign displayed
    bBwContrastSet: boolean; // true = set image contrast with GUI value
    SelectedBwContrast: integer;      // image contrast
    procedure Log(s: string);
    function DoStep(n1, n2: integer): char; // whait for user to press step button, flash symbols n1 and n2. Return key pressed on keyboard while step button has focus
    function Progress(s: string; bSyncWithTimer: boolean): boolean;
  end;

function InputQueryEx(const ACaption, APrompt: string; var Value: string; he: integer): Boolean;

var
  FoMain: TFoMain;
  ImSrc: TImage;

implementation

{$R *.DFM}

function InputQueryEx(const ACaption, APrompt: string; var Value: string; he: integer): Boolean;

   function GetAveCharSize(Canvas: TCanvas): TPoint;
   var I: Integer;
       Buffer: array[0..51] of Char;
   begin
     for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
     for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
     GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
     Result.X := Result.X div 52;
   end;

var
  Form: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
begin
  Result := False;
  Form := TForm.Create(Application);
  with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      ClientHeight := MulDiv(he, DialogUnits.Y, 8);
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        AutoSize := True;
        WordWrap := True;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Width := MulDiv(164, DialogUnits.X, 4);
        Caption := APrompt;
      end;
      Edit := TEdit.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := MulDiv(he-44, DialogUnits.Y, 8);
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := 255;
        Text := Value;
        SelectAll;
      end;
      ButtonTop := MulDiv(he-22, DialogUnits.Y, 8);
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := 'Assign';
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := 'Cancel';
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      if ShowModal = mrOk then
      begin
        Value := Edit.Text;
        Result := True;
      end;
    finally
      Form.Free;
    end;
end;

procedure TFoMain.FormCreate(Sender: TObject);
begin
   Zoom := 1000;
   CurrentSelectedSymb := -1;
   SelectedPage := -1;
   bOcrTextInsertMode := false;
   ImDisp.ControlStyle := ImDisp.ControlStyle + [csOpaque];
   LProgress.Caption := '';
   LMsg.Caption := '';
   LPage.Caption := '';
   ImgFileList := TStringList.Create;
   bBwContrastSet := false;
   SelectedBwContrast := 128;
end;

procedure TFoMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   bOCRStop := true;
end;

procedure TFoMain.FormResize(Sender: TObject);
begin
   ImDisp.Width := ImDisp.Height * 3 div 4;
   PageControl.Left := ImDisp.Left + ImDisp.Width + 10;
   PageControl.Width := FoMain.Width - PageControl.Left - 30;
   LMsg.Left := PageControl.Left;
end;

procedure TFoMain.ImDispPaint(Sender: TObject);
var ImHandle: HDC; // displayed image on GUI = destimation handle
    w,h: integer;
    r: TRect;
begin
   with Sender as TPaintBox do begin
      ImHandle := Canvas.Handle;
   end;

   w := round(3000.0 * Zoom / 1000.0);
   h := round(4000.0 * Zoom / 1000.0);
   StretchBlt(
     ImHandle, 0, 0, ImDisp.Width, ImDisp.Height, // destination
     ImSrc.Picture.Bitmap.Canvas.Handle, PosX, PosY, w,h,
     SRCCOPY);
   ImDisp.Canvas.Brush.Style := bsSolid;
   ImDisp.Canvas.Brush.Color := clDkGray;
   ZoomX := w / ImDisp.Width;
   ZoomY := h / ImDisp.Height;
   if PosX < 0 then begin
      r.Left := 0; r.Top := 0;
      r.Right := Round(-PosX / ZoomX); r.Bottom := ImDisp.Height;
      ImDisp.Canvas.FillRect(r);
   end;
   if PosY < 0 then begin
      r.Left := 0; r.Top := 0;
      r.Right := ImDisp.Width; r.Bottom := Round(-PosY / ZoomY);
      ImDisp.Canvas.FillRect(r);
   end;
   if PosX + w > ImSrc.Picture.Bitmap.Width then begin
      r.Left := ImDisp.Width - Round(((PosX + w) - ImSrc.Picture.Bitmap.Width) / ZoomX); r.Top := 0;
      r.Right := ImDisp.Width; r.Bottom := ImDisp.Height;
      ImDisp.Canvas.FillRect(r);
   end;
   if PosY + h > ImSrc.Picture.Bitmap.Height then begin
      r.Left := 0; r.Top := ImDisp.Height - Round(((PosY + h) - ImSrc.Picture.Bitmap.Height) / ZoomY);
      r.Right := ImDisp.Width; r.Bottom := ImDisp.Height;
      ImDisp.Canvas.FillRect(r);
   end;
end;

procedure TFoMain.ImDispMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    if (MouseDownX = X) and (MouseDownY = Y) then exit;
    MouseDown := true;
    MouseDownX := X;
    MouseDownY := Y;
    MouseDownPosX := PosX;
    MouseDownPosY := PosY;
end;

procedure TFoMain.ImDispMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MouseDown := false;
end;


procedure TFoMain.ImDispMouseMove(Sender: TObject; Shift: TShiftState; X,  Y: Integer);
var dx, dy: integer;
begin
   ImgDispMouseX := X;
   ImgDispMouseY := Y;
   if MouseDown then begin
      dx := Trunc((MouseDownX-X) * ZoomX);
      dy := Trunc((MouseDownY-Y) * ZoomY);
      PosX := MouseDownPosX + dx ;
      PosY := MouseDownPosY + dy;
      ImDisp.Repaint;
   end;
end;

procedure TFoMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
   Handled := true; // do disable mouse whee for the rest of form, specifically for TMemo insude tabs
   if Wheeldelta < 0 then begin
      Zoom := round(Zoom * 1.1);
   end else if Wheeldelta > 0 then begin
      Zoom := round(Zoom * 0.9);
   end;
   if Zoom < 10 then Zoom := 10;
   if Zoom > 4000 then Zoom := 4000;
   ImDisp.Repaint;
end;

procedure TFoMain.Log(s: string);
begin
   TLog.Lines.Add(s);
end;

// return false if stop pressed
var nTag_Progress: integer;
function TFoMain.Progress(s: string; bSyncWithTimer: boolean): boolean;
begin
   result := true;
   Application.ProcessMessages;
   if bSyncWithTimer then begin
      if nTag_Progress = FoMain.Timer300ms.Tag then exit;
      nTag_Progress := FoMain.Timer300ms.Tag;
   end;
   LProgress.Caption := s;
   if bOCRStop then result := false;
end;

procedure TFoMain.BStepClick(Sender: TObject);
begin
   bStepFlag := true;
end;

// enable and wahot for step button to be pressed
// Return key pressed on keyboard while step button has focus
function TFoMain.DoStep(n1, n2: integer): char;
var nTag: integer;
begin
   bStepFlag := false;
   FoMain.BStep.Visible := true;
   nTag := 0;
   cStepChar := ' ';
   repeat
       Application.ProcessMessages;
       if bOCRStop then break;
       if bStepFlag then break;
       if cStepChar <> ' ' then break;
       // flash n1 and n2
       if nTag = FoMain.Timer300ms.Tag then continue;
       nTag := FoMain.Timer300ms.Tag;
       if nTag = 0 then begin // flip-flop on timer's tag property
          if n1 >= 0 then SetSymbBoxColor(n1,250); // set box green
          if n2 >= 0 then ClearSymbColor(n2);
       end else begin
          if n1 >= 0 then ClearSymbColor(n1);
          if n2 >= 0 then SetSymbBoxColor(n2,250);
       end;
       ShowSymb;
   until false;
   if n1 >= 0 then ClearSymbColor(n1);
   if n2 >= 0 then ClearSymbColor(n2);
   FoMain.BStep.Visible := false;
   result := UpCase(cStepChar);
end;

procedure SelectSymbol;
var x,y,n: integer;
begin
   FoMain.MouseDown := false;
   x := Trunc(FoMain.ImgDispMouseX * FoMain.ZoomX) + FoMain.PosX;
   y := Trunc(FoMain.ImgDispMouseY * FoMain.ZoomY) + FoMain.PosY;
   n := SelSymbAtXY(x,y);
   if n >= 0 then begin
      FoMain.SelectedSymb := n;
      FoMain.Timer300msTimer(FoMain);
   end;
end;

procedure ShowSelectedSymbol;
var n,x1,y1,x2,y2,d: integer;
begin
   n := FoMain.SelectedSymb;
   if n < 0 then exit;
   x1 := Trunc((GetSymbVal(n, 'x1') - FoMain.PosX) / FoMain.ZoomX);
   y1 := Trunc((GetSymbVal(n, 'y1') - FoMain.PosY) / FoMain.ZoomY);
   x2 := Trunc((GetSymbVal(n, 'x2') - FoMain.PosX) / FoMain.ZoomX);
   y2 := Trunc((GetSymbVal(n, 'y2') - FoMain.PosY) / FoMain.ZoomY);
   d := min(x2-x1, y2-y1) div 8;
   x1 := x1 - d;
   x2 := x2 + d;
   y1 := y1 - d;
   y2 := y2 + d;
   if x2 > FoMain.ImDisp.Width  then FoMain.PosX := FoMain.PosX - Round((FoMain.ImDisp.Width - x2) * FoMain.ZoomX);
   if x1 < 0                    then FoMain.PosX := FoMain.PosX + Round(x1 * FoMain.ZoomX);
   if y2 > FoMain.ImDisp.Height then FoMain.PosY := FoMain.PosY - Round((FoMain.ImDisp.Height - y2) * FoMain.ZoomY);
   if y2 < 0                    then FoMain.PosY := FoMain.PosY + Round(y1 * FoMain.ZoomY);
end;

procedure ShowOcrTextCursorPos(cx, cy: integer); forward;

procedure SetOcrTextCarret(cx, cy: integer);
var nCol: integer;
begin
   if (cx >= 0) and (cy >= 0) and (FoMain.TOcrText.Lines.Count > 0) then begin
      nCol := length(FoMain.TOcrText.Lines[0]);
      FoMain.TOcrText.SelStart := (nCol + 2) * cy + cx + 1;
      FoMain.TOcrText.SelLength := -1;
      FoMain.TOcrText.Perform(EM_SCROLLCARET, 0, 0);
   end;
end;

procedure TFoMain.ImDispClick(Sender: TObject);
var cx, cy: integer;
begin
   SelectSymbol;
   if (SelectedSymb >= 0) then begin
      if FoMain.PageControl.ActivePage = TabText then begin
         FoMain.TOcrText.SetFocus;
      end;
      DisplaySymbMatch(SelectedSymb);
      // select symbol ay cx, cy in Ocr Tab text
      // select symbol ay cx, cy in Ocr Tab text
      cx := GetSymbVal(SelectedSymb, 'cx');
      cy := GetSymbVal(SelectedSymb, 'cy');
      SetOcrTextCarret(cx,cy);
      ShowOcrTextCursorPos(cx,cy); // symbol already selected in ImDisp. No need to do it again
   end;
end;

procedure TFoMain.ImDispDblClick(Sender: TObject);
begin
   SelectSymbol;
   if (SelectedSymb >= 0) then begin
       AssignSymb(SelectedSymb);
   end;
end;


procedure TFoMain.Timer300msTimer(Sender: TObject);
var n,c : integer;
begin
    n := FoMain.CurrentSelectedSymb;
    if n <> FoMain.SelectedSymb then begin
       // symbol selected has changed.
       if n >= 0 then begin
          // restore currently selected color: set to white, and update it
          ClearSymbColor(n);
          ShowSymb;
       end;
       n := FoMain.SelectedSymb;
       FoMain.CurrentSelectedSymb := n;
       FoMain.Timer300ms.Tag := 0;
    end;
    if FoMain.Timer300ms.Tag = 0 then begin // flip-flop on timer's tag property
       FoMain.Timer300ms.Tag := 1;
       c := SetSymbBoxColor(n,253); // set box purple
       if c = 249 then begin        // but is (was) red?
          SetSymbBoxColor(n, 254);  // then set box cyan instead
       end;
    end else begin
       FoMain.Timer300ms.Tag := 0;
       ClearSymbColor(n);           // restore box to the required color
       ShowSymb;
    end;
    ImDisp.Repaint;
end;

procedure ShowOcrTextCursorPos(cx, cy: integer);
begin
   FoMain.LMsg.Caption := 'Col: ' + IntToStr(cx+1) + ', ' + 'Row: ' + IntToStr(cy+1) + ' ';
   if FoMain.bOcrTextInsertMode then FoMain.LMsg.Caption := FoMain.LMsg.Caption + '(INSERT)';
end;

procedure ShowSymbAtTextOcrCursor;
var cx,cy: integer;
begin
   FoMain.TOcrText.SelLength := 0;
   cx := FoMain.TOcrText.CaretPos.x;
   cy := FoMain.TOcrText.CaretPos.y;
   ShowOcrTextCursorPos(cx,cy); // select symb in OcrText
   // select symbol at cx,xy in ImDisp
   FoMain.TOcrText.SelStart := FoMain.TOcrText.SelStart+1;
   FoMain.TOcrText.SelLength := -1;
   FoMain.SelectedSymb := SelSymbAtCXY(cx,cy);
   ShowSelectedSymbol;
   FoMain.ImDisp.Repaint;
end;

procedure TFoMain.TOcrTextMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   ShowSymbAtTextOcrCursor;
end;

procedure TFoMain.TOcrTextKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
   if Key = vk_insert then begin  // insert key togles the insert mode
      bOcrTextInsertMode := not bOcrTextInsertMode;
      ShowOcrTextCursorPos(FoMain.TOcrText.CaretPos.x, FoMain.TOcrText.CaretPos.y);
      if bOcrTextInsertMode then FoMain.LMsg.Caption := 'INSERT Mode: type ? to select next ? char, type key to assing this value to symbol';
   end;
end;

procedure TFoMain.TOcrTextKeyPress(Sender: TObject; var Key: Char);
var cx, cy, n: integer;
    nCol, nRow, nCarret: integer;
    s, Ascii: string;
begin
   if Key = #27 then begin      // esc -> select symbol at cursor
      ShowSymbAtTextOcrCursor;
      exit;
   end;
   if not bOcrTextInsertMode then begin
      cStepChar := Key;
      exit;  // no insert mode
   end;
   FoMain.TOcrText.SelLength := 0;
   cx := FoMain.TOcrText.CaretPos.x;
   cy := FoMain.TOcrText.CaretPos.y;
   nRow := FoMain.TOcrText.Lines.Count;
   if Key = '?' then begin       // tab -> select next ?
      nCarret := 0;
      while (nRow > 0) and (cy < nRow) do begin
         s := FoMain.TOcrText.Lines[cy];
         nCol := length(s);
         if cx >= nCol then begin
            cx := 0; inc(cy); nCarret := 1;
            continue;
         end;
         if (nCarret = 0) or (s[cx+1] <> '?') then begin
            inc(cx); nCarret := 1;
            continue;
         end;
         SetOcrTextCarret(cx,cy);
         ShowSymbAtTextOcrCursor;
         exit;
      end;
      SetOcrTextCarret(0,0);
      FoMain.LMsg.Caption := 'No more ? chars';
      exit;
   end;
   n := SelSymbAtCXY(cx,cy);
   if Key = #8 then begin
      // BackSpace -> mark symb as NotIdent
      if n >= 0 then AssignSymb2(n, '');
      Ascii := '?';
   end else if (Key >= ' ') and (Key < #128) then begin
     // assing key pressed to symbol at cursor
      if n >= 0 then AssignSymb2(n, Key);
      Ascii := Key;
   end else begin
      exit;
   end;
   // replace char at carret
   ShowSymbAtTextOcrCursor;
   if (nRow > 0) and (cy < nRow) then begin
      s := FoMain.TOcrText.Lines[cy];
      nCol := length(s);
      if cx < nCol then begin
         s[cx+1] := Ascii[1];
         nCarret := FoMain.TOcrText.SelStart;
         FoMain.TOcrText.Lines.BeginUpdate;
         FoMain.TOcrText.Lines[cy] := s;
         FoMain.TOcrText.Lines.EndUpdate;
         FoMain.TOcrText.SelStart := nCarret + 1;
         FoMain.TOcrText.SelLength := -1;
      end;
   end;
end;

procedure LoadFileList(fn: string);
var sr: TSearchRec;
    path, f1, f2: string;

    procedure Add(s: string);
    begin
       if UpStr(s) = UpStr('MatchList.bmp') then exit;
       FoMain.ImgFileList.Add(s);
    end;

var i: integer;
begin
   // get all .bmp images of fn's dir
   FoMain.ImgFileList.Clear;
   path := ExtractFilePath(fn) + '*.bmp';
   if FindFirst(path, faArchive, sr) = 0 then begin
      Add(sr.Name);
      while FindNext(sr) = 0 do begin
         Add(sr.Name);
      end;
      FindClose(sr);
   end;
   // sort and keep only the ones after fn
   FoMain.ImgFileList.Sort;
   f1 := UpStr(trim(ExtractFileName(fn)));
   while FoMain.ImgFileList.Count > 0 do begin
      f2 := UpStr(FoMain.ImgFileList.Strings[0]);
      if f1 = f2 then break;
      FoMain.ImgFileList.Delete(0);
   end;
   // add full path to all filenames
   for i := 0 to FoMain.ImgFileList.Count-1 do begin
      FoMain.ImgFileList.Strings[i] := ExtractFilePath(fn) + FoMain.ImgFileList.Strings[i];
   end;
end;

// AbsFlag = true -> load page n (first page is page 0).
// AbsFlag = false -> n is relative to current page (+1 load next page, -1 load previous page)
procedure LoadSelectedPage(n: integer; AbsFlag: boolean);
var fn: string;
begin
   if FoMain.SelectedPage < 0 then exit;
   if FoMain.ImgFileList.Count < 1 then exit;
   if AbsFlag = false then n := FoMain.SelectedPage + n;
   if n < 0 then n := 0;
   if n >= FoMain.ImgFileList.Count then n := FoMain.ImgFileList.Count-1;
   FoMain.SelectedPage := n;
   fn := FoMain.ImgFileList.Strings[FoMain.SelectedPage];
   LoadPage(fn);
   FoMain.LPage.Caption := 'Page ' + IntToStr(FoMain.SelectedPage+1) + ' of ' + IntToStr(FoMain.ImgFileList.Count);
end;


procedure TFoMain.TLinPatternChange(Sender: TObject);
begin
   GetSymbVal(0, 'MatchChanged'); // notofy that MatchList muts be saved with new Start of line pattern data 
end;

function CheckOCRinProgress: boolean;
begin
   result := false;
   if FoMain.BDoOCR.Tag = 0 then exit;
   MessageDlg('Not available while OCR in progress.', mtInformation, [mbOk], 0);
   result := true;
end;

function CheckNoPagesLoaded: boolean;
begin
   result := false;
   if FoMain.ImgFileList.Count > 0 then exit;
   MessageDlg('No pages loaded', mtError, [mbOk], 0);
   result := true;
end;

procedure TFoMain.BLoadClick(Sender: TObject);
var fn: string;
begin
   if CheckOCRinProgress then exit;
   if not OpenInSrcDialog.Execute then exit;
   fn := OpenInSrcDialog.FileName;
   Log('INFO: Load Pages from path: ' + ExtractFilePath(fn));
   LoadFileList(fn);
   SelectedPage := 0;
   LoadPage('');  // to force param load (including MatchList)
   LoadSelectedPage(0, True);
end;

procedure TFoMain.BPgUpClick(Sender: TObject);
begin
   if CheckOCRinProgress then exit;
   if CheckNoPagesLoaded then exit;
   SavePage;
   LoadSelectedPage(-1, False);
end;

procedure TFoMain.BPgDnClick(Sender: TObject);
begin
   if CheckOCRinProgress then exit;
   if CheckNoPagesLoaded then exit;
   SavePage;
   LoadSelectedPage(+1, False);
   if FoMain.PageControl.ActivePage = TabText then begin
      FoMain.TOcrText.SetFocus;
   end;
end;

procedure TFoMain.BPgGotoClick(Sender: TObject);
var s: string;
    n, m: integer;
begin
   if CheckOCRinProgress then exit;
   if CheckNoPagesLoaded then exit;
   if not InputQuery('Goto Page', 'Goto page number:', s) then exit;
   s := Trim(s);
   if s = '' then exit;
   m := FoMain.ImgFileList.Count;
   try
      n := StrToInt(s);
   except
      on EConvertError do begin
         MessageDlg('Invalid page number (must be a number in range 1 to '+IntToStr(m)+')', mtError, [mbOk], 0);
         exit;
      end;
   end;
   if (n < 1) or (n > m) then begin
      MessageDlg('Page number out of range (valid range 1 to '+IntToStr(m)+')', mtError, [mbOk], 0);
      exit;
   end;
   SavePage;
   LoadSelectedPage(n-1, True);
end;

procedure TFoMain.BPgReloadClick(Sender: TObject);
var s: string;
    n: integer;
begin
   if CheckOCRinProgress then exit;
   if CheckNoPagesLoaded then exit;
   s := IntToStr(FoMain.SelectedBwContrast);
   if not InputQuery('Contrast', 'Set Contrast (1-255, high value means darker)', s) then exit;
   s := Trim(s);
   if s = '' then exit;
   try
      n := StrToInt(s);
   except
      on EConvertError do begin
         MessageDlg('Invalid value (must be a number in range 1 to 255)', mtError, [mbOk], 0);
         exit;
      end;
   end;
   if (n < 1) or (n > 255) then begin
      MessageDlg('Value out of range (must be 1 to 255)', mtError, [mbOk], 0);
      exit;
   end;
   FoMain.SelectedBwContrast := n;
   FoMain.bBwContrastSet := true;
   LoadSelectedPage(0, False);
   FoMain.bBwContrastSet := false;
end;

procedure TFoMain.BStartClick(Sender: TObject);
begin
   if CheckOCRinProgress then exit;
   if CheckNoPagesLoaded then exit;
   bOCRStop := false;
   StartOCR(FoMain.CkAdjustPage.Checked);
end;

procedure TFoMain.BDoOCRClick(Sender: TObject);
begin
   if FoMain.BDoOCR.Tag = 0 then begin
      if CheckNoPagesLoaded then exit;
      if GetSymbVal(0, 'Exist') = 0 then begin
         MessageDlg('No symbols found. Must press Init OCR', mtError, [mbOk], 0);
         exit;
      end;
      FoMain.BDoOCR.Tag := 1;
      FoMain.BDoOCR.Caption := 'Stop';
      bOCRStop := false;
      DoOCR;
      FoMain.BDoOCR.Tag := 0;
      FoMain.BDoOCR.Caption := 'Do OCR';
   end else begin
      bOCRStop := true;
   end;
end;

procedure TFoMain.BDoOCRallClick(Sender: TObject);
var StartTime: TDateTime;
    n: integer;
begin
   if CheckOCRinProgress then exit;
   if CheckNoPagesLoaded then exit;
   FoMain.BDoOCR.Tag := 1;
   FoMain.BDoOCR.Caption := 'Stop';
   FoMain.TOcrText.Font.Size := 6;
   bOCRStop := false;
   StartTime := Now;
   repeat
      if (GetSymbVal(0, 'Exist') = 0) or (FoMain.CkKeepOCRed.Checked = false) then begin
         StartOCR(FoMain.CkAdjustPage.Checked);
      end;
      DoOCR;
      if bOCRStop then break;
      SavePage;
      if FoMain.SelectedPage = FoMain.ImgFileList.Count-1 then break; // last page processed
      LoadSelectedPage(+1, False);
   until false;
   FoMain.TOcrText.Font.Size := 14;
   FoMain.BDoOCR.Tag := 0;
   FoMain.BDoOCR.Caption := 'Do OCR';
   n := round(24*3600*frac(Now-StartTime)); // seconds elapsed from begin of process
   Log('INFO: OCR All Elapsed: ' + IntToStr(n) + ' sec. ');
end;







end.
