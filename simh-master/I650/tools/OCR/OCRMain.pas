unit OCRMain;

{$RANGECHECKS ON}

interface

function max(a,b: integer): integer;
function min(a,b: integer): integer;
function UpStr(s: string): string;

procedure StartOCR(bAdjustPage: boolean);
procedure DoOCR;
procedure SavePage;
procedure LoadPage(fn: string);
function GetSymbVal(n: integer; s: string): integer;
function SelSymbAtXY(x,y: integer): integer;
function SelSymbAtCXY(cx,cy: integer): integer;
function SetSymbBoxColor(n, c0: integer): integer;
procedure ClearSymbColor(n: integer);
procedure ShowSymb;
procedure AssignSymb(n: integer);
procedure AssignSymb2(n: integer; Ascii: string);
procedure DisplaySymbMatch(n: integer);

implementation

uses Windows, Messages, SysUtils, Graphics, Dialogs, Controls, Classes,
     FMain;

const MaxTextCols = 160;
const MaxTextLins = 120;
const MaxSymbList = MaxTextCols * MaxTextLins;
const MaxMatchList = 10;
// parameters
var Param: record
              Loaded: boolean;                  // true = params loaded form MatchList.ini file
              BwContrast: integer;              // if color > 128 pixel is black ele white when converting loaded image to B&W
              MinSymbolWi,MinSymbolHe: integer; // minimum symbol size (Width, heigt) in pixes
              MaxSymbolWi,MaxSymbolHe: integer; // max symbol size (Width, heigt) in pixes
              MaxCols, MaxLins: integer;        // max number of cols and lines per page
              MaxDifInSymbBox: integer;         // max difference in symbol box width or size between most frequet symbol and secodn most frequent symbol
              MatchSymbList: record
                   MatchSymbListChanged: boolean;        // flag to tell if user has added new symbol and must be saved to disk
                   nSymb: integer;                       // number of symbols in list
                   Box: array [0..MaxSymbList] of record // box arround symbol
                       x1,x2,y1,y2: integer;             // box boundaries of symbol box in ImSymb bitmap
                       wi,he: integer;                   // width/height (wi=x2-x1+1) just for easing processing
                       Quality: integer;                 // Maq Q value to be considered valid match (if <0, match disabled)
                       Ascii: string;
                       rot: double;                      // symbol rotation
                   end;
                   MaxImSymbWi, MaxImSymbHe: integer;    // max Symbols bitmap size
                   CurrentImSymbWi: integer;             // corrent used width
                   ImSymb: TBitmap;                      // Symbols bitmap
              end;
              QualityValues: array[0..9] of integer;     // quality value for 0..9 adyacent pixels
              UnalignForMinQuality: integer;             // pixels a symbol is unaligned to calc quality for Ok match
              BoxOversizeForCombineFragments: integer;   // % (0..100) of symbol box aumented to check for symbols fragmentes making a regular symbol
              BestMatchFactor: integer;                  // % (0..100) best match if qualily match[0] is < Factor*match[1]
              StartOfLinePatternMinCount: integer;       // number of times any start of line pattern must match in whole page to identify it as start of line
              FirstLineMinCharCount: integer;            // number of consecutive chars to identify the first line of page
              PageTopMargin: integer;                    // number of lines bellow/above text page
           end;
// symbol boxels list
type  TSymbOCRStates = (syNotIdent,                     // symbol not OCR'd yet
                        syIgnore,                       // ignore this symbol (OCR'd as space)
                        syUserIdent,                    // user has assisted to OCR this symbol.
                        syOkIdent,                      // OCR'd ok
                        syNOk_MultipleMatchs,           // OCR'd, but multiple possible matches to choose from
                        syNOk_LowQualityMatch,          // OCR'd, but poor quality math/matches
                        syNOk_NoMatch,                  // OCR'd, but no match found
                        syNOk_NoFit                     // OCR'd and match a multi char symbol that does not fit (overlaps symbols at right)
                        );
var SymbList: record
                   FileName: string;                     // filename for .bmp page being OCR'ed (symbols belong to this page)
                   BwContrast: integer;                  // if color > contrast pixel is black else white when converting loaded image to B&W
                   nSymb: integer;                       // number of symbols in list
                   Box: array [0..MaxSymbList] of record // box arround symbol
                       x1,x2,y1,y2: integer;             // box boundaries of symbol box
                       wi,he: integer;                   // width/height (wi=x2-x1+1) just for easing processing
                       OCRState: TSymbOCRStates;         // state of ocr for this symbol
                       nMatchs: integer;                 // number of matches in MatchList
                       MatchList: array[0..MaxMatchList] of record
                          MatchSymb: integer;            // symbol matched from MatchSymbList
                          Ascii: string;                 // ascii test this match represent
                          Quality: integer;              // quality of match (0=perfect, >0 worse)
                          MatchOk: integer;              // 1 -> good quality match, 2 -> best match (match[0].q is < 50% match[1].q)
                       end;
                       Ascii: string;                    // ascii test this symbol represent
                       cx, cy: integer;                  // col, row where symbol is positioned (start at col/row 0,0)
                       Color: integer;                   // color for the symbol box
                   end;
                   wi, he: integer;                      // most frequent symbol box size
                   nCol, nRow: integer;                  // page number of cols and rows
                   rot: double;                          // page rotation
               end;


procedure Log(s: string);
begin
   FoMain.Log(s);
end;

procedure ClearImSrc;
var x,y,he,wi,n: integer;
    r,g,b,bw: integer;
    P: PByteArray;
begin
   he := FoMain.ImSrc.Picture.Bitmap.Height;
   wi := FoMain.ImSrc.Picture.Bitmap.Width;
   if FoMain.ImSrc.Picture.Bitmap.PixelFormat = pf24bit then begin
      // convert to b&w
      for y := 0 to he-1 do begin
         P := FoMain.ImSrc.Picture.Bitmap.ScanLine[y];
         for x := 0 to wi-1 do begin
            n := x*3;
            r := P[n];  g := P[n+1];  b := P[n+2];
            bw := (r * 2 + g * 7 + b) div 10;
            if bw > SymbList.BwContrast then bw := 255 else bw := 0;
            P[n] := bw; P[n+1] := bw; P[n+2] := bw;
         end;
      end;
      FoMain.ImSrc.Picture.Bitmap.PixelFormat := pf8bit; // converto to 8 bits per pixel
   end;
   FoMain.SelectedSymb := -1;
   for y := 0 to he-1 do begin
      P := FoMain.ImSrc.Picture.Bitmap.ScanLine[y];
      for x := 0 to wi-1 do begin
          if P[x] < 10 then begin
             P[x] := 0;
          end else begin
             P[x] := 255;
          end;
      end;
   end;
   for n := 0 to SymbList.nSymb-1 do begin
       SymbList.Box[n].Color := 255; // white symbol box
   end;
end;

procedure SetDefaultParams;
var Rect: TRect;
    i: integer;
begin
   Param.Loaded := False;
   Param.BwContrast  := 128;
   Param.MinSymbolWi := 3;
   Param.MinSymbolHe := 3;
   Param.MaxSymbolWi := 100;
   Param.MaxSymbolHe := 100;
   Param.MaxCols := 160;
   Param.MaxLins := 120;
   if Param.MaxCols > MaxTextCols then Param.MaxCols := MaxTextCols;
   if Param.MaxLins > MaxTextCols then Param.MaxLins := MaxTextLins;
   Param.MaxDifInSymbBox := 3;
   Param.MatchSymbList.MatchSymbListChanged := true;
   Param.MatchSymbList.nSymb := 0;
   Param.MatchSymbList.MaxImSymbWi := 10000;
   Param.MatchSymbList.MaxImSymbHe := 200;
   Param.MatchSymbList.CurrentImSymbWi := 0;
   if (Param.MatchSymbList.ImSymb <> Nil) then Param.MatchSymbList.ImSymb.Free;
   Param.MatchSymbList.ImSymb := TBitmap.Create;
   Param.MatchSymbList.ImSymb.PixelFormat := pf8bit;
   Param.MatchSymbList.ImSymb.Width := Param.MatchSymbList.MaxImSymbWi;
   Param.MatchSymbList.ImSymb.Height := Param.MatchSymbList.MaxImSymbHe;
   Param.MatchSymbList.ImSymb.Canvas.Brush.Color := clWhite;
   Rect.Left := 0; Rect.Top := 0;
   Rect.Right :=  Param.MatchSymbList.ImSymb.Width;
   Rect.Bottom := Param.MatchSymbList.ImSymb.Height;
   Param.MatchSymbList.ImSymb.Canvas.FillRect(Rect);
   for i := 0 to 9 do Param.QualityValues[i] := i;
   Param.QualityValues[6] := 10;
   Param.QualityValues[7] := 13;
   Param.QualityValues[8] := 18;
   Param.QualityValues[9] := 20;
   Param.UnalignForMinQuality := 1;
   Param.BoxOversizeForCombineFragments := 20;
   Param.BestMatchFactor := 50;
   Param.StartOfLinePatternMinCount := 5;
   Param.FirstLineMinCharCount := 3;
   Param.PageTopMargin := 1;
end;


procedure SavePage;
var ini: TStringList;

   procedure Add1(s: string; n1: integer);
   begin
      ini.Add(s + '=' + IntToStr(n1));
   end;
   procedure Add2(s: string; n1,n2: integer);
   begin
      ini.Add(s + '=' + IntToStr(n1) + ',' + IntToStr(n2));
   end;

var n, m: integer;
    fn: string;
begin
   if SymbList.FileName = '' then exit; 
   // save page symbols
   ini := TStringList.Create;
   ini.Add('; Symbols in page ');
   ini.Add('');
   Add1('nSymb', SymbList.nSymb);
   Add2('wi,he', SymbList.wi, SymbList.he);
   Add2('nCol,nRow', SymbList.nCol, SymbList.nRow);
   Add1('rot', round(SymbList.rot * 1000));
   for n := 0 to SymbList.nSymb-1 do begin
      Add1('Box', n);
      Add2('x1,y1', SymbList.Box[n].x1, SymbList.Box[n].y1);
      Add2('x2,y2', SymbList.Box[n].x2, SymbList.Box[n].y2);
      Add2('wi,he', SymbList.Box[n].wi, SymbList.Box[n].he);
      Add1('OCRState', ord(SymbList.Box[n].OCRState));
      ini.Add('Ascii=' + SymbList.Box[n].Ascii);
      Add2('cx,cy', SymbList.Box[n].cx, SymbList.Box[n].cy);
      Add1('Color', SymbList.Box[n].Color);
      Add1('nMatchs', SymbList.Box[n].nMatchs);
      for m := 0 to SymbList.Box[n].nMatchs-1 do begin
         Add1('MatchList', m);
         Add1('MatchSymb', SymbList.Box[n].MatchList[m].MatchSymb);
         ini.Add('Ascii=' + SymbList.Box[n].MatchList[m].Ascii);
         Add2('Quality,MatchOk', SymbList.Box[n].MatchList[m].Quality, SymbList.Box[n].MatchList[m].MatchOk);
      end;
   end;
   Add1('BwContrast', SymbList.BwContrast);
   fn := ChangeFileExt(SymbList.FileName, '.ini');
   ini.SaveToFile(fn);
   Log('INFO: Save Page State: ' + fn);
   // save page OCR'ed chars
   fn := ChangeFileExt(SymbList.FileName, '.txt');
   if FoMain.TOcrText.Lines.Count > 0 then begin
      FoMain.TOcrText.Lines.SaveToFile(fn);
      Log('INFO: Save Page OCRed Text: ' + fn);
   end;
   // save symbols from match list (the ones defined by user)
   if Param.MatchSymbList.MatchSymbListChanged then begin
      Param.MatchSymbList.MatchSymbListChanged := false;
      fn := ExtractFilePath(SymbList.FileName) + 'MatchList.bmp';
      Param.MatchSymbList.ImSymb.SaveToFile(fn);
      Log('INFO: Save MatchList Image: ' + fn);
      ini.Clear;
      ini.Add('; MatchList defined by user');
      ini.Add('');
      Add1('nSymb', Param.MatchSymbList.nSymb);
      Add2('wi,he', Param.MatchSymbList.MaxImSymbWi, Param.MatchSymbList.MaxImSymbHe);
      Add1('CurrentWi', Param.MatchSymbList.CurrentImSymbWi);
      for m := 0 to Param.MatchSymbList.nSymb-1 do begin
         Add1('Box', m);
         Add2('x1,y1', Param.MatchSymbList.Box[m].x1, Param.MatchSymbList.Box[m].y1);
         Add2('x2,y2', Param.MatchSymbList.Box[m].x2, Param.MatchSymbList.Box[m].y2);
         Add2('wi,he', Param.MatchSymbList.Box[m].wi, Param.MatchSymbList.Box[m].he);
         Add1('Quality', Param.MatchSymbList.Box[m].Quality);
         ini.Add('Ascii=' + Param.MatchSymbList.Box[m].Ascii);
         Add1('rot', round(Param.MatchSymbList.Box[m].rot * 1000));
      end;
      Add1('nStartOfLinePattern', FoMain.TLinPattern.Lines.Count);
      for n := 0 to FoMain.TLinPattern.Lines.Count-1  do begin
         ini.Add('StartOfLinePattern=' + FoMain.TLinPattern.Lines[n]);
      end;
      fn := ExtractFilePath(SymbList.FileName) + 'MatchList.ini';
      ini.SaveToFile(fn);
      Log('INFO: Save MatchList State: ' + fn);
   end;
   ini.Free;
end;

procedure LoadPage(fn: string);
var ini: TStringList;
    nIni: integer;

    procedure GetS(sTag: string; var s: string);
    var s2: string;
    begin
       s := '';
       repeat // skip blank & comments
          if nIni >= ini.Count then exit;
          s := ini.Strings[nIni]; s2 := trim(s);
          inc(nIni);
          if s2 = '' then continue;
          if s2[1] = ';' then continue;
          break;
       until false;
       if sTag + '=' <> Copy(s, 1, length(sTag)+1) then exit; // different tag!
       s := Copy(s, length(sTag)+2,255);
    end;

    procedure Get1(sTag: string; var n: integer);
    var s: string;
    begin
       n := 0;
       GetS(sTag, s);
       if s = '' then exit;
       n := StrToInt(s);
    end;

    procedure Get2(sTag: string; var n1,n2: integer);
    var s: string;
        i: integer;
    begin
       n1 := 0; n2 := 0;
       GetS(sTag, s);
       if s = '' then exit;
       i := Pos(',', s);
       if i = 0 then exit;
       n1 := StrToInt(Copy(s, 1, i-1));
       n2 := StrToInt(Copy(s, i+1, 255));
    end;

   procedure ClearImSymb;
   var Rect: TRect;
   begin
      Param.MatchSymbList.ImSymb.Canvas.Brush.Color := clWhite;
      Rect.Left := 0; Rect.Top := 0;
      Rect.Right :=  Param.MatchSymbList.ImSymb.Width;
      Rect.Bottom := Param.MatchSymbList.ImSymb.Height;
      Param.MatchSymbList.ImSymb.Canvas.FillRect(Rect);
   end;

var i, n, m: integer;
    s: string;
begin
   if fn = '' then begin
      Param.Loaded := false;
      exit; 
   end;
   SymbList.FileName := fn;
   SymbList.BwContrast := -1;
   // load bitmap image
   Log('INFO: Load Page Image: ' + ExtractFileName(SymbList.FileName));
   FoMain.ImSrc.Picture.LoadFromFile(SymbList.FileName);
   FoMain.ImSrc.Picture.Bitmap.PixelFormat := pf24bit;
   // load page symbols
   ini := TStringList.Create;
   fn := ChangeFileExt(SymbList.FileName, '.ini');
   if not FileExists(fn) then begin
      SymbList.nSymb := 0;
      SymbList.wi := 0;   SymbList.he := 0;
      SymbList.nCol := 0; SymbList.nRow := 0;
      SymbList.rot := 0;
   end else begin
      Log('INFO: Load Page State: ' + ExtractFileName(fn));
      ini.LoadFromFile(fn);
      nIni := 0;
      Get1('nSymb', SymbList.nSymb);
      Get2('wi,he', SymbList.wi, SymbList.he);
      Get2('nCol,nRow', SymbList.nCol, SymbList.nRow);
      Get1('rot', i); SymbList.rot := i / 1000;
      for n := 0 to SymbList.nSymb-1 do begin
         Get1('Box', i);
         Get2('x1,y1', SymbList.Box[n].x1, SymbList.Box[n].y1);
         Get2('x2,y2', SymbList.Box[n].x2, SymbList.Box[n].y2);
         Get2('wi,he', SymbList.Box[n].wi, SymbList.Box[n].he);
         Get1('OCRState', i); SymbList.Box[n].OCRState := syNotIdent; inc(SymbList.Box[n].OCRState, i);
         GetS('Ascii', SymbList.Box[n].Ascii);
         Get2('cx,cy', SymbList.Box[n].cx, SymbList.Box[n].cy);
         Get1('Color', SymbList.Box[n].Color);
         Get1('nMatchs', SymbList.Box[n].nMatchs);
         for m := 0 to SymbList.Box[n].nMatchs-1 do begin
            Get1('MatchList', i);
            Get1('MatchSymb', SymbList.Box[n].MatchList[m].MatchSymb);
            GetS('Ascii', SymbList.Box[n].MatchList[m].Ascii);
            Get2('Quality,MatchOk', SymbList.Box[n].MatchList[m].Quality, SymbList.Box[n].MatchList[m].MatchOk);
         end;
      end;
      Get1('BwContrast', SymbList.BwContrast);
   end;
   // load page OCR'ed chars
   FoMain.TOcrText.Lines.Clear;
   fn := ChangeFileExt(SymbList.FileName, '.txt');
   if FileExists(fn) then begin
      FoMain.TOcrText.Lines.LoadFromFile(fn);
      Log('INFO: Load Page OCRed Text: ' + ExtractFileName(fn));
   end;
   // load params if needed
   if not Param.Loaded then begin
      SetDefaultParams;
      Param.Loaded := true;
      fn := ExtractFilePath(SymbList.FileName) + 'MatchList.ini';
      if not FileExists(fn) then begin
         ClearImSymb;
         Param.MatchSymbList.nSymb := 0;
         Param.MatchSymbList.CurrentImSymbWi := 0;
      end else begin
         ini.Clear;
         Log('INFO: Load MatchList State: ' + ExtractFileName(fn));
         ini.LoadFromFile(fn);
         nIni := 0;
         Get1('nSymb', Param.MatchSymbList.nSymb);
         Get2('wi,he', Param.MatchSymbList.MaxImSymbWi, Param.MatchSymbList.MaxImSymbHe);
         if (Param.MatchSymbList.MaxImSymbWi > 0) and (Param.MatchSymbList.MaxImSymbHe > 0) then begin
            Param.MatchSymbList.ImSymb.Width := Param.MatchSymbList.MaxImSymbWi;
            Param.MatchSymbList.ImSymb.Height := Param.MatchSymbList.MaxImSymbHe;
         end;
         Get1('CurrentWi', Param.MatchSymbList.CurrentImSymbWi);
         for m := 0 to Param.MatchSymbList.nSymb-1 do begin
            Get1('Box', i);
            Get2('x1,y1', Param.MatchSymbList.Box[m].x1, Param.MatchSymbList.Box[m].y1);
            Get2('x2,y2', Param.MatchSymbList.Box[m].x2, Param.MatchSymbList.Box[m].y2);
            Get2('wi,he', Param.MatchSymbList.Box[m].wi, Param.MatchSymbList.Box[m].he);
            Get1('Quality', Param.MatchSymbList.Box[m].Quality);
            GetS('Ascii', Param.MatchSymbList.Box[m].Ascii);
            Get1('rot', i); Param.MatchSymbList.Box[m].rot := i / 1000;
         end;
         FoMain.TLinPattern.Lines.Clear;
         Get1('nStartOfLinePattern', n);
         for i := 0 to n-1  do begin
            GetS('StartOfLinePattern', s);
            if Trim(s) = '' then continue;
            FoMain.TLinPattern.Lines.Add(s);
         end;
         fn := ExtractFilePath(SymbList.FileName) + 'MatchList.bmp';
         if not FileExists(fn) then begin
            ClearImSymb;
         end else begin
            Log('INFO: Load MatchList Image: ' + ExtractFileName(fn));
            Param.MatchSymbList.ImSymb.LoadFromFile(fn);
         end;
      end;
      Param.MatchSymbList.MatchSymbListChanged := false;
   end;
   // adjust contrast to gui (if nedded)
   if SymbList.BwContrast <= 0 then begin
      SymbList.BwContrast := Param.BwContrast;
   end;
   if FoMain.bBwContrastSet then begin
      SymbList.BwContrast := FoMain.SelectedBwContrast; // bBwContrastSet = true -> set image contrast with GUI value
   end else begin
      FoMain.SelectedBwContrast := SymbList.BwContrast;
   end;
   ini.Free;
   // rebuild of symbol box
   if SymbList.nSymb > 0 then begin
      ClearImSrc;
      for n := 0 to SymbList.nSymb-1 do begin
          SymbList.Box[n].Color := 255; // to force repaint
      end;
      ShowSymb;
   end;
end;

function max(a,b: integer): integer;
begin
   if a > b then result := a else result := b;
end;

function min(a,b: integer): integer;
begin
   if a < b then result := a else result := b;
end;

function UpStr(s: string): string;
var i: integer;
begin
   for i := 1 to length(s) do s[i] := UpCase(s[i]);
   result := s;
end;

function GetSymbVal(n: integer; s: string): integer;
var r: integer;
begin
   s := UpStr(s);
   if s = 'MATCHCHANGED' then begin
      Param.MatchSymbList.MatchSymbListChanged := true; // set to true the flag to save MatchList data
      result := 0;
      exit;
   end;
   if (n < 0) or (n >= SymbList.nSymb) then begin result := 0; exit; end;
   r := 0;
   if s = 'EXIST' then r := 1; // return 1 if symbol n exists (return 0 if not)
   if s = 'X1' then r := SymbList.Box[n].x1;
   if s = 'X2' then r := SymbList.Box[n].x2;
   if s = 'Y1' then r := SymbList.Box[n].y1;
   if s = 'Y2' then r := SymbList.Box[n].y2;
   if s = 'WI' then r := SymbList.Box[n].wi;
   if s = 'HE' then r := SymbList.Box[n].he;
   if s = 'CX' then r := SymbList.Box[n].cx;
   if s = 'CY' then r := SymbList.Box[n].cy;
   result := r;
end;

// return match quality of two symbols. 0-> perfect match, >0 -> worse
// using BitBlt windows func
function GetQualityMatch(hdc1: HDC; x1, y1, wi1, he1: integer; // bitmap1
                         hdc2: HDC; x2, y2, wi2, he2: integer; // bitmap2
                         x2ofs, y2ofs: integer): integer;      // position of bitmap2 over bitmap1
var bmp: TBitmap;
    Rect: TRect;
    x1ofs, y1ofs: integer;
    wi, he, x, y, n, Quality: integer;
    Pup, P, Pdn: PByteArray;
begin
   x1ofs := 0;
   y1ofs := 0;
   if x2ofs < 0 then begin x1ofs := - x2ofs; x2ofs := 0; end;
   if y2ofs < 0 then begin y1ofs := - y2ofs; y2ofs := 0; end;
   wi := max(wi1 + x1ofs, wi2 + x2ofs);
   he := max(he1 + y1ofs, he2 + y2ofs);
   // xor bitmaps using BitBlt
   bmp := TBitmap.Create;
   bmp.PixelFormat := pf8bit;
   bmp.Width := wi;
   bmp.Height := he;
   bmp.Canvas.Brush.Color := clWhite;
   Rect.Left := 0; Rect.Top := 0; Rect.Right :=  wi; Rect.Bottom := he;
   bmp.Canvas.FillRect(Rect);
   BitBlt(
     bmp.Canvas.Handle, x1ofs,y1ofs,wi1,he1, // destination
     hdc1, x1,y1, SRCCOPY);
   BitBlt(
     bmp.Canvas.Handle, x2ofs,y2ofs,wi2,he2, // destination
     hdc2, x2,y2, SRCINVERT);
   BitBlt(
     bmp.Canvas.Handle, x2ofs,y2ofs,wi2,he2, // destination
     bmp.Canvas.Handle, x2ofs,y2ofs, NOTSRCCOPY	);
   // bmp.SaveToFile('XOR.bmp');
   P := NIL; Pdn := NIL; Quality := 0;
   if he > 0 then Pdn := bmp.ScanLine[0];
   for y := 0 to he-1 do begin
       Pup := P; P := Pdn;
       if y < he-1 then Pdn :=bmp.ScanLine[y+1] else Pdn := NIL;
       for x := 0 to wi-1 do begin
           if (P[x] > 10) then continue;
           // scan array 3x3 of adjacent pixels
           n := 1;
           if (x > 0) and (P[x-1] = 0) then inc(n);
           if (x < wi-1) and (P[x+1] = 0) then inc(n);
           if (Pup <> NIL) then begin
               if (x > 0) and (Pup[x-1] = 0) then inc(n);
               if (Pup[x] = 0) then inc(n);
               if (x < wi-1) and (Pup[x+1] = 0) then inc(n);
           end;
           if (Pdn <> NIL) then begin
               if (x > 0) and (Pdn[x-1] = 0) then inc(n);
               if (Pdn[x] = 0) then inc(n);
               if (x < wi-1) and (Pdn[x+1] = 0) then inc(n);
           end;
           Quality := Quality + Param.QualityValues[n];
       end;
   end;
   bmp.Free;
   result := Quality;
end;

// show symbols OCRs as chars in TOCRed
procedure ShowSymbGrid;
var n, cx, cy, cxMax, cyMax, i: integer;
    s: string[255];
    OcrText: array[0..MaxTextLins] of string[255];
begin
   // fill with blannk text
   cxMax := 0;
   cyMax := 0;
   for n := 0 to SymbList.nSymb-1 do begin
       cx := SymbList.Box[n].cx;
       cy := SymbList.Box[n].cy;
       if cx > cxMax then cxMax := cx;
       if cy > cyMax then cyMax := cy;
   end;
   SymbList.nCol := cxMax+1;
   SymbList.nRow := cyMax+1;
   // init OCR text
   s := '';
   for n := 0 to cxMax do s := s + ' ';
   for n := 0 to cyMax do begin
      OcrText[n] := s;
   end;
   // compose OCR'ed text from symbols
   for n := 0 to SymbList.nSymb-1 do begin
       cx := SymbList.Box[n].cx;
       cy := SymbList.Box[n].cy;
       if cx < 0 then continue;
       if cy < 0 then continue;
       if SymbList.Box[n].OCRState = syIgnore then continue;
       if (SymbList.Box[n].OCRState = syUserIdent) or (SymbList.Box[n].OCRState = syOkIdent) then begin
          s := SymbList.Box[n].Ascii;
       end else begin
          s := '?';
       end;
       for i := 1 to length(s) do begin
          OcrText[cy][cx+i] := s[i];
       end;
   end;
   FoMain.TOcrText.Lines.BeginUpdate;
   FoMain.TOcrText.Lines.Clear;
   for n := 0 to cyMax do begin
      FoMain.TOcrText.Lines.Add(OcrText[n]);
   end;
   FoMain.TOcrText.Lines.EndUpdate;
end;

// display match info in TMatch memo on GUI
procedure DisplaySymbMatch(n: integer);
var s: string;
    i,MatchOk: integer;
    bAscii, bMatchList: boolean;
begin
   if (n < 0) or (n >= SymbList.nSymb) then exit;
   // fill Mach tab with Symbol info
   FoMain.TMatch.Lines.Clear;
   FoMain.TMatch.Lines.Add('Symbol Number: ' + IntToStr(n));
   FoMain.TMatch.Lines.Add('Col: ' + IntToStr(SymbList.Box[n].cx+1) + ', ' +
                           'Row: ' + IntToStr(SymbList.Box[n].cy+1));
   FoMain.TMatch.Lines.Add('Width: ' + IntToStr(SymbList.Box[n].wi) + ', ' +
                           'Height: ' + IntToStr(SymbList.Box[n].he));
   bAscii := false; bMatchList := false;
   case SymbList.Box[n].OCRState of
      syNotIdent:            begin s := 'Symbol not identified'; end;
      syIgnore:              begin s := 'Symbol marked as Ignore'; end;
      syUserIdent:           begin s := 'Symbol identified by user'; bAscii := true; end;
      syOkIdent:             begin s := 'OK'; bAscii := true; bMatchList := true; end;
      syNOk_MultipleMatchs:  begin s := 'NOK (multiple Matches)'; bMatchList := true; end;
      syNOk_LowQualityMatch: begin s := 'NOK (low-quality Matches)'; bMatchList := true; end;
      syNOk_NoMatch:         begin s := 'NOK (No match for this symbol)'; end;
      syNOk_NoFit:           begin s := 'NOK (Symbol does not fit)'; bAscii := true; bMatchList := true; end;
      else begin
         s := '!!!!! Unknown state !!!!!'
      end;
   end;
   FoMain.TMatch.Lines.Add('OCR State: ' + s);
   if bAscii then s := SymbList.Box[n].Ascii else s := '';
   FoMain.TMatch.Lines.Add('OCR''d as: ' + s);
   if bMatchList then begin
      FoMain.TMatch.Lines.Add('Matches: ' + IntToStr(SymbList.Box[n].nMatchs));
      for i := 0 to SymbList.Box[n].nMatchs-1 do begin
         s := '   ' + IntToStr(i) + ' - ';
         s := s + 'Quality: ' + IntToStr(SymbList.Box[n].MatchList[i].Quality) + ', ';
         s := s + 'Ascii String: "' + SymbList.Box[n].MatchList[i].Ascii + '" ';
         MatchOk := SymbList.Box[n].MatchList[i].MatchOk;
         if MatchOk=1 then s := s + '(Match Ok)';
         if MatchOk=2 then s := s + '(Best Match)';
         FoMain.TMatch.Lines.Add(s);
      end;
   end;
end;

// assign symbol at given box to a ascii string value
procedure AssignSymb(n: integer);
var s, sInp, s2: string;
    i, m, wi, he: integer;
    b: boolean;
begin
   if (n < 0) or (n >= SymbList.nSymb) then exit;
   sInp := '';
   case SymbList.Box[n].OCRState of
      syNotIdent:
         s := 'Symbol not identified';
      syIgnore:
         s := 'Symbol marked as Ignore';
      syUserIdent: begin
         sInp := SymbList.Box[n].Ascii;
         s := 'Symbol identified by user as: ' + sInp;
         if SymbList.Box[n].nMatchs = 0 then s := s + ' (but not a symbol to match)';
         end;
      syOkIdent: begin
         sInp := SymbList.Box[n].Ascii;
         s := 'Symbol OCRed as: ' + sInp;
         end;
      syNOk_MultipleMatchs: begin
         m := SymbList.Box[n].nMatchs;
         s := 'Symbol OCRed with '+IntToStr(m)+' Matches:';
         for i := 0 to m-1 do s := s + ' ' + SymbList.Box[n].MatchList[i].Ascii;
         end;
      syNOk_LowQualityMatch: begin
         m := SymbList.Box[n].nMatchs;
         s := 'Symbol OCRed with '+IntToStr(m)+' low-quality Matches:';
         for i := 0 to m-1 do s := s + ' ' + SymbList.Box[n].MatchList[i].Ascii;
         end;
      syNOk_NoMatch:
         s := 'No match for this symbol';
      syNOk_NoFit:
         s := 'Symbol OCRed as multi char symbol, but does not fits';
      else begin
         s := '!!!!! Unknown state !!!!!'
      end;
   end;
   s := s + #13#10 + #13#10 +
        'Input an Empty string to set as no identified, ' +
        'a space to set as ignore, or one or more ascii chars to be assigned to symbol';
   s2 := sInp;
   b := InputQueryEx('Assign Symbol', s, sInp, 120);
   if b=false then exit; // dialog cancelled
   if sInp = s2 then exit; // no changes
   // check if set to ignore
   if (Trim(sInp) = '') and (sInp <> '') then begin
      // input a space -> set as ignore
      SymbList.Box[n].OCRState := syIgnore;
      SymbList.Box[n].nMatchs := 0;
      SymbList.Box[n].Ascii := '';
      exit;
   end;
   // remove spaces
   s2 := sInp; sInp := '';
   for i := 1 to length(s2) do if s2[i] <> ' ' then sInp := sInp + s2[i];
   // check if set to not identified
   if sInp = '' then begin
      // input an empty string -> set as not yet ident (not yet OCR'd)
      SymbList.Box[n].OCRState := syNotIdent;
      SymbList.Box[n].nMatchs := 0;
      SymbList.Box[n].Ascii := '';
      exit;
   end;
   // check if symbol aready in match list
   s2 := '';
   for i := 0 to Param.MatchSymbList.nSymb-1 do begin
       if Param.MatchSymbList.Box[i].Ascii = sInp then begin
          s2 := ' (already defined)';
          break;
       end;
   end;
   // ask if symbol needs to be added to match list
   if MessageDlg('Add symbol to match list?' + s2,
                  mtConfirmation, [mbYes, mbNo], 0) <> mrYes then begin
      SymbList.Box[n].OCRState := syUserIdent;
      SymbList.Box[n].Ascii := sInp;
      SymbList.Box[n].nMatchs := 0;
      exit;
   end;
   // add symbol to match list
   // check if room for new symbol
   if (Param.MatchSymbList.nSymb >= MaxSymbList) then begin
      ShowMessage('Match list full');
      exit;
   end;
   if (Param.MatchSymbList.CurrentImSymbWi + SymbList.Box[n].wi + 5 >= Param.MatchSymbList.MaxImSymbWi) then begin
      ShowMessage('Match bitmap full');
      exit;
   end;
   if (SymbList.Box[n].He >= Param.MatchSymbList.MaxImSymbHe) then begin
      ShowMessage('Symbol heigh ('+IntToStr(SymbList.Box[n].He)+') too big for Match bitmap (max '+IntToStr(Param.MatchSymbList.MaxImSymbHe)+')');
      exit;
   end;
   // set symbol box white before copying it
   SetSymbBoxColor(n,255);
   // set the destination of symbol in symbol bitmap
   inc(Param.MatchSymbList.CurrentImSymbWi);
   m := Param.MatchSymbList.nSymb;
   wi := SymbList.Box[n].wi;
   he := SymbList.Box[n].he;
   Param.MatchSymbList.Box[m].Quality := 0;
   Param.MatchSymbList.Box[m].Ascii := sInp;
   Param.MatchSymbList.Box[m].x1 := Param.MatchSymbList.CurrentImSymbWi;
   Param.MatchSymbList.Box[m].x2 := Param.MatchSymbList.CurrentImSymbWi + wi - 1;
   Param.MatchSymbList.Box[m].y1 := 1;
   Param.MatchSymbList.Box[m].y2 := he;
   Param.MatchSymbList.Box[m].wi := wi;
   Param.MatchSymbList.Box[m].he := he;
   Param.MatchSymbList.Box[m].rot := SymbList.rot;
   inc(Param.MatchSymbList.CurrentImSymbWi, wi + 2);
   inc(Param.MatchSymbList.nSymb);
   BitBlt(
     Param.MatchSymbList.ImSymb.Canvas.Handle,
     Param.MatchSymbList.Box[m].x1, Param.MatchSymbList.Box[m].y1, wi, he, // destination
     FoMain.ImSrc.Picture.Bitmap.Canvas.Handle, SymbList.Box[n].x1, SymbList.Box[n].y1,
     SRCCOPY);
   // draw box arround symbol bitmap
   Param.MatchSymbList.ImSymb.Canvas.MoveTo(Param.MatchSymbList.Box[m].x1-1, Param.MatchSymbList.Box[m].y1-1);
   Param.MatchSymbList.ImSymb.Canvas.LineTo(Param.MatchSymbList.Box[m].x2+1, Param.MatchSymbList.Box[m].y1-1);
   Param.MatchSymbList.ImSymb.Canvas.LineTo(Param.MatchSymbList.Box[m].x2+1, Param.MatchSymbList.Box[m].y2+1);
   Param.MatchSymbList.ImSymb.Canvas.LineTo(Param.MatchSymbList.Box[m].x1-1, Param.MatchSymbList.Box[m].y2+1);
   Param.MatchSymbList.ImSymb.Canvas.LineTo(Param.MatchSymbList.Box[m].x1-1, Param.MatchSymbList.Box[m].y1-1);
   // update the Symb list to point to the new created symbol
   SymbList.Box[n].OCRState := syUserIdent;
   SymbList.Box[n].Ascii := sInp;
   SymbList.Box[n].nMatchs := 1;
   SymbList.Box[n].MatchList[0].MatchSymb := m;
   SymbList.Box[n].MatchList[0].Ascii := sInp;
   SymbList.Box[n].MatchList[0].Quality := 0; // 0-> perfect match
   // calc max quality value
   Param.MatchSymbList.Box[m].Quality := GetQualityMatch(
     Param.MatchSymbList.ImSymb.Canvas.Handle,
     Param.MatchSymbList.Box[m].x1, Param.MatchSymbList.Box[m].y1, wi, he,     // bitmap1
     Param.MatchSymbList.ImSymb.Canvas.Handle,
     Param.MatchSymbList.Box[m].x1, Param.MatchSymbList.Box[m].y1, wi, he, // bitmap2
     Param.UnalignForMinQuality, Param.UnalignForMinQuality);
   // mark as modified
   Param.MatchSymbList.MatchSymbListChanged := true;
end;

// assign symbol at given symb to a ascii string value
procedure AssignSymb2(n: integer; Ascii: string);
begin
   if (n < 0) or (n >= SymbList.nSymb) then exit;
   if Ascii = '' then begin
      SymbList.Box[n].OCRState := syNotIdent;
      SymbList.Box[n].nMatchs := 0;
      SymbList.Box[n].Ascii := '';
   end else if Ascii = ' ' then begin
      SymbList.Box[n].OCRState := syIgnore;
      SymbList.Box[n].nMatchs := 0;
      SymbList.Box[n].Ascii := '';
   end else begin
      SymbList.Box[n].OCRState := syUserIdent;
      SymbList.Box[n].nMatchs := 0;
      SymbList.Box[n].Ascii := Ascii;
   end;
end;

procedure CalcSymbMatch;
var n, m, i, j, Q, Quality, nOK, nNOK: integer;
    dx, dy, ix, iy, x0, y0: integer;
    OCRState: TSymbOCRStates;
    s, Ascii: string;
    b: boolean;
    StartTime: TDateTime;

    function GetQual(ix, iy: integer): integer;
    begin
       result := GetQualityMatch(
                   Param.MatchSymbList.ImSymb.Canvas.Handle,
                   Param.MatchSymbList.Box[m].x1, Param.MatchSymbList.Box[m].y1,
                   Param.MatchSymbList.Box[m].wi, Param.MatchSymbList.Box[m].he,     // bitmap1
                   FoMain.ImSrc.Picture.Bitmap.Canvas.Handle,
                   SymbList.Box[n].x1, SymbList.Box[n].y1,
                   SymbList.Box[n].wi, SymbList.Box[n].he,
                   ix,iy);
    end;

begin
   Log('INFO: Start OCR processing ... ');
   StartTime := Now;
   ClearImSrc;
   nOk := 0; nNOK := 0;
   for n := 0 to SymbList.nSymb-1 do begin
      // for each symbol isolated
      OCRState := SymbList.Box[n].OCRState;
      // if already OCR'ed skip
      if (OCRState = syNOk_MultipleMatchs) or
         (OCRState = syNOk_LowQualityMatch) or
         (OCRState = syNOk_NoFit) or
         (OCRState = syNOk_NoMatch) then SymbList.Box[n].OCRState := syNotIdent;
   end;
   for n := 0 to SymbList.nSymb-1 do begin
      // for each symbol isolated
      OCRState := SymbList.Box[n].OCRState;
      // if already OCR'ed skip
      if (OCRState = syIgnore) or (OCRState = syUserIdent) or (OCRState = syOkIdent) then continue;
      // check match with each one in match list
      SymbList.Box[n].nMatchs := 0;
      // set symbol box white before scanning it
      SetSymbBoxColor(n,255);
      for m := 0 to Param.MatchSymbList.nSymb-1 do begin
          if Param.MatchSymbList.Box[m].Quality < 0 then continue; // match disabled
          // locate best match
          dx := Param.MatchSymbList.Box[m].wi div 2;
          dy := Param.MatchSymbList.Box[m].he div 2;
          // check if too different in size
          if (SymbList.Box[n].wi < Param.MatchSymbList.Box[m].wi div 2) or
             (SymbList.Box[n].wi div 2 > Param.MatchSymbList.Box[m].wi) or
             (SymbList.Box[n].he < Param.MatchSymbList.Box[m].he div 2) or
             (SymbList.Box[n].he div 2 > Param.MatchSymbList.Box[m].he) then continue; // more than x2 in horiz or vert dimension
          // scan x axis looking for best match
          Quality := -1;
          x0 := 0;
          b := true;
          for ix := -dx to dx do begin
              b := not b; if b then continue; // for -dx to dx step 2
              Q := GetQual(ix,0);
              if (Quality < 0) or (Q < Quality) then begin
                 Quality := Q; // this is the best match on x axis
                 x0 := ix;
              end;
          end;
          // scan neighbour pos for best scan just in case step 2 has missed the best match
          ix := 0;
          Q := GetQual(x0-1,0);
          if (Q < Quality) then begin
             Quality := Q; // this is the best match on x axis
             ix := -1;
          end;
          Q := GetQual(x0+1,0);
          if (Q < Quality) then begin
             ix := +1;
          end;
          x0 := x0 + ix;
          if dx < abs(x0) then begin
             // bets match is outside a MatchAjust % of symbol box -> no central match -> no match -> skip this match symbol
             continue;
          end;
          // scan y axis
          Quality := -1;
          y0 := 0;
          b := true;
          for iy := -dy to dy do begin
              b := not b; if b then continue;
              Q := GetQual(x0,iy);
              if (Quality < 0) or (Q < Quality) then begin
                 Quality := Q; // this is the best match on x axis
                 y0 := iy;
              end;
          end;
          // scan neighbour pos for best scan just in case step 2 has missed the best match
          iy := 0;
          Q := GetQual(x0, y0-1);
          if (Q < Quality) then begin
             Quality := Q; // this is the best match on x axis
             iy := -1;
          end;
          Q := GetQual(x0, y0+1);
          if (Q < Quality) then begin
             Quality := Q;
             iy := +1;
          end;
          y0 := y0 + iy;
          if dy < abs(y0) then begin
             continue;
          end;
          // See if already matched another symbol with same ascii value (alternate
          for i := 0 to SymbList.Box[n].nMatchs-1 do begin
             if (SymbList.Box[n].MatchList[i].Ascii <> Param.MatchSymbList.Box[m].Ascii) then continue;
             // same ascii already matched. Keep the best one
             if (SymbList.Box[n].MatchList[i].Quality < Quality) then begin
                Quality := -1; // the existing match in match list is best -> ignore the current match
                break;
             end;
             // this one is best -> remove the previous one in match list
             for j := i+1 to SymbList.Box[n].nMatchs-1 do begin
                SymbList.Box[n].MatchList[j-1] := SymbList.Box[n].MatchList[j];
             end;
             dec(SymbList.Box[n].nMatchs);
             break;
          end;
          if Quality < 0 then break;
          // add matched symbol in match list ordered by quality
          i := -1;
          for j := 0 to SymbList.Box[n].nMatchs-1 do begin
             if SymbList.Box[n].MatchList[j].Quality < Quality then continue;
             // insert match list at pos j -> make room
             if SymbList.Box[n].nMatchs < MaxMatchList then inc(SymbList.Box[n].nMatchs);
             for i := SymbList.Box[n].nMatchs-1 downto j+1 do begin
                SymbList.Box[n].MatchList[i] := SymbList.Box[n].MatchList[i-1];
             end;
             i := j;
             break;
          end;
          if (i < 0) then begin
             // is worse match -> put at the end of matchlist if room
             if SymbList.Box[n].nMatchs < MaxMatchList then begin
                inc(SymbList.Box[n].nMatchs);
                i := SymbList.Box[n].nMatchs-1;
             end;
          end;
          // add new match
          if (i >= 0) then begin
             SymbList.Box[n].MatchList[i].MatchSymb := m;
             SymbList.Box[n].MatchList[i].Ascii := Param.MatchSymbList.Box[m].Ascii;
             SymbList.Box[n].MatchList[i].Quality := Quality;
             SymbList.Box[n].MatchList[i].MatchOk := 0;
          end;
      end;
      // now check the matchlist to decide if something matched Ok and set the symbol status
      OCRState := syNOk_NoMatch;
      Ascii := '';
      // check if firts match y muche better than the second
      if OCRState = syNOk_NoMatch then begin
          if (SymbList.Box[n].nMatchs > 1) and
             (SymbList.Box[n].MatchList[0].Quality <
              SymbList.Box[n].MatchList[1].Quality * Param.BestMatchFactor div 100) then begin
             OCRState := syOkIdent;
             m := SymbList.Box[n].MatchList[0].MatchSymb;
             Ascii := Param.MatchSymbList.Box[m].Ascii;
             SymbList.Box[n].MatchList[0].MatchOk := 2; // best match
          end;
      end;
      // check how many matches are good ones (so low Q value)
      if OCRState = syNOk_NoMatch then begin
         j := 0;
         for i := 0 to SymbList.Box[n].nMatchs-1 do begin
             m := SymbList.Box[n].MatchList[i].MatchSymb;
             if (SymbList.Box[n].MatchList[i].Quality > Param.MatchSymbList.Box[m].Quality) then begin
                SymbList.Box[n].MatchList[i].MatchOk := 0; // bad qual match
             end else begin
                // good quality match
                SymbList.Box[n].MatchList[i].MatchOk := 1;
                inc(j);
                if j = 1 then begin
                   OCRState := syOkIdent;
                   Ascii := Param.MatchSymbList.Box[m].Ascii;
                end else if j < i+1 then begin
                   OCRState := syNOk_LowQualityMatch;
                   Ascii := '';
                end else begin
                   OCRState := syNOk_MultipleMatchs;
                   Ascii := '';
                end;
             end;
         end;
      end;
      // if match has multiple chars, check enough room
      if (OCRState = syOkIdent) and (length(Ascii) > 1) then begin
         // check no other symbol overlaps
         for i := 0 to SymbList.nSymb-1 do begin
            if OCRState <> syOkIdent then break;
            if i = n then continue;
            if SymbList.Box[i].OCRState = syIgnore then continue;
            if SymbList.Box[i].cy <> SymbList.Box[n].cy  then continue;
            for j := 1 to length(Ascii)-1 do begin
                if SymbList.Box[i].cx = SymbList.Box[n].cx + j then begin
                   OCRState := syNOk_NoFit;
                   Ascii := '';
                end;
            end;
         end;
      end;
      if OCRState = syOkIdent then inc(nOK) else inc(nNOK);
      SymbList.Box[n].OCRState := OCRState;
      SymbList.Box[n].Ascii := Ascii;
      if not FoMain.Progress(
         'Processed ' + IntToStr(n+1) + ' of ' + IntToStr(SymbList.nSymb) +
         ' (' + IntToStr(100*(n+1) div SymbList.nSymb) + '%)',
         True) then begin
         Log('INFO: OCR Stopped by user');
         break;
      end;
   end;
   // mark as not fit two symbols going to same col/row cx,cy
   for n := 0 to SymbList.nSymb-1 do begin
      if SymbList.Box[n].OCRState <> syOkIdent then continue;
      for i := 0 to SymbList.nSymb-1 do begin
         if i = n then continue;
         if SymbList.Box[i].cx <> SymbList.Box[n].cx then continue;
         if SymbList.Box[i].cy <> SymbList.Box[n].cy then continue;
         // symb n and i share same cx/cy -> mark both as no fit
         SymbList.Box[n].OCRState := syNOk_NoFit;
         SymbList.Box[i].OCRState := syNOk_NoFit;
      end;
   end;
   FoMain.Progress('OCR Finished', False);
   s := 'OCR Ok: '+IntToStr(nOK)+' Symbols, ' +
        'No Ok: '+IntToStr(nNOK);
   Log('INFO: ' + s);
   if nOK + nNOK > 0 then begin
      s := FloatToStrF( 100.0 * nOK / (nOK + nNOK), ffFixed, 3, 2);
      for i := 1 to length(s) do if s[i] = ',' then s[i] := '.';
      s := 'Accuracy: ' + s + '%';
      Log('INFO: ' + s);
   end;
   n := round(24*3600*frac(Now-StartTime)); // seconds elapsed from begin of process
   s := 'Elapsed: ' + IntToStr(n) + ' sec. ';
   if n > 0 then begin
      s := s + 'Speed: ' +
               FloatToStrF( (nOK + nNOK) / n, ffFixed, 4, 2) +
               ' symb/sec';
      for i := 1 to length(s) do if s[i] = ',' then s[i] := '.' else if s[i] = '.' then s[i] := ','
   end;
   Log('INFO: ' + s);
end;

// select symbol at pos x,y in Source Image and return its number
function SelSymbAtXY(x,y: integer): integer;
var i: integer;
begin
   result := -1;
   for i := 0 to SymbList.nSymb-1 do begin
       if x < SymbList.Box[i].x1 then continue;
       if x > SymbList.Box[i].x2 then continue;
       if y < SymbList.Box[i].y1 then continue;
       if y > SymbList.Box[i].y2 then continue;
       result := i;
       break;
   end;
end;


// select symbol at char pos cx,cy in Source Image and return its number
function SelSymbAtCXY(cx,cy: integer): integer;
var i: integer;
begin
   result := -1;
   for i := 0 to SymbList.nSymb-1 do begin
       if cx <> SymbList.Box[i].cx then continue;
       if cy <> SymbList.Box[i].cy then continue;
       result := i;
       break;
   end;
end;

// Colorize symbol Box n with color c0, return assigned color
function SetSymbBoxColor(n, c0: integer): integer;
var x,y,c: integer;
    P: PByteArray;
begin
    result := 0;
    if (n < 0) or (n >= SymbList.nSymb) then exit;
    result := SymbList.Box[n].Color;
    for y := SymbList.Box[n].y1 to SymbList.Box[n].y2 do begin
        P := FoMain.ImSrc.Picture.Bitmap.ScanLine[y];
        for x := SymbList.Box[n].x1 to SymbList.Box[n].x2 do begin
            c := P[x];
            if c < 10 then begin
                P[x] := 0; // set pixel black again
                continue;
            end;
            P[x] := c0;
        end;
    end;
end;

// recursive computation of symbols box
var Calc: record
             W, H: integer;              // wifth/heigth of full source scanned bitmap
             x1,x2, y1, y2: integer;     // current boundaries of symbol box
             Level,Hi,HiLevel: integer;  // nesting level control to allow stack overflow on large black zones
          end;
procedure CalcSymbBox(x,y: integer);
var P: PByteArray;
    xx,yy,ix,iy: integer;
begin
   // set central pixel as scanned, update symbol box size if needed
   P := FoMain.ImSrc.Picture.Bitmap.ScanLine[y];
   P[x] := 1;
   if Calc.Level > 1000 then exit;
   inc(Calc.Level);
   if Calc.Level > Calc.Hi then Calc.Hi := Calc.Level;
   if (x < Calc.x1) then begin Calc.x1 := x; end else
   if (x > Calc.x2) then begin Calc.x2 := x; end;
   if (y < Calc.y1) then begin Calc.y1 := y; end else
   if (y > Calc.y2) then begin Calc.y2 := y; end;
   if (Calc.y2 - Calc.y1 > Param.MaxSymbolHe) or
      (Calc.x2 - Calc.x1 > Param.MaxSymbolWi) then begin
      // symbol too big ... abort scan
   end else begin
   // scan array 3x3 of adjacent pixels
      for iy := -1 to 1 do begin
         yy := y + iy;
         if (yy < 0) or (yy >= Calc.H) then continue;
         P := FoMain.ImSrc.Picture.Bitmap.ScanLine[yy];
         for ix := -1 to 1 do begin
            xx := x + ix;
            if (xx < 0) or (xx >= Calc.W) or (P[xx] <> 0) then continue;
            CalcSymbBox(xx,yy);
         end;
      end;
   end;
   dec(Calc.Level);
   if Calc.Level = 0 then begin
      if (Calc.Hi < 1000) and (Calc.Hi > Calc.HiLevel) then begin
         Calc.HiLevel := Calc.Hi;
      end;
      Calc.Hi := 0;
   end;
end;


// identify symbols in current bitmap
procedure IsolateSymb;
var he, wi, x, y, wx, wy, n: integer;
    P: PByteArray;
begin
   // process the image to have 1 byte per pixel, 0=black dot, 255=white pixel
   he := FoMain.ImSrc.Picture.Bitmap.Height;
   wi := FoMain.ImSrc.Picture.Bitmap.Width;
   ClearImSrc;
   // init the chars array and symbol list
   SymbList.nSymb   := 0;
   //identify symbols as connected series of pixels
   Calc.W := wi; Calc.H := he;
   Calc.Level := 0; Calc.Hi := 0; Calc.HiLevel := 0;
   for y := 0 to he-1 do begin
      P := FoMain.ImSrc.Picture.Bitmap.ScanLine[y];
      for x := 0 to wi-1 do begin
          if P[x] <> 0 then continue;
          // block pixel -> identify sybol box arround
          Calc.x1 := x; Calc.y1 := y;
          Calc.x2 := x; Calc.y2 := y;
          CalcSymbBox(x,y);
          // check if box is big enough to be considered a symbol
          wx := Calc.x2 - Calc.x1 + 1;
          wy := Calc.y2 - Calc.y1 + 1;
          if (wx < Param.MinSymbolWi) or (wy < Param.MinSymbolHe) then continue;
          // it's big enought -> add to symbol list
          n := SymbList.nSymb;
          if n >= MaxSymbList then begin
             Log('ERROR: More than '+IntToStr(MaxSymbList)+' Symbols');
             exit;
          end;
          // save the symbol box
          SymbList.Box[n].x1 := Calc.x1;
          SymbList.Box[n].x2 := Calc.x2;
          SymbList.Box[n].y1 := Calc.y1;
          SymbList.Box[n].y2 := Calc.y2;
          SymbList.Box[n].wi := wx;
          SymbList.Box[n].he := wy;
          SymbList.Box[n].OCRState := syNotIdent;
          SymbList.Box[n].Color := 255; // white symbol box
          SymbList.nSymb := n+1;
          FoMain.Progress(
             'Found ' + IntToStr(n+1) + ', ' +
             'Processed ' + IntToStr((100*y) div (he-1)) + '%', True);
      end;
   end;
   FoMain.Progress('', False);
   Log('INFO: '+IntToStr(SymbList.nSymb)+' Symbols found');
end;

// locate symbols in grid and calc cx and cy por of each symbol
procedure CalcSymbGrid;
var i, n, n1, n2, dx, dy, d, dMin, dMin2, x, y, he, wi: integer;
    nx, ny, nIni, nEnd: integer;
    slope, chdx, chdy: double;
    y1, y2, x1, x2: integer;
    cx, cy, nCol, nRow, nSymbHome: integer;
    nMax: integer;
    Pos: array [0..MaxSymbList] of record // symbol at south (dn) and east (ri)
                                      dn_n,ri_n: integer;          // number of the symbol at right/donw
                                      up_n,le_n: integer;
                                      NormalSize, cxySet: integer;
                                   end;
    Count: array[0..400] of integer;
    bmp: TBitMap;
    Rect: TRect;
    P0, P1: PByteArray;
    StepMode, StepCount: integer;

   procedure NormalizeRowCols(var nCol, nRow: integer);
   var cx, cy, n, x,y: integer;
   begin
      // normalize rows & cols value: get min col/row value, then set them as 0,0
      cx := 999; cy := 999;
      for n := 0 to SymbList.nSymb-1 do begin
          x := SymbList.Box[n].cx;
          y := SymbList.Box[n].cy;
          if x < cx then cx := x;
          if y < cy then cy := y;
      end;
      for n := 0 to SymbList.nSymb-1 do begin
          SymbList.Box[n].cx := SymbList.Box[n].cx - cx;
          SymbList.Box[n].cy := SymbList.Box[n].cy - cy;
      end;
      // get max col/row
      cx := 0; cy := 0;
      for n := 0 to SymbList.nSymb-1 do begin
         x := SymbList.Box[n].cx;
         y := SymbList.Box[n].cy;
         if x > cx then cx := x;
         if y > cy then cy := y;
      end;
      nCol := cx + 1;
      nRow := cy + 1;
   end;

   procedure SetColRow(var cx, cy: integer; x,y: integer; chdx, chdy, slope1, slope2: double);
   var A,B,C,D: double;
   begin
      // x := round(cx * chdx + cy * chdy * slope2 / 1000) + x0;  //x0,y0=coord of box[cx=xy=0]
      // y := round(cy * chdy + cx * chdx * slope1 / 1000) + y0;  //coord on InSrc of box[cx,xy]
      A := chdx; B := chdy * slope2 / 1000;
      C := chdx * slope1 / 1000; D := chdy;
      cx := round((D * x - y * B ) / (D*A + C*B));
      cy := round((y * A - C * x ) / (D*A + C*B));
   end;

   function CountBoxPixelsCol(x, y1, he: integer): integer;
   var y, n: integer;
       P: PByteArray;
   begin
      n := 0;
      for y := y1 to y1 + he-1 do begin
         P := FoMain.ImSrc.Picture.Bitmap.ScanLine[y];
         if P[x] < 10 then inc(n);
      end;
      result := n;
   end;

   function CountBoxPixelsRow(x1, y, wi: integer): integer;
   var x, n: integer;
       P: PByteArray;
   begin
      n := 0;
      P := FoMain.ImSrc.Picture.Bitmap.ScanLine[y];
      for x := x1 to x1 + wi-1 do begin
         if P[x] < 10 then inc(n);
      end;
      result := n;
   end;

   procedure CalcMax(var max, val: integer);
   var i, n: integer;
   begin
       max := 0; val := 0;
       for i := 0 to 400 do begin
           n := Count[i];
           if n > max then begin
              max := n;
              val := i;
           end;
       end;
   end;


begin
   // combine symbols whose boxes are a fragmented symbol using a 20% bigger symbol box
   wi := SymbList.wi * (100 + Param.BoxOversizeForCombineFragments) div 100;
   he := SymbList.he * (100 + Param.BoxOversizeForCombineFragments) div 100;
   for n1 := 0 to SymbList.nSymb-1 do begin
      if SymbList.Box[n1].x1 = -1 then continue;
      for n2 := 0 to SymbList.nSymb-1 do begin
         if n1 = n2 then continue;
         if SymbList.Box[n2].x1 = -1 then continue;
         // check if combined symbols size fits in wi, he
         if (max(SymbList.Box[n1].x2, SymbList.Box[n2].x2) - min(SymbList.Box[n1].x1, SymbList.Box[n2].x1) > wi) or
            (max(SymbList.Box[n1].y2, SymbList.Box[n2].y2) - min(SymbList.Box[n1].y1, SymbList.Box[n2].y1) > he) then begin
            // combinend symbol too big -> do not combine
            continue;
         end;
         // combine symbols -> box is the one that contains both symbols
         SymbList.Box[n1].x1 := min(SymbList.Box[n1].x1, SymbList.Box[n2].x1);
         SymbList.Box[n1].y1 := min(SymbList.Box[n1].y1, SymbList.Box[n2].y1);
         SymbList.Box[n1].x2 := max(SymbList.Box[n1].x2, SymbList.Box[n2].x2);
         SymbList.Box[n1].y2 := max(SymbList.Box[n1].y2, SymbList.Box[n2].y2);
         SymbList.Box[n1].wi := SymbList.Box[n1].x2 - SymbList.Box[n1].x1 + 1;
         SymbList.Box[n1].he := SymbList.Box[n1].y2 - SymbList.Box[n1].y1 + 1;
         // mark box[n2] to be discarded
         SymbList.Box[n2].x1 := -1;
      end;
   end;
   // clean up discarded symbols
   n1 := 0;
   for n2 := 0 to SymbList.nSymb-1 do begin
      if SymbList.Box[n2].x1 = -1 then continue;
      SymbList.Box[n1] := SymbList.Box[n2];
      inc(n1);
   end;
   SymbList.nSymb := n1;
   // separate horizontal coalesced symbols
   n1 := -1;
   repeat
      inc(n1);
      if n1 > SymbList.nSymb-1 then break;
      if (SymbList.Box[n1].he < SymbList.he * 0.7) or (SymbList.Box[n1].he > SymbList.he * 1.3) then continue;
      if (SymbList.Box[n1].wi < SymbList.wi * 1.7) then continue; // symb must be normal height and 2x (or more) width
      // determine point of less contact
      wi := -1; dMin := 0;
      for x := SymbList.wi div 2 to SymbList.wi * 3 div 2 do begin
         d := CountBoxPixelsCol(SymbList.Box[n1].x1 + x, SymbList.Box[n1].y1, SymbList.Box[n1].he);
         if (wi < 0) or (d < dMin) then begin
            dMin := d;
            wi := x;
         end;
      end;
      if (dMin > CountBoxPixelsCol(SymbList.Box[n1].x1 + SymbList.wi div 2 - 1, SymbList.Box[n1].y1, SymbList.Box[n1].he)) or
         (dMin > CountBoxPixelsCol(SymbList.Box[n1].x1 + SymbList.wi * 3 div 2 + 1, SymbList.Box[n1].y1, SymbList.Box[n1].he)) or
         (wi < 3) then continue; // symb has no point of less contact -> do not divide
      // separate the symbols: leave symb with first char, create new one with rest of symbol
      n2 := SymbList.nSymb;
      if n2 >= MaxSymbList then begin
          Log('ERROR: More than '+IntToStr(MaxSymbList)+' Symbols');
          exit;
      end;
      inc(SymbList.nSymb);
      SymbList.Box[n2] := SymbList.Box[n1];
      SymbList.Box[n1].wi := wi; // symb n1 is the first symbol, n2 is the remaining
      SymbList.Box[n1].x2 := SymbList.Box[n1].x1 + wi - 1;
      SymbList.Box[n2].x1 := SymbList.Box[n2].x1 + wi + 1;
      SymbList.Box[n2].wi := SymbList.Box[n2].x2 - SymbList.Box[n2].x1 + 1;
      // now adjust Box he of separated symbols
      while CountBoxPixelsRow(SymbList.Box[n1].x1, SymbList.Box[n1].y1, SymbList.Box[n1].wi) = 0 do begin
          inc(SymbList.Box[n1].y1); dec(SymbList.Box[n1].he);
          if SymbList.Box[n1].he <= 3 then break;
      end;
      while CountBoxPixelsRow(SymbList.Box[n1].x1, SymbList.Box[n1].y2, SymbList.Box[n1].wi) = 0 do begin
          dec(SymbList.Box[n1].y2); dec(SymbList.Box[n1].he);
          if SymbList.Box[n1].he <= 3 then break;
      end;
      while CountBoxPixelsRow(SymbList.Box[n2].x1, SymbList.Box[n2].y1, SymbList.Box[n2].wi) = 0 do begin
          inc(SymbList.Box[n2].y1); dec(SymbList.Box[n2].he);
          if SymbList.Box[n2].he <= 3 then break;
      end;
      while CountBoxPixelsRow(SymbList.Box[n2].x1, SymbList.Box[n2].y2, SymbList.Box[n2].wi) = 0 do begin
          dec(SymbList.Box[n2].y2); dec(SymbList.Box[n2].he);
          if SymbList.Box[n2].he <= 3 then break;
      end;
   until false;
   // mark normal sized symbols
   wi := round(SymbList.wi * 1.3);
   he := round(SymbList.he * 1.3);
   dx := round(SymbList.wi * 0.7);
   dy := round(SymbList.he * 0.7);
   for n1 := 0 to SymbList.nSymb-1 do begin
      Pos[n1].NormalSize := 1;
      if (SymbList.Box[n1].wi >= wi) or (SymbList.Box[n1].he >= he) then begin
         Pos[n1].NormalSize := 0; // symbol > 20% of most frequet size
         continue;
      end ;
      if (SymbList.Box[n1].wi <= dx) or (SymbList.Box[n1].he <= dy) then begin
         Pos[n1].NormalSize := 0; // symbol < 20% of most frequet size
         continue;
      end ;
   end;

   // determine each symbol closer neighbour in down/right (south) direction
   for n1 := 0 to SymbList.nSymb-1 do begin
      Pos[n1].ri_n := -1;
      dMin := 0;
      for n2 := 0 to SymbList.nSymb-1 do begin
         if n1 = n2 then continue;
         dx := SymbList.Box[n2].x1 - SymbList.Box[n1].x1; // use upper left corner of symbol
         dy := SymbList.Box[n2].y1 - SymbList.Box[n1].y1;
         if abs(dy) * 6 <  dx then begin
            // neighbour at the right
            d := dx*dx + dy*dy;
            if (dMin = 0) or (dMin > d) then begin
               dMin := d;
               Pos[n1].ri_n := n2;
            end;
         end;
      end;
   end;
   for n1 := 0 to SymbList.nSymb-1 do begin
      Pos[n1].dn_n := -1;
      dMin := 0;
      for n2 := 0 to SymbList.nSymb-1 do begin
         if n1 = n2 then continue;
         dx := SymbList.Box[n2].x1 - SymbList.Box[n1].x1; // use lower left corner of symbol
         dy := SymbList.Box[n2].y2 - SymbList.Box[n1].y2;
         if abs(dx) * 6 < dy then begin
            // neighbour at the south (down neighbour)
            d := dx*dx + dy*dy;
            if (dMin = 0) or (dMin > d) then begin
               dMin := d;
               Pos[n1].dn_n := n2;
            end;
         end;
      end;
   end;
   // determine most frequent slope of line (using only normal size symb)
   FillChar(Count, SizeOf(Count), 0);
   for n1 := 0 to SymbList.nSymb-1 do begin
      if Pos[n1].NormalSize = 0 then continue;
      n2 := Pos[n1].ri_n;
      if n2 < 0 then continue; // no neighbour
      if Pos[n2].NormalSize = 0 then continue;
      dx := SymbList.Box[n2].x1 - SymbList.Box[n1].x1; // use lower left corner of symbol
      dy := SymbList.Box[n2].y2 - SymbList.Box[n1].y2;
      if abs(dx) < 10 then continue;       // symbols separated by 10 pixels min for slope calculation
      d := round(1000.0 * dy / dx) + 200 ; // pixels up/donn eaxh 1000 horizontal pixels, normalize to -200 .. + 200
      if (d > 0) and (d < 400) then inc(Count[d]);
   end;
   CalcMax(nx, d); // max value is d (appears nx times)
   chdx := d - 200;
   // determine most frequent slope of column
   FillChar(Count, SizeOf(Count), 0);
   for n1 := 0 to SymbList.nSymb-1 do begin
      if Pos[n1].NormalSize = 0 then continue;
      n2 := Pos[n1].dn_n;
      if n2 < 0 then continue; // no neighbour
      if Pos[n2].NormalSize = 0 then continue; 
      dx := SymbList.Box[n2].x1 - SymbList.Box[n1].x1; // use upper left corner of symbol
      dy := SymbList.Box[n2].y1 - SymbList.Box[n1].y1;
      if abs(dy) < 10 then continue;       // symbols separated by 10 pixels min for slope calculation
      d := round(1000.0 * dx / dy) + 200 ; // pixels left/right eaxh 1000 vertical pixels, normalize to -200 .. + 200
      if (d > 0) and (d < 400) then inc(Count[d]);
   end;
   CalcMax(ny, d); // max value is d (appears n times)
   chdy := d - 200;
   // get the slope based on the bigger number of measurements
   if nx > ny then slope := chdx else slope := chdy;
   // recalculate each symbol closer neighbour in right (east) direction give the line slope calculated
   for n1 := 0 to SymbList.nSymb-1 do begin
      Pos[n1].le_n := -1;
      Pos[n1].ri_n := -1;
      dMin := 0;
      for n2 := 0 to SymbList.nSymb-1 do begin
         if n1 = n2 then continue;
         dx := SymbList.Box[n2].x1 - SymbList.Box[n1].x1;
         if dx < 0 then continue;
         if dx < SymbList.wi div 4 then continue; // too close -> discard
         y1 := round(dx * slope / 1000) + SymbList.Box[n1].y1;
         y2 := y1 + min(SymbList.Box[n1].he, he);
         if (min(SymbList.Box[n2].he, he) + SymbList.Box[n2].y1 < y1) or
            (y2 < SymbList.Box[n2].y1) then continue; // symbols out of band
         dy := SymbList.Box[n2].y1 - SymbList.Box[n1].y1;
         d := dx*dx + dy*dy;
         if (dMin = 0) or (dMin > d) then begin
            dMin := d;
            Pos[n1].ri_n := n2;
         end;
      end;
      (* trace right neighbour on gui
      n2 := Pos[n1].ri_n;
      if not FoMain.Progress('Symb ' + IntToStr(n1) + ' -> ' + IntToStr(n2)) then exit;
      FoMain.DoStep(n1, n2);
      *)
   end;
   // recalculate each symbol closer neighbour in down (south) direction give the line slope calculated
   for n1 := 0 to SymbList.nSymb-1 do begin
      Pos[n1].up_n := -1;
      Pos[n1].dn_n := -1;
      dMin := 0;
      for n2 := 0 to SymbList.nSymb-1 do begin
         if n1 = n2 then continue;
         dy := SymbList.Box[n2].y1 - SymbList.Box[n1].y1;
         if dy < 0 then continue;
         if dy < SymbList.he div 4 then continue;
         x1 := round(dy * slope / 1000) + SymbList.Box[n1].x1;
         x2 := x1 + min(SymbList.Box[n1].wi, wi);
         if (min(SymbList.Box[n2].wi, wi) + SymbList.Box[n2].x1 < x1) or (x2 < SymbList.Box[n2].x1) then continue; // symbols out of band
         dx := SymbList.Box[n2].x1 - SymbList.Box[n1].x1;
         d := dx*dx + dy*dy;
         if (dMin = 0) or (dMin > d) then begin
            dMin := d;
            Pos[n1].dn_n := n2;
         end;
      end;
   end;
   // set only one symbol being at right/bottom if several ones (keep closer)
   for n1 := 0 to SymbList.nSymb-1 do begin
      n := -1; dMin := 0;
      // closer symbol at left
      for n2 := 0 to SymbList.nSymb-1 do begin
         if n1 = n2 then continue;
         if Pos[n2].ri_n <> n1 then continue;
         // n2 is at the left of n1
         d := SymbList.Box[n1].x1 - SymbList.Box[n2].x1;
         if (n < 0) or (dMin > d) then begin
            n := n2;
            dMin := d;
         end;
      end;
      n2 := n;
      Pos[n1].le_n := n2; // n2 is at the left of n1
      if n2 >= 0 then begin
         for n := 0 to SymbList.nSymb-1 do if Pos[n].ri_n = n1 then Pos[n].ri_n := -1; // clear any other link to n1
         Pos[n2].ri_n := n1; // n2 is at the left of n1
      end;
   end;
   for n1 := 0 to SymbList.nSymb-1 do begin
      n := -1; dMin := 0;
      // closer symbol above
      for n2 := 0 to SymbList.nSymb-1 do begin
         if n1 = n2 then continue;
         if n1 <> Pos[n2].dn_n then continue;
         // n2 is over n1
         d := SymbList.Box[n1].y1 - SymbList.Box[n2].y1;
         if (n < 0) or (dMin > d) then begin
            n := n2;
            dMin := d;
         end;
      end;
      n2 := n;
      Pos[n1].up_n := n2; // n2 is over n1
      if n2 >= 0 then begin
         for n := 0 to SymbList.nSymb-1 do if Pos[n].dn_n = n1 then Pos[n].dn_n := -1; // clear any other link to n1
         Pos[n2].dn_n := n1; // n2 is at the left of n1
      end;
   end;
   // calculate most frequent ch height and ch width (size of char including space behind/at right up to next char)
   FillChar(Count, SizeOf(Count), 0);
   for n1 := 0 to SymbList.nSymb-1 do begin
      n2 := Pos[n1].ri_n;
      if n2 < 0 then continue; // no neighbour
      dx := SymbList.Box[n2].x1 - SymbList.Box[n1].x1; // use lower left corner of symbol
      if (dx > 0) and (dx < 400) then inc(Count[dx]);
   end;
   CalcMax(n, d); // max value is d (appears n times)
   chdx := d;
   FillChar(Count, SizeOf(Count), 0);
   for n1 := 0 to SymbList.nSymb-1 do begin
      n2 := Pos[n1].dn_n;
      if n2 < 0 then continue; // no neighbour
      dy := SymbList.Box[n2].y2 - SymbList.Box[n1].y2; // use lower left corner of symbol
      if (dy > 0) and (dy < 400) then inc(Count[dy]);
   end;
   CalcMax(n, d); // max value is d (appears n times)
   chdy := d;
   // locate upper leftmost symbol (home symbol) of normal size
   dMin := -1;
   nSymbHome := -1;
   for n1 := 0 to SymbList.nSymb-1 do begin
      Pos[n1].cxySet := 0;
      if Pos[n1].NormalSize = 0 then continue;
      x1 := SymbList.Box[n1].x1;
      y1 := SymbList.Box[n1].y1;
      d := x1*x1 + y1*y1;
      if (dMin < 0) or (d < dMin) then begin
         dMin := d;
         nSymbHome := n1;
      end;
   end;
   //  determine cx cy for each symbol
   cx := 0;
   cy := 0;
   nIni := nSymbHome;
   nEnd := 0;
   StepCount := 0;
   StepMode  := 0;                // =1 to activate stepping on gui
   while nIni >= 0 do begin
      // set cx/cy for lines connected at dist 1 or 2 in down/right direction starting from n1 at cx,cy col/row
      if StepMode > 0 then StepMode := 1;
      dMin := 0;
      repeat
         // walk over horiz line, in steps of 1 or 2 chars
         n2 := nIni; // = line start
         // go to left of line in steps of 1 or 2 chars
         repeat
            nx := Pos[n2].le_n;
            if nx < 0 then break;
            dx := SymbList.Box[n2].x1 - SymbList.Box[nx].x1;
            if (dx > chdx * 0.7) and (dx < chdx * 1.3) then begin
               cx := cx - 1;
            end else if (dx > chdx * 1.7) and (dx < chdx * 2.3) then begin
               cx := cx - 2;
            end else begin
               break;
            end;
            n2 := nx;
         until false;
         // assign cx,cy to chars in line going to right, identify the first char down at 1 or 2 char dist
         ny := -1;
         n := 0;
         repeat
            if (Pos[n2].cxySet = 1) then begin
               // symb with coord already set. Keep first ones
               inc(dMin); // count number of already set symbols
            end else begin
               if not FoMain.Progress('Set Symb ' + IntToStr(n2) +
                                      ' Col,Row: ' + IntToStr(cx) + ',' + IntToStr(cy),
                                      StepMode = 0) then exit;
               SymbList.Box[n2].cx := cx;
               SymbList.Box[n2].cy := cy;
               Pos[n2].cxySet := 1;
               // trace cx cy on gui
               if StepMode > 0 then begin
                  FoMain.ImSrc.Picture.Bitmap.Canvas.Brush.Color := clRed;
                  FoMain.ImSrc.Picture.Bitmap.Canvas.TextOut(
                              SymbList.Box[n2].x1, SymbList.Box[n2].y1,
                              IntToStr(cx)+','+IntToStr(cy));
                  inc(StepCount);
                  if StepMode = 1 then begin
                     Log(IntToStr(StepCount) + ': Symb ' + IntToStr(n2) + ' cx ' + IntToStr(cx) + ', cy ' + IntToStr(cy));
                     if FoMain.DoStep(n2,-1) = 'R' then StepMode := 2;
                  end;
               end;
            end;
            // save in ny first symbol of line that has down neighbour
            d := Pos[n2].dn_n;
            if (ny < 0) and (d >= 0) and (Pos[n2].NormalSize = 1) and (Pos[d].NormalSize = 1) then begin
               dy := SymbList.Box[d].y2 - SymbList.Box[n2].y2;
               if (dy > chdy * 0.7) and (dy < chdy * 1.3) then begin
                  ny := n2;
                  n := 1;
               end else if (dy > chdy * 1.7) and (dy < chdy * 2.3) then begin
                  ny := n2;
                  n := 2;
               end;
            end;
            // continue to the right of line
            nx := Pos[n2].ri_n;
            if nx < 0 then break;
            dx := SymbList.Box[nx].x1 - SymbList.Box[n2].x1;
            if (dx > chdx * 0.7) and (dx < chdx * 1.3) then begin
               cx := cx + 1;
            end else if (dx > chdx * 1.7) and (dx < chdx * 2.3) then begin
               cx := cx + 2;
            end else begin
               break;
            end;
            n2 := nx;
         until false;
         // no char down -> exit
         if ny < 0 then break;
         // if too may symbs examined exit (posible cycle?)
         if dMin > SymbList.nSymb then break;
         // localte char on line down
         cx := SymbList.Box[ny].cx;
         cy := SymbList.Box[ny].cy + n;
         nIni := Pos[ny].dn_n; // continue on line down
      until false;
      // try to locate a symbol using already defined coord walking right/down any distance
      nIni := -1;
      dMin := 0;
      for n1 := 0 to SymbList.nSymb-1 do begin
         if Pos[n1].cxySet = 1 then continue;;
         if Pos[n1].NormalSize = 0 then continue;
         // go to left of symbol's line
         n2 := n1;
         repeat
            if (Pos[n2].cxySet = 1) and (Pos[n2].NormalSize = 1) then break;
            nx := Pos[n2].le_n;
            if nx < 0 then break;
            n2 := nx;
         until false;
         // n2 = first symb of line, Now go right looking for a symb with already defined cy
         dy := -1;
         repeat
            if (Pos[n2].cxySet = 1) and (Pos[n2].NormalSize = 1) then begin
               dy := n2;
               break; // cy coord found
            end;
            nx := Pos[n2].ri_n;
            if nx < 0 then break;
            n2 := nx;
         until false;
         // if cy not found, try another symbol
         if dy < 0 then continue;
         // go to top of symbols's column
         n2 := n1;
         repeat
            if (Pos[n2].cxySet = 1) and (Pos[n2].NormalSize = 1) then break;
            ny := Pos[n2].up_n;
            if ny < 0 then break;
            n2 := ny;
         until false;
         // n2 = first symb of columne, Now go down looking fo a symb with already defined cx
         dx := -1;
         repeat
            if (Pos[n2].cxySet = 1) and (Pos[n2].NormalSize = 1) then begin
               dx := n2;
               break; // cx coord found
            end;
            ny := Pos[n2].dn_n;
            if ny < 0 then break;
            n2 := ny;
         until false;
         // if cx not found, try another symbol
         if dx < 0 then continue;
         // calc dist betwwen the
         nx := SymbList.Box[dx].cx - SymbList.Box[dy].cx;
         ny := SymbList.Box[dx].cy - SymbList.Box[dy].cy;
         d := nx*nx + ny*ny;
         if (dMin = 0) or (d < dMin) then begin
            nIni := n1;
            dMin := d;
            cx := SymbList.Box[dx].cx;
            cy := SymbList.Box[dy].cy;
         end;
      end;
      if nIni >= 0 then continue; // explore lines on down direction
      // no more symbols can be located in cx/cy folowing line/col up to a know pos
      // kow try looking at up/dn neighbour
      nIni := -1;
      for n1 := 0 to SymbList.nSymb-1 do begin
         if Pos[n1].cxySet = 1 then continue;
         n2 := Pos[n1].dn_n;
         if (n2 >= 0) and (Pos[n2].cxySet = 1) then begin
            dy := SymbList.Box[n2].y2 - SymbList.Box[n1].y2;
            if (dy > chdy * 0.7) and (dy < chdy * 1.3) then begin
               cx := SymbList.Box[n2].cx;
               cy := SymbList.Box[n2].cy-1;
               nIni := n1;
               break; // cy coord found (from line in down dir)
            end;
         end;
         n2 := Pos[n1].up_n;
         if (n2 >= 0) and (Pos[n2].cxySet = 1) then begin
            dy := SymbList.Box[n1].y1 - SymbList.Box[n2].y1;
            if (dy > chdy * 0.7) and (dy < chdy * 1.3) then begin
               cx := SymbList.Box[n2].cx;
               cy := SymbList.Box[n2].cy+1;
               nIni := n1;
               break; // cy coord found (from line in up dir)
            end;
         end;
      end;
      if nIni >= 0 then continue; // explore lines on down direction
      // no more symbols can be located looking at up/dn neighbour
      // calc cx/cy of nearest defined symbol
      nEnd := -1;
      dMin2 := -1;
      for n1 := 0 to SymbList.nSymb-1 do begin
         if Pos[n1].cxySet = 1 then continue;
         if Pos[n1].NormalSize = 0 then continue;
         dMin := 0; n := -1;
         for n2 := 0 to SymbList.nSymb-1 do begin
            if n1 = n2 then continue;
            if Pos[n2].cxySet = 0 then continue;
            if Pos[n2].NormalSize = 0 then continue; // only uses NormalSize symb as cx/cy reference
            dx := SymbList.Box[n2].x1 - SymbList.Box[n1].x1; // use upper left corner of symbol
            dy := SymbList.Box[n2].y1 - SymbList.Box[n1].y1;
            d := dx*dx + dy*dy;
            if (dMin = 0) or (dMin > d) then begin
               dMin := d;
               n := n2;
            end;
         end;
         if n >= 0 then begin
            dx := round((SymbList.Box[n1].x1 - SymbList.Box[n].x1) / chdx);
            dy := round((SymbList.Box[n1].y2 - SymbList.Box[n].y2) / chdy);
            dMin := dx*dx + dy*dy;
            if (dMin2 < 0) or (dMin2 > dMin) then begin
               dMin2 := dMin;
               x := dx + SymbList.Box[n].cx;
               y := dy + SymbList.Box[n].cy;
               nEnd := n1;
            end;
         end;
      end;
      if dMin2 < 0 then dMin := -1 else dMin := round(sqrt(dMin2)); // set the min distance
      // try lo locate symbol at same cy, and calculated cx
      nIni := -1;
      for n1 := 0 to SymbList.nSymb-1 do begin
         if Pos[n1].cxySet = 1 then continue;;
         if Pos[n1].NormalSize = 0 then continue;
         // go to left of symbol's line
         n2 := n1;
         repeat
            if (Pos[n2].cxySet = 1) and (Pos[n2].NormalSize = 1) then break;
            nx := Pos[n2].le_n;
            if nx < 0 then break;
            n2 := nx;
         until false;
         // n2 = first symb of line, Now go right looking for a symb with already defined cy
         dy := 0;
         repeat
            if (Pos[n2].cxySet = 1) and (Pos[n2].NormalSize = 1) then begin
               cy := SymbList.Box[n2].cy;
               dy := 1;
               break; // cy coord found
            end;
            nx := Pos[n2].ri_n;
            if nx < 0 then break;
            n2 := nx;
         until false;
         // if cy not found, try another symbol
         if dy = 0 then continue;
         // calculate cx based on the located symbol
         dx := SymbList.Box[n1].x1 - SymbList.Box[n2].x1;
         if (nEnd >= 0) and (abs(dx / chdx) > dMin) then continue; // too distant
         cx := round(dx / chdx) + SymbList.Box[n2].cx ;
         nIni := n1;  // pending symbols
         break;
      end;
      if nIni >= 0 then continue; // explore lines on down direction
      // try lo locate symbol at same cx, and calculated cy
      nIni := -1;
      for n1 := 0 to SymbList.nSymb-1 do begin
         if Pos[n1].cxySet = 1 then continue;;
         if Pos[n1].NormalSize = 0 then continue;
         // go to up of symbol's line
         n2 := n1;
         repeat
            if (Pos[n2].cxySet = 1) and (Pos[n2].NormalSize = 1) then break;
            ny := Pos[n2].up_n;
            if ny < 0 then break;
            n2 := ny;
         until false;
         // n2 = first symb of line, Now go down looking for a symb with already defined cx
         dx := 0;
         repeat
            if (Pos[n2].cxySet = 1) and (Pos[n2].NormalSize = 1) then begin
               cx := SymbList.Box[n2].cx;
               dx := 1;
               break; // cy coord found
            end;
            nx := Pos[n2].dn_n;
            if nx < 0 then break;
            n2 := nx;
         until false;
         // if cx not found, try another symbol
         if dx = 0 then continue;
         // calculate cx based on the located symbol
         dy := SymbList.Box[n1].y2 - SymbList.Box[n2].y2;
         if (nEnd >= 0) and (abs(dy / chdy) > dMin) then continue; // too distant
         cy := round(dy / chdy) + SymbList.Box[n2].cy ;
         nIni := n1;  // pending symbols
         break;
      end;
      if nIni >= 0 then continue; // explore lines on down direction
      if nEnd >= 0 then begin
         // use previously calc nearest symb to a known cs,cy
         nIni := nEnd;
         cx := x;
         cy := y;
         continue;
      end;
      // no cx or cy found. So calc cx/cy using nearest defined symbol even if normalsize = 0
      nIni := -1;
      for n1 := 0 to SymbList.nSymb-1 do begin
         if Pos[n1].cxySet = 1 then continue;
         dMin := 0; n := -1;
         for n2 := 0 to SymbList.nSymb-1 do begin
            if n1 = n2 then continue;
            if Pos[n2].cxySet = 0 then continue;
            if Pos[n2].NormalSize = 0 then continue; // only uses NormalSize symb as cx/cy reference
            dx := SymbList.Box[n2].x1 - SymbList.Box[n1].x1; // use upper left corner of symbol
            dy := SymbList.Box[n2].y1 - SymbList.Box[n1].y1;
            d := dx*dx + dy*dy;
            if (dMin = 0) or (dMin > d) then begin
               dMin := d;
               n := n2;
            end;
         end;
         if n >= 0 then begin
            dx := SymbList.Box[n1].x1 - SymbList.Box[n].x1;
            dy := SymbList.Box[n1].y2 - SymbList.Box[n].y2;
            cx := round(dx / chdx) + SymbList.Box[n].cx;
            cy := round(dy / chdy) + SymbList.Box[n].cy;
            nIni := n1;
            break;
         end;
      end;
      if nIni >= 0 then continue; // explore lines on down direction
      // last resource: calc cx/cy from pixel coords
      nIni := -1;
      for n1 := 0 to SymbList.nSymb-1 do begin
         if Pos[n1].cxySet = 1 then continue;;
         nIni := n1;  // pending symbols
         // calc cx and cy based on chdx and chdy
         dx := SymbList.Box[n1].x1 - SymbList.Box[nSymbHome].x1;
         dy := SymbList.Box[n1].y2 - SymbList.Box[nSymbHome].y2;
         cx := round(dx / chdx);
         cy := round(dy / chdy);
         break;
      end;
   end;
   FoMain.Progress('', False);
   // get page rotation from slope
   SymbList.rot := arctan(slope / 1000);
   // normalize cx,cy in range 0,0 -> MaxCols,MaxLins
   NormalizeRowCols(nCol, nRow);
   for n := 0 to SymbList.nSymb-1 do begin
      if SymbList.Box[n].cx > Param.MaxCols then SymbList.Box[n].cx := Param.MaxCols;
      if SymbList.Box[n].cy > Param.MaxLins then SymbList.Box[n].cy := Param.MaxLins;
   end;
   NormalizeRowCols(nCol, nRow);
   // draw on ImScr the symbiol network
   // copy current ImSrc
   wi := FoMain.ImSrc.Picture.Bitmap.Width;
   he := FoMain.ImSrc.Picture.Bitmap.Height;
   bmp := TBitMap.create;
   bmp.PixelFormat := pf8bit;
   bmp.Width := wi;
   bmp.Height := he;
   bmp.Canvas.Brush.Color := clWhite;
   Rect.Left := 0; Rect.Top := 0; Rect.Right := wi; Rect.Bottom := he;
   bmp.Canvas.FillRect(Rect);
   BitBlt(
     bmp.Canvas.Handle, 0,0,wi,he, // destination
     FoMain.ImSrc.Picture.Bitmap.Canvas.Handle, 0,0, SRCCOPY);
   // draw lines on ImScr from each center of contiguos chars
   for n1 := 0 to SymbList.nSymb-1 do begin
     n2 := Pos[n1].dn_n;
     if n2 >= 0 then begin
        // symbol in down direction
         x1 := SymbList.Box[n1].x1 + min(SymbList.Box[n1].wi, wi) div 2;
         y1 := SymbList.Box[n1].y1 + min(SymbList.Box[n1].he, he) div 2;
         x2 := SymbList.Box[n2].x1 + min(SymbList.Box[n2].wi, wi) div 2;
         y2 := SymbList.Box[n2].y1 + min(SymbList.Box[n2].he, he) div 2;
         FoMain.ImSrc.Picture.Bitmap.Canvas.MoveTo(x1,y1);
         FoMain.ImSrc.Picture.Bitmap.Canvas.LineTo(x2,y2);
      end;
      n2 := Pos[n1].ri_n;
      if n2 >= 0 then begin
         // symbol in right direction
         x1 := SymbList.Box[n1].x1 + min(SymbList.Box[n1].wi, wi) div 2;
         y1 := SymbList.Box[n1].y1 + min(SymbList.Box[n1].he, he) div 2;
         x2 := SymbList.Box[n2].x1 + min(SymbList.Box[n2].wi, wi) div 2;
         y2 := SymbList.Box[n2].y1 + min(SymbList.Box[n2].he, he) div 2;
         FoMain.ImSrc.Picture.Bitmap.Canvas.MoveTo(x1,y1);
         FoMain.ImSrc.Picture.Bitmap.Canvas.LineTo(x2,y2);
      end;
   end;
   // convert bitmap to yellow and overlay original bitmap
   for y := 0 to he-1 do begin
      P0 := FoMain.ImSrc.Picture.Bitmap.ScanLine[y];
      P1 := bmp.ScanLine[y];
      for x := 0 to wi-1 do begin
          if P0[x] <> 255 then begin
             P0[x] := 251;               // yellow
          end;
          if (P1[x] < 10) then begin
             P0[x] := 0;                 // black
          end;
      end;
   end;
   bmp.Free;
end;

// calc and adjust col 1 for cx symbols
procedure CalcGridCol1;

   function MatchLine(sMatch, sLin: string): integer;
   var i,j: integer;
       sLiLen, sMaLen: integer;
       cMa, cLi: char;
       bMatch: boolean;
   begin
      sLiLen := length(sLin);
      sMaLen := length(sMatch);
      for i := 1 to sLiLen do begin
         if sLiLen - i - 1 < sMaLen then break; // cannot match, line too short
         bMatch := true;
         for j := 1 to sMaLen do begin
            cMa := sMatch[j];             // char from mathc string
            cLi := sLin[i + j - 1];       // corresponding char from line
            if cMa = ' ' then begin
               if cLi <> ' ' then begin
                  bMatch := false;        // space not matched
                  break;
               end;
            end else if cMa = 'X' then begin
               if cLi = ' ' then begin
                  bMatch := false;        // char not matched
                  break;
               end;
            end;
            if not bMatch then break;
         end;
         if bMatch then begin
            result := i;                  // line matched starting from col i (first col = 1)
            exit;
         end;
      end;
      result := 0; // line not matched
   end;

var i, nLin, cxCol1: integer;
    n, nMin, nCount, nMinTotal, nCountTotal, nIgnore, nIgnoreTotal: integer;
    sMatch, sLin: string;
begin
   nMinTotal := 0;
   nIgnoreTotal := 0;
   nCountTotal := 0;
   for i := 0 to FoMain.TLinPattern.Lines.Count-1  do begin
      sMatch := FoMain.TLinPattern.Lines[i];    // get line pattern to macth
      sMatch := UpStr(sMatch);
      if Trim(sMatch) = '' then continue;
      nMin := 0;
      nIgnore := 0;
      nCount := 0;
      for nLin := 0 to FoMain.TOcrText.Lines.Count-1 do begin // for each line of ocr'ed text
         sLin := FoMain.TOcrText.Lines[nLin];   // get ocr'ed line
         n := MatchLine(sMatch, sLin);   // n = start column where pattern matches line
         if n = 0 then continue;
         if (nCount = 0) or (n < nMin) then begin // get lower start column
            nMin := n;
            nIgnore := Pos('*', sMatch);
            nCount  := 1;
         end else if n = nMin then begin
            inc(nCount);
         end;
      end;
      if nCount > 0 then begin
         if (nCountTotal = 0) or (nMin < nMinTotal) then begin
            nMinTotal := nMin;
            nIgnoreTotal := nIgnore;
            nCountTotal := nCount;
         end else if nMin = nMinTotal then begin
            nCountTotal := nCountTotal + nCount;
         end;
      end;
   end;
   if nCountTotal > Param.StartOfLinePatternMinCount then begin
      // enought matches. Now make col nMinTotal the next col 1
      cxCol1 := nMinTotal - 1;
      for n := 0 to SymbList.nSymb-1 do begin
         SymbList.Box[n].cx := SymbList.Box[n].cx - cxCol1;
      end;
      if nIgnoreTotal > 0 then begin
         for n := 0 to SymbList.nSymb-1 do begin
            if SymbList.Box[n].cx >= nIgnoreTotal-1 then begin
               SymbList.Box[n].OCRState := syIgnore;
            end;
         end;
      end;
   end;
end;

// calc and adjust lin 1 for cy symbols
procedure CalcGridLin1;

   function ValidLine(sLin: string): boolean;
   var i, j, sLiLen: integer;
       bFound: boolean;
   begin
      sLin := Trim(sLin);   // get ocr'ed line
      sLiLen := length(sLin);
      bFound := false;
      for i := 1 to sLiLen do begin
         if sLin[i] = ' ' then continue;
         if sLiLen < i+Param.FirstLineMinCharCount-1 then break;
         bFound := true;
         for j := i to i+Param.FirstLineMinCharCount-1 do begin
            if sLin[j] = ' ' then begin
               bFound := false;
               break;
            end;
         end;
         if bFound then break;
      end;
      result := bFound; // valid line has at least Param.FirstLineMinCharCount consecutive chars
   end;

var nLin1, nLin2, nLin, n, cy: integer;
    sLin: string;
begin
   // look for 3 symb of normal size from top
   nLin1 := -1;
   for nLin := 0 to FoMain.TOcrText.Lines.Count-1 do begin // for each line of ocr'ed text
      sLin := FoMain.TOcrText.Lines[nLin];
      if ValidLine(sLin) then begin
         nLin1 := nLin;
         break;
      end;
   end;
   if nLin1 < 0 then exit; // first line not found
   // look for 3 symb of normal size from bottom of page
   nLin2 := -1;
   for nLin := FoMain.TOcrText.Lines.Count-1 downto 0 do begin // for each line of ocr'ed text
      sLin := FoMain.TOcrText.Lines[nLin];
      if ValidLine(sLin) then begin
         nLin2 := nLin;
         break;
      end;
   end;
   if nLin2 < 0 then exit; // last line not found
   // adjust cy
   for n := 0 to SymbList.nSymb-1 do begin
      cy := SymbList.Box[n].cy;
      if cy > nLin2 then begin
         cy := -1
      end else begin
         cy := cy - nLin1;
         if cy < 0 then begin
            cy := -1;
         end else begin
            cy := cy + Param.PageTopMargin;
         end;
      end;
      SymbList.Box[n].cy := cy;
   end;

end;

procedure ClearSymbColor(n: integer);
begin
   if (n < 0) or (n >= SymbList.nSymb) then exit;
   SymbList.Box[n].Color := 255;
end;

// show symbol's box in source image
procedure ShowSymb;
var i, c: integer;
begin
   for i := 0 to SymbList.nSymb-1 do begin
       case SymbList.Box[i].OCRState of
          syNotIdent:
             c := 251;                         // set box pixel yellow
          syIgnore:
             c := 247;                         //               lt grey
          syUserIdent, syOkIdent:
             c := 250;                         //               green
          syNOk_MultipleMatchs, syNOk_LowQualityMatch, syNOk_NoMatch, syNOk_NoFit:
             c := 249;                         //               red
          else c := 248;                       //               grey
       end;
       if (SymbList.Box[i].Color <> c) then begin
          SymbList.Box[i].Color := c;
          SetSymbBoxColor(i,c); // set box color
       end;
   end;
end;


// calculate most common symbol width and height
procedure CalcSymbWiHe;
var Count: array[0..400] of integer;

   procedure CalcMax(var max, val: integer);
   var i, n: integer;
   begin
       max := 0; val := 0;
       for i := 0 to 400 do begin
           n := Count[i];
           if n > max then begin
              max := n;
              val := i;
           end;
       end;
   end;

   function CalcMaxWiHe(bWi: boolean): integer;
   var n, i: integer;
       Max: array[1..2] of record
                             val, count: integer;
                            end;
       sWi: string;
   begin
      if bWi then begin
         sWi := 'Width';
      end else begin
         sWi := 'Height';
      end;
      FillChar(Count, SizeOf(Count), 0);
      // count occurences of different symbols width
      for i := 0 to SymbList.nSymb-1 do begin
         if bWi then begin
            n := SymbList.Box[i].wi;
         end else begin
            n := SymbList.Box[i].he;
         end;
         if (n < 0) or (n > 400) then continue;
         inc(Count[n]);
      end;
      // get the two most frequent width
      FillChar(Max, SizeOf(Max), 0);
      CalcMax(Max[1].count, Max[1].val);
      Count[Max[1].val] := 0;
      CalcMax(Max[2].count, Max[2].val);
      if abs(Max[1].val - Max[2].val) > Param.MaxDifInSymbBox then begin
         Log('WARNING: Not a clear Symbol '+sWi+': most frequent is ' +
                       'wi='+IntToStr(Max[1].val)+' ('+IntToStr(max[1].count)+' occurences), '+
                       'but second most frequent is ' +
                       'wi='+IntToStr(Max[2].val)+' ('+IntToStr(max[2].count)+' occurences). Using most frequent'
                       );
         result := Max[1].val;
      end else begin
         // the two most frequent width have a difference of 3 pixels or less
         // keep the biggest one
         if Max[1].val > Max[2].val then begin
            result := Max[1].val;
         end else begin
            result := Max[2].val;
         end;
      end;
   end;

begin
   SymbList.wi := CalcMaxWiHe(true);
   SymbList.he := CalcMaxWiHe(false);
end;


procedure StartOCR(bAdjustPage: boolean);
begin
   Log('INFO: Start page processing ... ');
   IsolateSymb;
   CalcSymbWiHe;
   CalcSymbGrid;
   if bAdjustPage then begin
      ShowSymbGrid;  // needed to set TOcrText before CalcGridCol1/Lin1 processing
      CalcGridLin1;
      ShowSymbGrid;
      CalcGridCol1;
   end;
   ShowSymbGrid;  // again to show the result
end;

procedure DoOCR;
begin
   CalcSymbMatch;
   ShowSymbGrid;
end;

end.
