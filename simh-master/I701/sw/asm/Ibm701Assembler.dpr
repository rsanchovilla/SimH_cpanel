program NorcAssembler;
{$APPTYPE CONSOLE}
uses
  SysUtils;

// assembly line format

//          1         2         3         4         5         6
// 123456789012345678901234567890123456789012345678901234567890

// LABEL--- S OPCODE--- OPADDR---    COMMENT

// Label can be up to 8 ascii ident
// number can be NNNN (decimal) or octal *NNNN. No sign allowed

// PSEUDO OPS      SYMBOL EQU  NNNN | SYMBOL
//                 *      return current addr
//                 RES   NNNN   reserve NNNB halfwords
//                 DEF   NNNN   set this half word to given value
//                 ORG   NNNN | EVEN    even to set addr to next even addr
//
//                 HEAD NAA     generates naa.txt with NAA SpeedEx assembler source format
//                 HEAD NNNN    set current symbolic line number.
//                              First ORG EVEN sets the start of DATA region, second is the TEMP region
//
//                 HEAD NR9003  generates nr9003.txt with NR9003 Symbolc Assembly source format
//                 HEAD         add heading card
//                 HEAD N       set symbolic address number to next 1XX00 increment
//
//                 HEAD SO2     generates so2.txt with SOL Regional Assembler
//                 HEAD NNA     generates an origin card for this section
//
const MaxSymb = 300;
var fIn, fOut, fOut2: text;
    bfOut: boolean;
var fname: string;
    Addr: integer = -1;
    nSymb: integer = 0;
    Symb: array[0..MaxSymb] of record
       sName: string;
       Addr: integer;
    end;
    sLin: string;
    nLin: integer;

    bNAA: boolean; // true if generating also source for NAA SpeedEx Assembler
    LocSymb: array[0..4095] of record
        sChar: string; // can be S D or T
        Num: integer; // symbolic addr;
    end;
    LocSymbRec: record
        sChar: char; // S D or T
        Num: integer;
        Origin, Data, Temp: integer;
    end;

    bNR9003: boolean; // true if generating also source for NR9003 Symbolic Assembler
    bSO2: boolean; // true if generating also source for SO2 regional Assembler

    RelRegion: record
       sCode: string;
       Origin: integer;
       Num: integer;
       List: array[0..200] of record
          sCode: string;
          Origin: integer;
          sComment: string; 
       end;
    end;

const
    MaxMne = 33;
    Mne: array[0..MaxMne-1]       of string = ('STOP', 'TR', 'TR OV', 'TR +', 'TR 0',
                                               'SUB', 'R SUB', 'SUB AB', 'NOOP', 'ADD', 'R ADD', 'ADD AB', 'STORE',  'STORE A',
                                               'EXTR', 'STORE MQ', 'LOAD MQ', 'MPY', 'MPY R', 'DIV', 'ROUND',
                                               'L LEFT', 'L RIGHT', 'A LEFT', 'A RIGHT',
                                               'READ', 'READ B', 'WRITE', 'WRITE EF', 'REWIND', 'SET DR', 'SENSE', 'COPY');
    MneOpcode: array[0..MaxMne-1] of integer =( 0, 1, 2, 3, 4,
                                                5, 6, 7, 8, 9, 10, 11, 12, 13,
                                                45, 14, 15, 16, 17, 18, 19,
                                                20, 21, 22, 23,
                                                24, 25, 26, 27, 28, 29, 30, 31);
    NAAMne: array[0..MaxMne-1]     of string = ('ST', 'UT', 'OV', 'PT', 'ZT',
                                               'SU', 'RS', 'SP', 'NI', 'AD', 'RA', 'AP', 'SR',  'SA',
                                               'ET', 'SQ', 'LQ', 'MY', 'MR', 'DV', 'RN',
                                               'LL', 'LR', 'AL', 'AR',
                                               'RD', 'RR', 'WR', 'EF', 'RW', 'DA', 'SS', 'CS');

procedure err(sErr: string);
begin
   writeln('Line ', nLin, ': ', sLin);
   writeln('Error: ', sErr);
   close(fIn);
   if (bfOut) then begin
      close(fOut);
      DeleteFile('prog.txt');
      if bNAA then begin
         close(fOut2);
         DeleteFile('naa.txt');
      end else if bNR9003 then begin
         close(fOut2);
         DeleteFile('nr9003.txt');
      end;
   end;
   Halt(1);
end;

// return value for sName symbol, -1 if not found
function FindSymb(sName: string): integer;
var i: integer;
begin
   for i := 0 to nSymb-1 do begin
      if Symb[i].sName = sName then begin
         result := i;
         exit;
      end;
   end;
   result := -1;
end;

// add symbol to symbol table
procedure AddSymb(sName: string; Addr: integer);
begin
   if (nSymb > MaxSymb) then err('Too many symbols defined');
   if (FindSymb(sName) >= 0) then err('Symbol '+ sName +' already defined');
   Symb[nSymb].sName := sName;
   Symb[nSymb].Addr  := Addr;
   inc(nSymb);
end;

// Retrun Text symbol
function sT(sLin: string; n, l: integer): string;
begin
    sT := Trim(UpperCase(Copy(sLin, n, l)));
end;

// Return number as 18 bit unsigned integer
// format can be NNNN (decimal) or *NNNN (octal)
function sN(s: string): integer;
var n, Code: integer;
    c: char;
begin
   s:=trim(s);
   if Copy(s,1,1)='*' then begin
      delete(s,1,1);
      n := 0;
      while s<>'' do begin
         c := s[1];
         if c <> ' ' then begin
            if (c<'0') or (c>'7') then begin
               result := -1;
               exit;
            end;
            n := n * 8 + ord(c) - ord('0');
         end;
         delete(s,1,1);
      end;
      code := 0;
   end else begin
      Val(s, n, code);
   end;
   if code = 0 then result := n
               else result := -1;
end;

// convert integer to string
// if len < 0, integer as octal
// if len > 0, integer as decimal, pad zero on left to acieve total len chars
// if len = 0, integer as decimal, no zero left padding
function ToS(d, len: integer): string;
var s: string;
    n, octal: integer;
begin
   if (len <0) then begin
      len:=-len; octal:= 1;
   end else octal := 0;
   s := '';
   if (len=0) then begin
      s:=IntToStr(d);
   end else while (len>0) do begin
      if octal > 0 then begin
         n := d mod 8; d := d div 8;
      end else begin
         n := d mod 10; d := d div 10;
      end;
      s := chr(n + ord('0')) + s;
      dec(len);
   end;
   result := s;
end;

// get actual address
var sAddr_IsSymbol: boolean;
function sAddr(s: string): integer;
var n, Code: integer;
begin
   sAddr_IsSymbol := false;
   if (s = '') then err('Addr cannot be blank');
   if s='*' then begin
      result:=Addr;
      exit;
   end;
   if Copy(s,1,1)='*' then begin
      result := sN(s);
      exit;
   end;
   Val(s, n, code);
   if code = 0 then begin
      result := n;  // is a number
      exit;
   end;
   // is a symbol
   n := FindSymb(s);
   if n < 0 then err('Symbol ' + s + ' not defined');
   result := Symb[n].Addr;
   sAddr_IsSymbol := true;
end;

procedure parse;
var sLabel, sSign, sOpCode, sOpAddr, sComment: string;
    nLabel, nOpAddr, m: integer;
begin
    sLabel  := sT(sLin,  1, 8);
    sSign   := Copy(sLin, 10, 1);
    sOpCode := sT(sLin, 12, 9);
    if (sOpCode = '') and (sLabel='') then exit; // blank line
    if (sLabel = 'LABEL---') then exit;
    if (Copy(sLabel,1,1) = ';') then exit; // comment line
    nLabel  := sN(sLabel);
    sOpAddr := sT(sLin, 22, 9);
    nOpAddr := sN(sOpAddr);
    sComment := sT(sLin, 35, 200);

    if sLabel <> '' then begin
       // label present
       if (nLabel >= 0) then begin
          Addr := nLabel; // numeric label sets current addr
       end else if sOpCode = 'EQU' then begin
          // LABEL   EQU   NNNN|SYMBOL   - set label to NNNN
          nOpAddr := sAddr(sOpAddr);
          if (nOpAddr < 1) then err('Value not numeric');
          AddSymb(sLabel, nOpAddr);
       end else begin
          if (Addr < 0) then err('No current addr defined');
          // alpha Label gets PC value
          AddSymb(sLabel, Addr);
       end;
    end;
    if bNAA then begin
       if (Addr >= 0) then begin
          LocSymb[Addr].sChar := LocSymbRec.sChar;
          if (LocSymbRec.sChar = 'S') then begin
             if (LocSymbRec.Origin < 0) then LocSymbRec.Origin := Addr;
             LocSymb[Addr].Num := LocSymbRec.Num;
             LocSymbRec.Num := LocSymbRec.Num + 10;
          end else if (LocSymbRec.sChar = 'D') then begin
             if (LocSymbRec.Data < 0) then LocSymbRec.Data := Addr;
             LocSymb[Addr].Num := Addr - LocSymbRec.Data;
          end else if (LocSymbRec.sChar = 'T') then begin
             if (LocSymbRec.Temp < 0) then LocSymbRec.Temp := Addr;
             LocSymb[Addr].Num := Addr - LocSymbRec.Temp;
          end else err('Invalid NAA address type');
       end;
    end;
    if sOpCode = 'HEAD' then begin
       // for NAA Assembler
       if (sOpAddr = '') and (bNR9003 = false) then err('Missing HEAD specifier');
       if sOpAddr = 'NAA' then begin
          bNAA := true;
          LocSymbRec.sChar := 'S';
          LocSymbRec.Num := 10;
       end else if bNAA then begin
          if (nOpAddr < 0) then err('Non numeric HEAD specifier');
          LocSymbRec.sChar := 'S';
          LocSymbRec.Num := nOpAddr;
       end else if sOpAddr = 'NR9003' then begin
          bNR9003 := true;
          LocSymbRec.Num := 100;
       end else if bNR9003 then begin
          if (sOpAddr = 'N') or (sOpAddr = '') then begin
             LocSymbRec.Num := ((LocSymbRec.Num div 100) + 1) * 100;
             if LocSymbRec.Num > 9999 then err('Exceed 99.99.00 symbolic address');
          end
       end else if sOpAddr = 'SO2' then begin
          bSO2 := true;
          RelRegion.sCode  := '';
          RelRegion.Origin := 0;
          RelRegion.Num := 0;
       end else if bSO2 then begin
          if (sOpAddr = '') then err('Missing HEAD region code (two digits + letter)');
          for m := 0 to 4095 do begin
             if LocSymb[m].sChar = sOpAddr then err('region code '+sOpAddr+' already defined');
          end;
          RelRegion.sCode  := sOpAddr; // region code
          RelRegion.Origin := Addr;    // region origin
          RelRegion.List[RelRegion.Num].sCode  := RelRegion.sCode;
          RelRegion.List[RelRegion.Num].Origin := RelRegion.Origin;
          RelRegion.List[RelRegion.Num].sComment := sComment;
          inc(RelRegion.Num)
       end else begin
          err('Unknown Head spec');
       end;
    end else if sOpCode = 'RES' then begin
       // LABEL   RES   NNNN   - reserve space
       if (Addr < 0) then err('No current addr defined');
       if (nOpAddr < 1) then err('Amount of mem to reserve not numeric');
       if bNR9003 then for m := Addr to Addr + nOpAddr - 1 do begin
          LocSymb[m].Num := LocSymbRec.Num;
          LocSymbRec.Num := LocSymbRec.Num + 1;
       end else if bSO2 then begin
          if RelRegion.sCode = '' then err('Missing Regional code');
          LocSymb[Addr].sChar := RelRegion.sCode;
          LocSymb[Addr].Num   := Addr - RelRegion.Origin;
       end;
       Addr := Addr + nOpAddr;
    end else if sOpCode = 'ORG' then begin
       //         ORG    NNNN   - set origin
       if sOpAddr = 'EVEN' then begin
          if bNAA then begin
              if LocSymbRec.sChar = 'S' then begin
                 LocSymbRec.sChar := 'D';
                 Addr := Addr + 3; // room for NAA last 3 cards: prog origin, half words of data, half words temp
              end else if LocSymbRec.sChar = 'D' then begin
                 LocSymbRec.sChar := 'T';
              end;
          end else if bNR9003 then begin
             err('ORG EVEN not supported by NR9003 Symbolic Assembler');
          end;
          Addr := (Addr + 1) and 4094;
       end else begin
          if (nOpAddr < 1) then err('Amount of mem to reserve not numeric');
          Addr := nOpAddr;
       end;
    end else begin
       if (Addr < 0) then err('No current addr defined');
       if bNR9003 then begin
          LocSymb[Addr].Num := LocSymbRec.Num;
          LocSymbRec.Num := LocSymbRec.Num + 1;
       end else if bSO2 then begin
          if RelRegion.sCode = '' then err('Missing Regional code');
          LocSymb[Addr].sChar := RelRegion.sCode;
          LocSymb[Addr].Num   := Addr - RelRegion.Origin;
       end;
       Addr := Addr + 1;
    end;

end;

function generate: string;
var sLabel, sSign, sOpCode, sOpAddr, sComment: string;
    nOpCode, nOpAddr, m: integer;
    AddrInc: integer;
    sNAA, sNR9003, sSO2, s: string;
begin
    sLabel  := sT(sLin,  1, 8);
    sSign   := sT(sLin, 10, 1);
    sOpCode := sT(sLin, 12, 9);
    sOpAddr := sT(sLin, 22, 9);
    sComment := sT(sLin, 35, 200);

    if (sOpCode = '') and (sLabel='') then begin // comment line
       result := '';
       if bNR9003 then begin
          sNR9003 := '             +010204                        ' + sComment;
          writeln(fOut2, sNR9003);
       end;
       exit;
    end;
    if (sLabel = 'LABEL---') then begin // blank line
       result := '<SKIP>';
       exit;
    end;
    if (Copy(sLabel,1,1) = ';') then begin // comment line
       result := '';
       exit;
    end;

    if sLabel <> '' then begin
       // label present
       if sOpCode = 'EQU' then begin
          // LABEL   EQU   - set label
          result := '';
          exit;
       end;
       Addr := sAddr(sLabel); // numeric label / symbolic label sets current addr
    end;
    AddrInc:=1;

    if sOpCode = 'ORG' then begin
       //         ORG    NNNN   - set origin
       if sOpAddr = 'EVEN' then begin
          Addr := (Addr + 1) and 4094;
       end else begin
          Addr := sN(sOpAddr);
       end;
       result := '';
       if bNR9003 then begin
          sNR9003 := '             +010209         '+ToS(Addr,4)+'           PROGRAM ORIGIN ' + sComment;
          writeln(fOut2, sNR9003);
          sNR9003 := '             +010205               00       PROGRAM START';
          writeln(fOut2, sNR9003);
       end;
       exit;
    end;
    if sOpCode = 'HEAD' then begin
       result := '';
       if bNR9003 then begin
          if sOpAddr = '' then begin
              sNR9003 := '             +010205               00       HEADING ' + sComment;
              writeln(fOut2, sNR9003);
          end;
       end;
       exit;
    end;
    if sOpCode = 'RES' then begin
       // LABEL   RES   NNNN   - reserve space
       AddrInc := sN(sOpAddr);
       nOpCode := 0;
       nOpAddr := 0;
       sSign   := '+';
       if bNR9003 then begin
          for m := Addr to Addr + AddrInc - 1 do begin
              sNR9003 := '             +010201';
              sNR9003 := sNR9003 + ToS(LocSymb[m].Num * 100, 6) + '+00';
              sNR9003 := sNR9003 + '0000  ';
              if m=Addr then  sNR9003 := sNR9003 + '         ' + sComment;
              writeln(fOut2, sNR9003);
          end;
       end;
    end else begin
       // convert opcode mnemonic to instrction code
       nOpCode := -1;
       if sOpCode = '' then begin
          err('Missing Opcode');
       end else if sOpCode = 'DEF' then begin
          nOpAddr := sAddr(sOpAddr);
          nOpCode := nOpAddr shr 12;
          nOpAddr := nOpAddr and $0fff;
          sSign   := '+';
          if bNR9003 then begin
             if nOpCode >= 32 then begin
                 sSign   := '-';
                 nOpCode := nOpCode - 32;
             end;
             sNR9003 := '             +010201';
             sNR9003 := sNR9003 + ToS(LocSymb[Addr].Num * 100, 6) + sSign + ToS(nOpCode, -2);
             sNR9003 := sNR9003 + ToS(nOpAddr, 4) + '  ';
             sNR9003 := sNR9003 + '         ' + sComment;
             writeln(fOut2, sNR9003);
          end else if bSO2 then begin
             if nOpCode >= 32 then begin
                 sSign   := '-';
                 nOpCode := nOpCode - 32;
             end;
             if sSign = '-' then s := '-' else s := ' ';
             sSO2 := '        0' + LocSymb[Addr].sChar + ToS(LocSymb[Addr].Num, 4) + s + ToS(nOpCode, 2);
             sSO2 := sSO2 + '00R' + ToS(nOpAddr, 4);
             sSO2 := sSO2 + '                  ' + sComment;
             writeln(fOut2, sSO2);
          end;
       end else begin
          for m:=0 to MaxMne-1 do begin
             if (sOpCode <> mne[m]) then continue;
             nOpCode := MneOpcode[m];
             break;
          end;
          if (nOpCode < 0) then begin
             nOpCode := sN(sOpCode);
             if nOpCode < 0 then err('Opcode value '+sOpCode+' is invalid');
          end;
          if (sSign <> '-') and (sSign <> '+') then err('Opcode sign should be + or -');
          nOpAddr := sAddr(sOpAddr);

          if bNAA then begin
             sNAA := '         ' + ToS(LocSymb[Addr].Num, 5);
             for m:=0 to MaxMne-1 do begin
                if (nOpCode <> MneOpcode[m]) then continue;
                sNAA := sNAA + NAAMne[m];
                break;
             end;
             if (sOpAddr[1] = '/') then begin
                sNAA := sNAA + 'A';
                if (sSign = '+') then sNAA := sNAA + '1' else sNAA := sNAA + '2';
                sNAA := sNAA + '-----';
             end else if ((sOpAddr[1] >= '0') and (sOpAddr[1] <= '9')) or (sOpAddr[1] = '*') then begin
                sNAA := sNAA + 'A';
                if (sSign = '+') then sNAA := sNAA + '1' else sNAA := sNAA + '2';
                sNAA := sNAA + ToS(nOpAddr, 5);
             end else begin
                sNAA := sNAA + LocSymb[nOpAddr].sChar;
                if (sSign = '+') then sNAA := sNAA + '1' else sNAA := sNAA + '2';
                sNAA := sNAA + ToS(LocSymb[nOpAddr].Num, 5);
             end;
             sNAA := sNAA + '                      ' + sComment;
             writeln(fOut2, sNAA);
          end else if bNR9003 then begin
             if sOpAddr = '*' then sAddr_IsSymbol := true;
             if (sOpAddr[1] = '/') then sAddr_IsSymbol := false;
             sNR9003 := '             +01020';
             if sAddr_IsSymbol then begin
                sNR9003 := sNR9003 + '0';
             end else begin
                sNR9003 := sNR9003 + '1';
             end;
             sNR9003 := sNR9003 + ToS(LocSymb[Addr].Num * 100, 6) + sSign + ToS(nOpCode, -2);
             if sAddr_IsSymbol then begin
                sNR9003 := sNR9003 + ToS(LocSymb[nOpAddr].Num * 100, 6);
             end else begin
                sNR9003 := sNR9003 + ToS(nOpAddr, 4) + '  ';
             end;
             // sNR9003 := sNR9003 + '         ' + sLin;

             s := ToS(LocSymb[Addr].Num div 100, 0) + '.' + ToS(LocSymb[Addr].Num mod 100, 0);
             while length(s) < 6 do s := s + ' ';
             nOpCode := nOpCode and 31;
             if (sSign = '-') and (nOpcode = 13) then nOpCode := 45;
             for m:=0 to MaxMne-1 do begin
                if nOpCode <> MneOpcode[m] then continue;
                s := s + Mne[m];
                break;
             end;
             while length(s) < 6+9 do s := s + ' ';
             if sAddr_IsSymbol then begin
                s := s + ToS(LocSymb[nOpAddr].Num div 100, 0) + '.' + ToS(LocSymb[nOpAddr].Num mod 100, 0);
             end else begin
                s := s + ToS(nOpAddr, 4);
             end;
             while length(s) < 6+9+6 do s := s + ' ';
             sNR9003 := sNR9003 + '         ' + s + sComment;
             writeln(fOut2, sNR9003);
          end else if bSO2 then begin
             if sOpAddr = '*' then sAddr_IsSymbol := true;
             if (sOpAddr[1] = '/') or (sOpAddr[1] = '[') then sAddr_IsSymbol := false;
             if sSign = '-' then s := '-' else s := ' ';
             sSO2 := '        0' + LocSymb[Addr].sChar + ToS(LocSymb[Addr].Num, 4) + s + ToS(nOpCode, 2);
             if sAddr_IsSymbol then begin
                sSO2 := sSO2 + LocSymb[nOpAddr].sChar + ToS(LocSymb[nOpAddr].Num, 4);
             end else begin
                sSO2 := sSO2 + '00R' + ToS(nOpAddr, 4);
             end;
             s := Copy(sComment,1,20);
             while length(s) < 20 do s := s + ' ';
             sSO2 := sSO2 + '                  ' + s + sOpCode;
             writeln(fOut2, sSO2);
          end;
       end;
    end;
    if (Addr < 0) then err('No current addr defined');

    Result := ToS(Addr, -4) + ' ' +  sSign +
              ToS(nOpCode,-2) + ' ' + ToS(nOpAddr,-4) + '              ' + sLin;

    Addr := Addr + AddrInc;

end;

var s, sPad: string;
    bOctalSet: boolean;
    i: integer;
begin
  bfOut:=false;
  sPad := '';
  for nLin:=1 to 27 do sPad := sPad + ' ';
  fname:=ParamStr(1);
  if fname='' then begin
     writeln('Missing source file name');
     halt(1);
  end;
  if not FileExists(fname) then begin
     writeln('Source file name "',fname,'" not found');
     halt(1);
  end;
  bNAA:=false; bNR9003 := false; bSO2 := false;
  DeleteFile('prog.txt');
  DeleteFile('naa.txt');
  DeleteFile('nr9003.txt');
  DeleteFile('so2.txt');

  for i:= 0 to 4095 do begin
      LocSymb[i].sChar := '?';
      LocSymb[i].Num := -1;
  end;
  LocSymbRec.sChar := '?';
  LocSymbRec.Num := -1;
  LocSymbRec.Origin := -1; LocSymbRec.Data := -1; LocSymbRec.Temp := -1;

  assign(fIn, fname);
  reset(fIn);
  nLin:=0; Addr := -1;
  AddSymb('[  ]', 0);
  AddSymb('/   /', 0);
  AddSymb('(   )', 0);
  AddSymb('-----', 0);
  writeln('Pass 1');
  while not eof(fIn) do begin
     inc(nLin);
     readln(fIn, sLin);
     parse;
  end;
  reset(fIn);
  nLin:=0; Addr := -1;
  assign(fOut, 'prog.txt');
  rewrite(fOut);
  bfOut:=true;
  if bNAA then begin
     assign(fOut2, 'naa.txt');
     rewrite(fOut2);
  end else if bNR9003 then begin
     assign(fOut2, 'nr9003.txt');
     rewrite(fOut2);
  end else if bSO2 then begin
     assign(fOut2, 'so2.txt');
     rewrite(fOut2);
  end;
  writeln('Pass 2');
  bOctalSet:= false;
  while not eof(fIn) do begin
     inc(nLin);
     readln(fIn, sLin);
     s := generate;
     if s = '<SKIP>' then continue;
     if s = '' then begin
         s := sPad + sLin;
     end else if not bOctalSet then begin
         bOctalSet := true;
         writeln(fOut, 'OCT');
     end;
     writeln(fOut, s);
  end;
  close(fIn);
  close(fOut);
  if bNAA then begin
     if LocSymbRec.Temp < 0 then LocSymbRec.Temp := Addr;
     if LocSymbRec.Data < 0 then LocSymbRec.Data := Addr;
     i := LocSymbRec.Origin;
     s := '         99997YEA1' + ToS(i, 5) + '                      PROGRAM ORIGIN';
     writeln(fOut2, s);
     i := LocSymbRec.Temp - LocSymbRec.Data;
     s := '         99998YEA1' + ToS(i, 5) + '                      NUMBER HALF WORDS DATA';
     writeln(fOut2, s);
     i := Addr - LocSymbRec.Temp;
     s := '         99999YEA1' + ToS(i, 5) + '                      NUMBER HALF WORDS TEMPORARY';
     writeln(fOut2, s);
     close(fOut2);
  end else if bNR9003 then begin
     s := '             +010205               00       PROGRAM END';
     writeln(fOut2, s);
     close(fOut2);
  end else if bSO2 then begin
     close(fOut2);
     if RelRegion.Num > 0 then begin
        // set origin cards at the beginning of file
        s := '';
        reset(fOut2);
        while not eof(fOut2) do begin
           readln(fOut2, sPad);
           s := s + sPad + #13#10;
        end;
        close(fOut2);
        rewrite(fOut2);
        for i := 0 to RelRegion.Num - 1 do begin
           sPad := '        1' + RelRegion.List[i].sCode + '0000 0000R' + ToS(RelRegion.List[i].Origin, 4);
           sPad := sPad + '                  ' + RelRegion.List[i].sComment;
           writeln(fOut2, sPad);
        end;
        write(fOut2, s);
        close(fOut2);
     end;
  end;
end.
