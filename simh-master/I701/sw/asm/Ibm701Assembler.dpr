program Ibm701Assembler;
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
//                 *            return current addr
//                 RES   NNNN   reserve NNNB halfwords
//                 DEF   NNNN   set this half word to given value
//                 ORG   NNNN | EVEN    even to set addr to next even addr
//                 OUTOCT       set octal as output base for prog.txt (default)
//                 OUTDEC       set decimal as output base for prog.txt
//                 END          end of assembler source
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
//                 HEAD nnn     generates an origin card for this section
//
//                 HEAD PACTREL generates pactrel.txt with PACT Relative Assembler
//                 HEAD TEMP              ends Region I, starts region T (Temporal) at next even addr
//                 HEAD DEF:label         defines a label that can be USEd in another file
//                 HEAD USE:label         uses a label defined in another file
//                 ORG  EVEN              if needed, insert a +000000 instr so next location is even
//                 TXT "aaa" or 'aaa'     add 3-char text in PACT base48 format as a negative halfword
//                 TXT "aaa bb cccc",0    <- terminates the string with +0 half word
//                 symbol V0000 is replaces by "V 0000" referencing variable region
//
const MaxSymb = 900;
var fIn, fOut, fOut2: text;
    bfOut: boolean;
    bOutBaseOctal: boolean;
var fname: string;
    Addr: integer = -1;
    nSymb: integer = 0;
    Symb: array[0..MaxSymb] of record
       sName: string;
       Addr: integer;
       Referenced: boolean;
       bIsAbsContant: boolean;
    end;
    sLin: string;
    nLin: integer;
    bEND: boolean;

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
    bPACT: boolean; // true if generating also source for PACT Relativr assembler

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

    PACTRegion: record
       Symbol: array [0..50] of record
          symb: string; // the label defined or used
          cType: char; // U -> USEd symbol, D -> DEFined symbol
          Num: integer;
       end;
       I, T, S: integer;
    end;
    LocPACTRel: array [0..4095] of record
       Reg: char;
       Num: integer;
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
    PACTMne: array[0..MaxMne-1]    of string = ('H ', 'T ', 'TF', 'TP', 'TZ',
                                                'S ', 'RS', 'SV', 'N ', 'A ', 'RA', 'AV', 'ST',  'SA',
                                                'EX', 'SM', 'LM', 'M ', 'MR', 'D ', 'R ',
                                                'LL', 'LR', 'AL', 'AR',
                                                'RD', 'RB', 'W ', 'WE', 'RW', 'SD', 'SE', 'C ');
    Space9 = '         '; // 9 spaces

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
      end else if bSO2 then begin
         close(fOut2);
         DeleteFile('so2.txt');
      end else if bPACT then begin
         close(fOut2);                                         
         DeleteFile('pactrel.txt');
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
procedure AddSymb(sName: string; Addr: integer; bIsAbsContant: boolean);
begin
   if (nSymb > MaxSymb) then err('Too many symbols defined');
   if (FindSymb(sName) >= 0) then err('Symbol '+ sName +' already defined');
   Symb[nSymb].sName := sName;
   Symb[nSymb].Addr  := Addr;
   Symb[nSymb].Referenced := False;
   Symb[nSymb].bIsAbsContant := bIsAbsContant;
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
// if len > 0, integer as decimal, pad zero on left to achieve total len chars
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
var sAddr_IsSymbolN: integer;
function sAddr(s: string): integer;
var n, Code: integer;
begin
   sAddr_IsSymbol := false;
   sAddr_IsSymbolN := -1;
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
   sAddr_IsSymbolN := n; // the symbol found
end;

procedure GetBase48Txt(s: string; var sOut: string; var nHalfWords: integer);
// input s: "aaa"[,0] -> outs: 'aaa   ' -> will use nHalfWords
var m, bFound: integer;  
begin
   while length(s)<2 do s:=s+' ';
   if (s[1] <> '"') and (s[1] <> '''') then err('Missing " or '' ');
   bFound := 0;
   for m := 2 to length(s) do begin // found terminating delimiter
       if (s[m] = s[1]) then begin
          bFound:=m;
          break;
       end;
   end;
   if bFound = 0 then err('Missing string termination');
   if Copy(s, bFound+1, 2)=',0' then begin
      // string zero terminated
      s := Copy(s, 2, bFound-2);
      bFound := 1;
   end else begin
      s := Copy(s, 2, bFound-2);
      bFound := 0; // not zero-terminated
   end;
   if s='' then err('Empty TXT string');
   // 3 chars can be packed in each half word
   nHalfWords := (length(s) + 2) div 3;
   if bFound > 0 then nHalfWords := nHalfWords+1; 
   sOut := s + '      ';
end;

function PackBase48(s: string): integer;
// return base48 3-char packed string
const Base48 = ' 1234567890-+ABCDEFGHIJKLMNOPQRSTUVWXYZ,%$*.#/+-';
var alfa, n, c: integer;
begin
   alfa := 0;
   for n := 1 to 3 do begin
      c := Pos(s[n], Base48);
      if c = 0 then c := 47 else c := c-1;
      alfa := alfa * 48 + c;
   end;
   result := alfa;
end;

procedure parse;
var sLabel, sSign, sOpCode, sOpAddr, sComment, s: string;
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
          if (nOpAddr < 0) then err('Value not numeric');
          if sAddr_IsSymbolN > 0 then begin
             Symb[sAddr_IsSymbolN].Referenced := true;
             AddSymb(sLabel, nOpAddr, false);
          end else begin
             AddSymb(sLabel, nOpAddr, true); // is an absolute constant
          end;
          exit;
       end else begin
          if (Addr < 0) then err('No current addr defined');
          // alpha Label gets PC value
          AddSymb(sLabel, Addr, false);
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
       if (sOpAddr = '') and (bNR9003 = false) then err('Missing HEAD specifier');
       if sOpAddr = 'NAA' then begin
          // for NAA Assembler
          bNAA := true;
          LocSymbRec.sChar := 'S';
          LocSymbRec.Num := 10;
       end else if bNAA then begin
          if (nOpAddr < 0) then err('Non numeric HEAD specifier');
          LocSymbRec.sChar := 'S';
          LocSymbRec.Num := nOpAddr;
       end else if sOpAddr = 'NR9003' then begin
          // for NR9003 Assembler
          bNR9003 := true;
          LocSymbRec.Num := 100;
       end else if bNR9003 then begin
          if (sOpAddr = 'N') or (sOpAddr = '') then begin
             LocSymbRec.Num := ((LocSymbRec.Num div 100) + 1) * 100;
             if LocSymbRec.Num > 9999 then err('Exceed 99.99.00 symbolic address');
          end
       end else if sOpAddr = 'SO2' then begin
          // for SO2 Assembler
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
       end else if sOpAddr = 'PACTREL' then begin
          // for PACT Relative Assembler
          bPACT := true;
          PACTRegion.S := 0;
          PACTRegion.I := 0;
          PACTRegion.T := 0;
          if (Addr < 0) then Addr := 0;
          for m := 0 to 4095 do begin // init equivalent in PACT rel of each location
             LocPACTRel[m].Reg := 'I'; LocPACTRel[m].Num := m;
          end;
       end else if bPACT then begin
          if sOpAddr = 'TEMP' then begin
             if PACTRegion.T < 1 then begin
                PACTRegion.T := 0;
                PACTRegion.I := Addr; // length of I region
             end else begin
                err('Duplicated HEAD TEMP');
             end;
             Addr := (Addr + 1) and 4094; // make addr even
             for m := Addr to 4095 do begin // init equivalent in PACT rel of each location
                LocPACTRel[m].Reg := 'T'; LocPACTRel[m].Num := m-Addr;
             end;
             PACTRegion.T := Addr; // start of T region
          end else if (Copy(sOpAddr,1,4)='DEF:') then begin
             // define a symbol (max 8 chars)
             sOpAddr := trim(sT(sLin, 22+4, 8));
             if PACTRegion.S > 0 then
                for m:=0 to PACTRegion.S-1 do begin
                  if PACTRegion.Symbol[m].Symb = sOpAddr then
                     err('Symbol '+sOpAddr+' already USEd or DEFined');
                end;
             m := PACTRegion.S; inc(PACTRegion.S); if m >= 50 then err('Too many symbols USEd or DEFined');
             PACTRegion.Symbol[m].Symb  := sOpAddr; // The assembler label name
             PACTRegion.Symbol[m].cType := 'D'; // DEFined symbol
             PACTRegion.Symbol[m].Num   := 0; // pending to be resolved
             // do NOT reset PACTRegion.I. All symbols are into the same region. so the can TR
             // or access all routines data
          end else if (Copy(sOpAddr,1,4)='USE:') then begin
             // use a PACT routine -> define a symbol to be called
             sOpAddr := trim(sT(sLin, 22+4, 8));
             if PACTRegion.S > 0 then
                for m:=0 to PACTRegion.S-1 do begin
                  if PACTRegion.Symbol[m].Symb = sOpAddr then
                     err('Symbol '+sOpAddr+' already USEd or DEFined');
                end;
             m := PACTRegion.S; inc(PACTRegion.S); if m >= 50 then err('Too many symbols');
             PACTRegion.Symbol[m].Symb  := sOpAddr; // The assembler label name
             PACTRegion.Symbol[m].cType := 'U'; // USEd symbol
             PACTRegion.Symbol[m].Num  := m; // symbol sequence number, pending to be linked. Sequence starts in 1
             // create the symbol
             AddSymb(PACTRegion.Symbol[m].Symb, 4095, true);
          end else err('Invalid HEAD');
       end else begin
          err('Unknown HEAD spec');
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
    end else if (sOpCode = 'TXT') and (bPACT) then begin
       GetBase48Txt(Trim(sT(sLin, 22, 80)), s, nOpAddr);
       Addr := Addr + nOpAddr;
    end else if (sOpCode = 'OUTOCT') or
                (sOpCode = 'OUTDEC')  then begin
       // will be handled in generate procedure
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
          if (nOpAddr < 0) then err('Amount of mem to reserve not numeric');
          Addr := nOpAddr;
       end;
    end else if sOpCode = 'END' then begin
       // terminate input file
       bEND := true;
       exit;
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
    if Addr > 4095 then err('program too big');
end;

function generate: string;
var sLabel, sSign, sOpCode, sOpAddr, sComment: string;
    nOpCode, nOpAddr, m, bFound: integer;
    AddrInc: integer;
    sNAA, sNR9003, sSO2, sPACT, s: string;
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

    if sOpCode = 'END' then begin
       // terminate input file
       bEND := true;
       exit;
    end else if sOpCode = 'OUTOCT' then begin
       bOutBaseOctal := true; // set the addr and instr base in generated prog.txt file
       exit;
    end else if sOpCode = 'OUTDEC' then begin
       bOutBaseOctal := false; // set the addr and instr base in generated prog.txt file
       exit; 
    end;
    if sOpCode = 'ORG' then begin
       //         ORG    NNNN   - set origin
       if sOpAddr = 'EVEN' then begin
          if bPACT and ((Addr and 1) <> 0) then begin
             // PACTREL format:
             // 123456789012345678901234567890             x
             // 1234abcd123456789012345678901234567890123456789012345678901234567890123456789012
             //          nnnn +nnnnnn    comment
             //          nnnn -op r nnnn comment
             sPACT := Space9 + ToS(LocPACTRel[Addr].Num, 4) + ' +000000';
             while length(sPACT) < 25 do sPACT := sPACT + ' ';
             sPACT := sPACT + sLin;
             writeln(fOut2, sPACT);
             // insert a zero halfword to make next location even
          end;
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
       end else if bPact and (sOpAddr = 'TEMP') then begin
          Addr := (Addr + 1) and 4094;
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
       end else if bPACT then begin
          if LocPACTRel[Addr].Reg <> 'T' then begin
             // on TEMP region, RES declaration does not appears on listing
             // PACTREL format:
             // 123456789012345678901234567890             x
             // 1234abcd123456789012345678901234567890123456789012345678901234567890123456789012
             //          nnnn +nnnnnn    comment
             //          nnnn -op r nnnn comment
             sPACT := Space9 + ToS(LocPACTRel[Addr].Num, 4) + ' +000000';
             while length(sPACT) < 25 do sPACT := sPACT + ' ';
             sPACT := sPACT + sLin;
             writeln(fOut2, sPACT);
          end;
       end;
    end else begin
       // convert opcode mnemonic to instrction code
       nOpCode := -1;
       if sOpCode = '' then begin
          err('Missing Opcode');
       end else if (sOpCode = 'TXT') and (bPACT) then begin
          // BASE48 text literals for PACT
          // TXT "aaa" or 'aaa'. TXT "aaa",0 <- terminates the string with +0 half word
          // calculate size
          GetBase48Txt(Trim(sT(sLin, 22, 80)), s, AddrInc);
          for m := Addr to Addr + AddrInc - 1 do begin
             if LocPACTRel[m].Reg = 'T' then err('TXT not allowed in TEMP region');
             nOpCode := PackBase48(s);
             if (nOpCode = 0) and (m=Addr + AddrInc - 1) then sSign := '+' else sSign := '-';
             // PACTREL format:
             // 123456789012345678901234567890             x
             // 1234abcd123456789012345678901234567890123456789012345678901234567890123456789012
             //          nnnn +nnnnnn    comment
             //          nnnn -op r nnnn comment
             sPACT := Space9 + ToS(LocPACTRel[m].Num, 4) + ' ' + sSign + ToS(nOpCode, 6);
             Delete(s,1,3);
             while length(sPACT)<25 do sPACT := sPACT + ' ';
             if m = Addr then sPACT := sPACT + sLin;
             writeln(fOut2, sPACT);
          end;
          nOpCode := 0;
       end else if sOpCode = 'DEF' then begin
          nOpAddr := abs(sAddr(sOpAddr));
          if sAddr_IsSymbolN >  0 then Symb[sAddr_IsSymbolN].Referenced := true;
          if sOpAddr[1]='-' then begin
             sSign   := '-';
          end else sSign := '+';
          nOpCode := nOpAddr shr 12;
          nOpAddr := nOpAddr and $0fff;
          if nOpCode >= 32 then begin
              sSign   := '-';
              nOpCode := nOpCode - 32;
              if nOpCode >= 32 then err('Number too big');
          end;

          if bNR9003 then begin
             sNR9003 := '             +010201';
             sNR9003 := sNR9003 + ToS(LocSymb[Addr].Num * 100, 6) + sSign + ToS(nOpCode, -2);
             sNR9003 := sNR9003 + ToS(nOpAddr, 4) + '  ';
             sNR9003 := sNR9003 + '         ' + sComment;
             writeln(fOut2, sNR9003);
          end else if bSO2 then begin
             if sSign = '-' then s := '-' else s := ' ';
             sSO2 := '        0' + LocSymb[Addr].sChar + ToS(LocSymb[Addr].Num, 4) + s + ToS(nOpCode, 2);
             sSO2 := sSO2 + '00R' + ToS(nOpAddr, 4);
             sSO2 := sSO2 + '                  ' + sComment;
             writeln(fOut2, sSO2);
          end else if bPACT then begin
             if LocPACTRel[Addr].Reg = 'T' then err('DEF not allowed in TEMP region');
             // PACTREL format:
             // 123456789012345678901234567890             x
             // 1234abcd123456789012345678901234567890123456789012345678901234567890123456789012
             //          nnnn +nnnnnn    comment
             //          nnnn -op r nnnn comment
             if sSign <> '-' then sSign := '+';
             sPACT := Space9 + ToS(LocPACTRel[Addr].Num, 4) + ' ' +sSign + ToS(nOpCode*4096+nOpAddr, 6);
             while length(sPACT) < 25 do sPACT := sPACT + ' ';
             sPACT := sPACT + sLin;
             writeln(fOut2, sPACT);
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
          if sAddr_IsSymbolN > 0 then Symb[sAddr_IsSymbolN].Referenced := true;
          if nOpAddr > 4095 then begin
             err('OpAddr cannot be >4095');
          end;

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
          end else if bPACT then begin
             if LocPACTRel[m].Reg = 'T' then err('intruction code not allowed in TEMP region');
             // PACTREL format:
             // 123456789012345678901234567890             x
             // 1234abcd123456789012345678901234567890123456789012345678901234567890123456789012
             //          nnnn +nnnnnn    comment
             //          nnnn -op r nnnn comment
             sPACT := Space9 + ToS(LocPACTRel[Addr].Num, 4) + ' ' +sSign;
             for m:=0 to MaxMne-1 do begin
                if (nOpCode <> MneOpcode[m]) then continue;
                sPACT := sPACT + PACTMne[m] + ' ';
                break;
             end;
             if sAddr_IsSymbol then begin
                if sOpAddr = 'V0000' then begin
                   // special var V0000 references variable region
                   sPACT := sPACT + 'V 0000';
                end else begin
                   sAddr_IsSymbol := false;
                   // if referencing an used symbol, replace by its reference
                   for m := 0 to PACTRegion.S-1 do begin
                      if (PACTRegion.Symbol[m].Symb = sOpAddr) and
                         (PACTRegion.Symbol[m].cType = 'U') then begin
                         sAddr_IsSymbol := true;
                         sPACT := sPACT + 'S ' + ToS(PACTRegion.Symbol[m].Num,4);
                         if nOpCode <> 1 then err('Only can reference USEd symbols on TR instr');
                         break;
                      end;
                   end;
                   if sAddr_IsSymbol = false then begin
                      // not an unused symbol, check if is a DEFined constant
                      if Symb[sAddr_IsSymbolN].bIsAbsContant then begin
                         sPACT := sPACT + 'A ' + ToS(nOpAddr,4);
                      end else begin
                         // no, is a regular addr. Use its region
                         sPACT := sPACT + LocPACTRel[nOpAddr].Reg + ' ' + ToS(LocPACTRel[nOpAddr].Num,4);
                      end;
                   end;
                end;
             end else begin
                // not symbol
                if (sOpAddr= '*') then begin
                   sPACT := sPACT + 'I ' + ToS(nOpAddr, 4);
                end else begin
                   sPACT := sPACT + 'A ' + ToS(nOpAddr, 4);
                end;
             end;
             while length(sPACT) < 25 do sPACT := sPACT + ' ';
             sPACT := sPACT + sLin;
             writeln(fOut2, sPACT);
          end;
       end;
    end;
    if (Addr < 0) then err('No current addr defined');

    if (nOpCode >= 32) then begin
       nOpCode := nOpCode-32;
       sSign := '-';
    end;

    if bOutBaseOctal then begin
       // octal -> use normal format
       // 01234567890123456789012345678901234567890
       // NNNN S OPNAME   OP ADDR    Remarks...(max 79 chars)
       Result := ToS(Addr, -4) + ' ' +  sSign +
                 ToS(nOpCode,-2) + ' ' + ToS(nOpAddr,-4) + '              ' + sLin;
    end else begin
       // decimal -> use compact format
       // 01234567890123456789012345678901234567890
       //         NNNN OP ADDR COMMENTS
       Result := '        ' + ToS(Addr, 4) + sSign +
                 ToS(nOpCode,2) + ' ' + ToS(nOpAddr,4) + ' ' + sLin;
    end;
    Addr := Addr + AddrInc;

end;

var s, sPad: string;
    bBaseSet: boolean;
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
  bNAA:=false; bNR9003 := false; bSO2 := false; bPACT := false;
  bEND := false; bOutBaseOctal := true;
  DeleteFile('prog.txt');
  DeleteFile('naa.txt');
  DeleteFile('nr9003.txt');
  DeleteFile('so2.txt');
  DeleteFile('pactrel.txt');

  for i:= 0 to 4095 do begin
      LocSymb[i].sChar := '?';
      LocSymb[i].Num := -1;
      LocPACTRel[i].Reg := '?';
      LocPACTRel[i].Num := -1;
  end;
  LocSymbRec.sChar := '?';
  LocSymbRec.Num := -1;
  LocSymbRec.Origin := -1; LocSymbRec.Data := -1; LocSymbRec.Temp := -1;

  assign(fIn, fname);
  reset(fIn);
  nLin:=0; Addr := -1;
  AddSymb('[  ]', 0, true);
  AddSymb('/   /', 0, true);
  AddSymb('(   )', 0, true);
  AddSymb('-----', 0, true);
  writeln('Pass 1');
  while not eof(fIn) do begin
     inc(nLin);
     readln(fIn, sLin);
     parse;
     if bEND then break;
  end;

  reset(fIn);
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
  end else if bPACT then begin
     assign(fOut2, 'pactrel.txt');
     rewrite(fOut2);
     writeln(fOut2, '; SYMBOL TABLE');
     writeln(fOut2, '; Reg Addr   Len');
     if PACTRegion.I = 0 then PACTRegion.I := Addr; // no NUM region -> I region extends up to the end of prog
     writeln(fOut2, ';  I     0  ',PACTRegion.I:4);
     i := 0; if PACTRegion.T > 0 then i := Addr - PACTRegion.T; // PACTRegion.T contains the T region start -> i=region length
     writeln(fOut2, ';  T  ',PACTRegion.T:4, '  ', i:4);
     writeln(fOut2, ';    Num  Op.  Addr  Name');
     for i := 0 to PACTRegion.S-1 do begin
        write(fOut2, '; SY  ',i:2,'  ');
        if (PACTRegion.Symbol[i].cType = 'D') then begin
           // DEFined symbol in this assembler code. resolve its address
           PACTRegion.Symbol[i].Num := sAddr( PACTRegion.Symbol[i].Symb );
           if sAddr_IsSymbolN > 0 then Symb[sAddr_IsSymbolN].Referenced := true;
           // to signal that symbol is resolved
           write(fOut2, 'DEF  ');
           writeln(fOut2, PACTRegion.Symbol[i].Num:4,'  ', PACTRegion.Symbol[i].Symb);
           // print on screen defined symbols
           writeln('          DEF    ',PACTRegion.Symbol[i].Symb);
        end else begin
           // USEd symbol
           // to signal that symbol is not resolved
           write(fOut2, 'USE  ');
           writeln(fOut2, '    ','  ', PACTRegion.Symbol[i].Symb);
        end;
     end;
     writeln(fOut2, ';  ');
  end;
  writeln('Pass 2');
  nLin:=0; Addr := -1;
  bBaseSet:= false; bEND := false;
  while not eof(fIn) do begin
     inc(nLin);
     readln(fIn, sLin);
     s := generate;
     if bEND then break;
     if s = '<SKIP>' then continue;
     if s = '' then begin
         s := sPad + sLin;
     end else if not bBaseSet then begin
         bBaseSet := true;
         if bOutBaseOctal then begin
            writeln(fOut, 'OCT');
         end else begin
            // for decimal output use compact mode
            writeln(fOut, '        DEC');
            writeln(fOut, '        NNNN OP ADDR COMMENTS')
         end;
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
  end else if bPACT then begin
     close(fOut2);
  end;
  for i := 4 to nSymb-1 do begin
      if Symb[i].Referenced = False then begin
         writeln('Symbol ' + Symb[i].sName + ' not referenced');
      end;
   end;

end.
