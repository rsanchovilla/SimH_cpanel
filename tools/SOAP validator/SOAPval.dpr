
program SOAPval;
{$APPTYPE CONSOLE}
uses SysUtils;

(*

column   1         2         3         4         5         6         7         8
12345678901234567890123456789012345678901234567890123456789012345678901234567890
 102   0    SUBR3    STD  EXITX  DOWNR   STOR EXIT       0650    24  0653  0656
       ^    ^        ^    ^      ^       ^               ^       ^
       |    |        |    |      |       |               |       |
       |    |        |    |      |       |               |  col 66: NN NNNN NNNN word to store in location
       |    |        |    |      |       |               +- col 58: NNNN location to store word
       |    |        |    |      |       +- col 42: Comment
       |    |        |    |      +- col 34: Instruction Address (IA)
       |    |        |    +- col 27: Data Address (DA)
       |    |        +- col 22: Opcode
       |    +- col 13: Instruction location address
       +- col 8: Card type: 1=comment
*)

procedure Halt(n: integer);
begin
   write('Press a Enter to continue ... ');
   readln;
//   System.Halt(n);
end;

var soap: array[0..4000] of record
       sLin,
       AddrStr, OpStr, DaStr, IaStr: string;
       AddrNum, OpNum, DaNum, IaNum: integer;
                       DaTag, IaTag: char;
       Addr,    Op,    Da,    Ia: integer;
       neg: integer;
    end;
    nSoap: integer;
    sHED: string;
    symb: array[0..9000] of record
       Name: string;
       Addr: integer;
       bCanHaveSameAddr: boolean;
    end;
    nSymb: integer;
    bGenerateSourceCode: boolean;

   procedure Err(s: string);
   begin
      writeln('   --> ' + s);
      Halt(1);
   end;

   procedure parse_label(s: string; sType: string; nCol, nLen: integer; var sLabel: string; var Num: integer);
   var i,AllowTag: integer;
   begin
      Num := 0;
      sLabel := '';
      if nCol < 0 then begin
         sLabel := copy(s,-nCol,nLen);
         exit;
      end;
      AllowTag:=0;
      if nLen = -5 then begin
         nLen:=5; // if len = -5 then allos 5 chars label to be followed by a A,B or C (index tag)
         AllowTag:=1;
      end;
      if (s[nCol-1] <> ' ') and (s[nCol-1] <> '-') then begin
         err(sType + ' label starting before start column');
      end;
      if (s[nCol+nLen] <> ' ') then begin
         if (AllowTag=1) and (s[nCol+nLen]>='A') and (s[nCol+nLen]<='D') then begin
            // label followed by A,B or C index tag. Suport also supersoap tag D (drum tag)
         end else begin
            err(sType + ' label too long');
         end;
      end;
      if copy(s,nCol,nLen) = copy('          ',1,nLen) then begin
         exit;
      end;
      if (s[nCol] = ' ') and (s[nCol+1] <> ' ') then begin
         if (s[nCol+1] < '0') or (s[nCol+1] > '9') then begin
            err(sType + ' label starting after start column');
         end;
         while s[nCol+nLen-1] = ' ' do dec(nLen);
         for i:=2 to nLen-1 do if (s[nCol+i] < '0') or (s[nCol+i] > '9') then begin
            err(sType + ' label numeric contains non 0-9 chars');
            break;
         end;
         Num := 0;
         for i:=1 to nLen-1 do Num := Num * 10 + ord(s[nCol+i]) - ord('0');
         sLabel := '<NUM>';
         exit;
      end;
      if s[nCol] = ' ' then begin
         err(sType + ' label starting after start column');
      end;
      sLabel := trim(copy(s,nCol,nLen));
   end;

   procedure parse_num(s: string; sType: string; nCol, nLen: integer; var Num: integer);
   var i: integer;
   begin
      Num := 0;
      if nCol < 0 then begin
         nCol := -nCol;
      end else if s[nCol-1] <> ' ' then begin
         err(sType + ' num starting before start column');
      end;
      if s[nCol+nLen] <> ' ' then begin
         err(sType + ' num too long');
      end;
      for i:=0 to nLen-1 do if (s[nCol+i] < '0') or (s[nCol+i] > '9') then begin
         err(sType + ' num contains non 0-9 chars ("' + s[nCol+i] + '")');
         break;
      end;
      Num := 0;
      for i:=0 to nLen-1 do Num := Num * 10 + ord(s[nCol+i]) - ord('0');
   end;

  procedure AddTo_Symbol_Table(sName: string; nAddr: integer; bCanHaveSameAddr: boolean);
  var i: integer;
  begin
      if (length(sName) < 5) and (trim(sHED) <> '') then begin
         while length(sName) < 4 do sName := sName + ' ';
         sName := sName + sHED;
      end;
      for i := 0 to nSymb-1 do
         if (symb[i].Name = sName) and
            (symb[i].Addr <> nAddr) then begin
            err('Symbol ' + sName + ' already defined');
            break;
         end;
      if bCanHaveSameAddr = false then
      for i := 0 to nSymb-1 do
         if (symb[i].Name <> sName) and
            (symb[i].Addr = nAddr) and
            (symb[i].bCanHaveSameAddr = false) then begin
            err('Symbol ' + sName + ' defined at same address as symbol ' + symb[i].Name);
            break;
         end;
      symb[nSymb].Name := sName;
      symb[nSymb].Addr := nAddr;
      symb[nSymb].bCanHaveSameAddr := bCanHaveSameAddr;
//      writeln('Add ' + sName + ' at ' + IntToStr(nSymb));
      inc(nSymb);
  end;

   function Search_Symbol_Table(sLabel: string): integer;
   var i: integer;
   begin
      Result := -1;
      if (sLabel = '<NUM>') or (sLabel = '') then exit;
      if (length(sLabel) < 5) and (trim(sHED) <> '') then begin
         while length(sLabel) < 4 do sLabel := sLabel + ' ';
         sLabel := sLabel + sHED;
      end;
      for i := 0 to nSymb-1 do begin
         if symb[i].Name <> sLabel then continue;
         Result := i;
         exit;
      end;
   end;

   procedure DeleteFrom_Symbol_Table(n: integer);
   var i: integer;
   begin
      if n < nSymb-1 then begin
         nSymb := nSymb - 1;
         for i := n to nSymb-1 do begin
            symb[i].Name := symb[i+1].Name;
            symb[i].Addr := symb[i+1].Addr;
            symb[i].bCanHaveSameAddr := symb[i+1].bCanHaveSameAddr;
         end;
      end;
   end;

   procedure AddRegionTo_Symbol_Table(sLabel: string; n1, n2: integer);
   var i, n: integer;
       sAddr: string;
   begin
      for n := 0000 to 2000 do begin
         sAddr := IntToStr(n);
         while length(sAddr) < 4 do sAddr := '0' + sAddr;
         i := Search_Symbol_Table(slabel + sAddr);
         if i >= 0 then begin
             DeleteFrom_Symbol_Table(i);
         end;
      end;
      for n := n1-1 to n2 do begin
         sAddr := IntToStr(n - (n1-1));
         while length(sAddr) < 4 do sAddr := '0' + sAddr;
         AddTo_Symbol_Table(sLabel + sAddr, n, true);
      end;
   end;

  procedure validate_str(sTxt: string; n1, n2, n3: integer);
  var n0: int64;
      sMemToAscii: string;
      i,j: integer;
  begin
     sMemToAscii :=     ' ~~~~~~~~~' + '~~~~~~~~.)' + '+~~~~~~~$*' +
                        '-/~~~~~~,(' + '~~~~~~~~=-' + '~~~~~~~~~~' +
                        '~ABCDEFGHI' + '~JKLMNOPQR' + '~~STUVWXYZ' +
                        '0123456789';
    sTxt := sTxt + '     ';
    n0 := n1 * int64(100000000) + n2 * int64(10000) + n3;
    for i := 1 to 5 do begin
       n1 := n0 div 100000000;
       n0 := (n0 mod 100000000) * 100;
       n2 := -1;
       for j := 1 to length(sMemToAscii) do begin
          if sMemToAscii[j] <> sTxt[i] then continue;
          n2 := j-1;
          break;
       end;
       if n2 < 0 then err('char ' + sTxt[i] + ' is not part of ibm 650 character set');
       if n1 <> n2 then err('char ' + sTxt[i] + ' assembled as ' + IntToStr(n1) +
                            ' but should be assembled to ' + IntToStr(n2));
    end;

  end;

  procedure validate_opcode(sOp: string; nOp: integer);
  var sOpcodes, sOp2: string;
      nOp2: integer;
  begin
     sOpcodes := 'TLE'#63'BD0'#90 +
                 'ALO'#15'AML'#17'AUP'#10'AXA'#50'AXB'#52'AXC'#58'BDO'#90'BD1'#91'BD2'#92 +
                 'BD3'#93'BD4'#94'BD5'#95'BD6'#96'BD7'#97'BD8'#98'BD9'#99'BIN'#26 +
                 'BMA'#41'BMB'#43'BMC'#49'BMI'#46'BOV'#47'BST'#57'DIV'#14'DVR'#64 +
                 'FAD'#32'FAM'#37'FDV'#34'FMP'#39'FSB'#33'FSM'#38'HLT'#01'LDD'#69'LDI'#09'LOD'#69 +
                 'LIB'#08'MPY'#19'NEF'#54'NOP'#00'NTS'#25'NZA'#40'NZB'#42'NZC'#48'NZE'#45'NZU'#44 +
                 'PCH'#71'RAA'#80'RAB'#82'RAC'#88'RAL'#65'RAM'#67'RAU'#60'RBR'#12'RCD'#70 +
                 'RC1'#72'RC2'#75'RC3'#78'RDS'#86'RD1'#70'RD2'#73'RD3'#76'REQ'#11 +
                 'RPY'#79'RSA'#81'RSB'#83'RSC'#89'RSL'#66'RSM'#68'RSU'#61'RTA'#05'RTC'#03'RTN'#04 +
                 'RWD'#55'SCT'#36'SDA'#22'SDS'#85'SET'#27'SIA'#23'SIB'#28'SLO'#16'SLT'#35'SML'#18 +
                 'SRD'#31'SRT'#30'STD'#24'STI'#29'STL'#20'STU'#21'SUP'#11'SXA'#51'SXB'#53'SXC'#59 +
                 'TLU'#84'UFA'#02'WDS'#87'WR1'#71'WR2'#74'WR3'#77'WTA'#07'WTM'#56'WTN'#06;
     while sOpcodes <> '' do begin
        sOp2 := copy(sOpcodes, 1, 3);
        nOp2 := ord(sOpcodes[4]);
        sOpcodes := copy(sOpcodes, 5, length(sOpcodes));
        if sOp2 = sOp then begin
           if nOp <> nOp2 then err('Opcode ' + sOp + ' has code ' + IntToStr(nOp2) +
                                ' but assembled as ' + IntToStr(nOp));
           exit;
        end;
     end;
     err('Illegal opcode ' + sOp);
  end;

  function ascii_to_ibm650chr(s: string): integer;
  var sCharMap, sCharNum: string;
      n: integer;
  begin
     sCharMap := '123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ/=+.,--*0,(';
     sCharNum := #91#92#93#94#95#96#97#98#99 +
                 #61#62#63#64#65#66#67#68#69 +
                 #71#72#73#74#75#76#77#78#79 +
                 #82#83#84#85#86#87#88#89 +
                 #81#48#20#18#19#30#49#29#90#38#39;
     Result := 0;
     if length(s) = 0 then exit;
     n := Pos(s[1], sCharMap);
     if n = 0 then exit;
     Result := ord(sCharNum[n]);
  end;

  procedure Read_SOAP_source;
  var f: text;
      fn: string;
      s, sLabel, sAddr: string;
      i,n,n1,n2,neg,soapI_syntax, comment: integer;
      ch: char;
  begin
     fn := ParamStr(1);
     nSoap := 0;
     nSymb := 0;
     soapI_syntax := 0;
     AssignFile(f, fn);
     reset(f);
     while not eof(f) do begin
         readln(f, s);
         s := copy(s,1,79);
         for i := 1 to length(s) do s[i] := Upcase(s[i]);
         writeln(s);
         while length(s) < 79 do s := s + ' ';
         if trim(s) = '' then continue;
         comment :=0;
         if (s[8] = '1') and (soapI_syntax = 0) then comment := 1; // comment line for SOAP II -> ignore
         if (s[8] = '5') then begin soapI_syntax := 1; comment := 1; end; // comment line for SOAP I -> ignore
         if (s[6] = '1') then comment := 1; // comment line -> ignore
         if (s[8] = '7') then comment := 1; // supersopa: type 7 card is ignored 
         if comment = 1 then begin
            if s[8] = '5' then s[8] := '1';
            s := copy(s, 6, 255);
            repeat
               i := pos('  ',s);
               if i = 0 then break;
               delete(s, i, 1);
            until false;
            soap[nSoap].sLin := trim(s);
            soap[nSoap].OpStr   := 'COMMENT';
            soap[nSoap].OpNum   := 0;
            soap[nSoap].neg := 2;
            inc(nSoap);
            continue;
         end;
         neg := 0;
         if s[11]='-' then begin inc(neg) end else
         for i := 18 to 21 do if s[i] = '-' then begin inc(neg); break; end;
         for i := 62 to 65 do if s[i] = '-' then begin inc(neg); break; end;
         if neg = 1 then err('unpaired word minus sign');
         if neg <> 0 then neg := 1;

         // get location label
         if trim(copy(s,13,5))<>'' then begin
            if (s[13] >= '0') and (s[13] <= '9') and (s[12]=' ') and (s[14]=' ') then begin
               // super soap program point
               sLabel := '<PP>';
               n := ord(s[13])-ord('0');
            end else begin
               parse_label(s, 'op addr', 13, 5, sLabel, n);
            end;
            soap[nSoap].AddrStr := sLabel;
            soap[nSoap].AddrNum := n;
         end;
         
         // get opcode
         sLabel := copy(s, 22,3);
         if (s[8] = '3') then begin
            if trim(sLabel) = '' then begin
               sLabel := 'REG';
            end;
         end;
         soap[nSoap].sLin    := s;
         if (s[8] = '4') then sLabel := 'SYN';
         if (s[8] = '1') and (soapI_syntax = 1) then sLabel := 'BLR';

         if (sLabel = 'DON') or (sLabel = 'HMO') then begin // SuperSoap pseudo-ops)
            if (trim(copy(s,25,11)) <> '') then begin
               err('Line must contain only pseudo-op');
            end;
            soap[nSoap].sLin := trim(s);
            soap[nSoap].OpStr   := '';
            soap[nSoap].OpNum   := 0;
            soap[nSoap].neg := 2;
            inc(nSoap);
            continue;
         end;
         if (sLabel = 'HED') then begin
            if (trim(copy(s,25,2)) <> '') or (trim(copy(s,28,8)) <> '') then begin
               err('HED char in bad position');
            end;
            sHED := s[27]; // set head char
            soap[nSoap].OpStr   := sLabel;
            soap[nSoap].OpNum   := 0;
            soap[nSoap].neg := 2;
            inc(nSoap);
            continue;
         end;
         if (sLabel = 'BLR') or (sLabel = 'BLA') then begin
            // psuedo ops Block Reserve/Available
            soap[nSoap].OpStr   := sLabel;
            soap[nSoap].OpNum   := 0;
            parse_label(s, 'addr1',      27, 5, sLabel, n);
            soap[nSoap].DaStr   := sLabel;
            soap[nSoap].DaNum   := n;
            parse_label(s, 'addr2',      34, 5, sLabel, n);
            soap[nSoap].IaStr   := sLabel;
            soap[nSoap].IaNum   := n;
            soap[nSoap].neg := 2;
            inc(nSoap);
            continue;
         end;
         if (sLabel = 'SYN') or (sLabel = 'EQU') then begin // define symbol
            soap[nSoap].OpStr   := sLabel;
            soap[nSoap].OpNum   := 0;
            parse_label(s, 'sym',      27, 5, sLabel, n);
            if sLabel = '' then err('Missing label');
            if sLabel = '<NUM>' then begin
               // normal soap does not allow SYM number -> err('label for sym cannot be a number');
               // supersoap allows SYN mun -> reserve area. Same as BLR num num
               soap[nSoap].DaStr   := sLabel;
               soap[nSoap].DaNum   := n;
               soap[nSoap].neg := 2;
               inc(nSoap);
               continue;
            end;
            soap[nSoap].DaStr   := sLabel;
            soap[nSoap].DaNum   := n;
            soap[nSoap].neg := 2;
            inc(nSoap);
            continue;
         end;
         if (sLabel = 'COR') or (sLabel = 'DRC') or (sLabel = 'ADN') or
            (sLabel = 'FIL') or (sLabel = 'NMO') or (sLabel = 'UND') or
            (sLabel = 'PAL') or (sLabel = 'CDD') or (sLabel = 'END') or
            (sLabel = 'CON') or (sLabel = 'COD') then begin // supersoap pseudo-ops
            soap[nSoap].OpStr   := sLabel;
            soap[nSoap].OpNum   := 0;
            parse_label(s, 'cor',      27, 5, sLabel, n);
            soap[nSoap].DaStr   := sLabel;
            soap[nSoap].DaNum   := n;
            parse_label(s, 'fil',      34, 5, sLabel, n);
            soap[nSoap].IaStr   := sLabel;
            soap[nSoap].IaNum   := n;
            soap[nSoap].neg := 2;
            inc(nSoap);
            continue;
         end;
         if (sLabel = 'REG') then begin // define region
            soap[nSoap].OpStr   := sLabel;
            soap[nSoap].OpNum   := 0;
            sLabel := s[27];
            if sLabel = ' ' then err('Missing label for reg');
            parse_num(s, 'reg',  -28, 4, n1);
            parse_num(s, 'reg',   35, 4, n2);
            if (n1 > 3999) then err('Addr ' + IntToStr(n1) + ' out of range');
            if (n2 > 3999) then err('Addr ' + IntToStr(n2) + ' out of range');
            soap[nSoap].DaStr   := '<NUM>';
            soap[nSoap].DaNum   := n1;
            soap[nSoap].IaStr   := sLabel;
            soap[nSoap].IaNum   := n2;
            AddRegionTo_Symbol_Table(sLabel,n1, n2);
            soap[nSoap].neg := 2;
            inc(nSoap);
            continue;
         end;
         parse_label(s, 'op str',  22, 3, sLabel, n);
         soap[nSoap].OpStr   := sLabel;
         soap[nSoap].OpNum   := n;
         if (sLabel = 'ALF') or (sLabel = '') then begin
            // string contant
            parse_label(s, 'da',     -27, 5, sLabel, n);  // string constant
            soap[nSoap].OpStr   := '<NUM>';
            soap[nSoap].OpNum   := ascii_to_ibm650chr(copy(sLabel,1,1));
            soap[nSoap].DaStr   := '<NUM>';
            soap[nSoap].DaNum   := ascii_to_ibm650chr(copy(sLabel,2,1)) * 100 +
                                   ascii_to_ibm650chr(copy(sLabel,3,1));
            soap[nSoap].IaStr   := '<NUM>';
            soap[nSoap].IaNum   := ascii_to_ibm650chr(copy(sLabel,4,1)) * 100 +
                                   ascii_to_ibm650chr(copy(sLabel,5,1));
         end else begin
            // regular opcode
            parse_label(s, 'da',      27, -5, sLabel, n);
            ch:=s[32];
            if (ch <> ' ') then begin
               if (ch <> 'A') and (ch <> 'B') and (ch <> 'C') and (ch <> 'D') then begin
                  err('Invalid index tag "'+ch+'" on da address');
               end;
               if sLabel = '<NUM>' then begin
                  if n < 9000 then begin
                     if (ch='A') then inc(n, 2000) else
                     if (ch='B') then inc(n, 4000) else
                     if (ch='C') then inc(n, 6000);
                  end else begin
                     if (ch='A') then inc(n, 200) else
                     if (ch='B') then inc(n, 400) else
                     if (ch='C') then inc(n, 600);
                  end;
               end;
            end;
            soap[nSoap].DaStr   := sLabel;
            soap[nSoap].DaNum   := n;
            soap[nSoap].DaTag   := ch;
            parse_label(s, 'ia',      34, -5, sLabel, n);
            ch:=s[39];
            if (ch <> ' ') then begin
               if (ch <> 'A') and (ch <> 'B') and (ch <> 'C') and (ch <> 'D') then begin
                  err('Invalid index tag "'+ch+'" on da address');
               end;
               if sLabel = '<NUM>' then begin
                  if n < 9000 then begin
                     if (ch='A') then inc(n, 2000) else
                     if (ch='B') then inc(n, 4000) else
                     if (ch='C') then inc(n, 6000);
                  end else begin
                     if (ch='A') then inc(n, 200) else
                     if (ch='B') then inc(n, 400) else
                     if (ch='C') then inc(n, 600);
                  end;
               end;
            end;
            soap[nSoap].IaTag   := ch;
            soap[nSoap].IaStr   := sLabel;
            soap[nSoap].IaNum   := n;
         end;
         if bGenerateSourceCode then begin
            // skip assembled data check
            if (soap[nSoap].AddrStr <> '<NUM>') and
               (soap[nSoap].AddrStr <> '') then begin
               sLabel := soap[nSoap].AddrStr;
               if Search_Symbol_Table(sLabel) < 0 then begin
                  AddTo_Symbol_Table(sLabel, 9999, true);
               end;
            end;
         end else begin
            parse_num(s, 'addr', 58, 4, n);
            soap[nSoap].Addr    := n;
            if (s[65]='+') then s[65]:=' '; // ignore + sign before opcode
            if (s[65]='-') then begin
               s[65]:=' '; // ignore - sign before opcode, should already be set in neg var
               if neg<>1 then err('Missing "-" before opcode');
            end;
            parse_num(s, 'op',   66, 2, n);
            soap[nSoap].Op      := n;
            parse_num(s, 'da',   70, 4, n);
            soap[nSoap].Da      := n;
            parse_num(s + ' ', 'ia',   76, 4, n);
            soap[nSoap].Ia      := n;
            if (soap[nSoap].AddrStr = '<NUM>') and
               (soap[nSoap].AddrNum <> soap[nSoap].Addr) and
               (soap[nSoap].AddrNum < 9000) and
               (soap[nSoap].Addr < 9000) then begin
               err('Numeric Addr does not match with assembled addr');
            end;
            if (soap[nSoap].OpStr = '<NUM>') and
               (soap[nSoap].OpNum <> soap[nSoap].Op) then begin
               err('Numeric Op does not match with assembled op');
            end;
            if (soap[nSoap].DaStr = '<NUM>') and
               (soap[nSoap].DaNum <> soap[nSoap].Da) then begin
               err('Numeric Da does not match with assembled Da');
            end;
            if (soap[nSoap].IaStr = '<NUM>') and
               (soap[nSoap].IaNum <> soap[nSoap].Ia) then begin
               err('Numeric Ia does not match with assembled Ia');
            end;
            if (soap[nSoap].AddrStr <> '<NUM>') and (soap[nSoap].AddrStr <> '<PP>') and
               (soap[nSoap].AddrStr <> '') then begin
               i := Search_Symbol_Table(soap[nSoap].AddrStr);
               if i < 0 then begin
                  AddTo_Symbol_Table(soap[nSoap].AddrStr, soap[nSoap].Addr, false);
               end else begin
                  if (symb[i].Addr <> soap[nSoap].Addr) and
                     (symb[i].Addr < 9000) and (soap[nSoap].Addr < 9000) then begin
                      err('Symbol Addr does not match with assembled addr');
                  end;
               end;
            end;
            if (soap[nSoap].OpStr = '') then begin
               if (soap[nSoap].IaStr = '') then begin
                  // assembling a string
                  validate_str(soap[nSoap].DaStr, soap[nSoap].Op, soap[nSoap].Da, soap[nSoap].Ia);
                end else begin
                  err('Missing instruction opcode');
                end;
            end else if (soap[nSoap].OpStr <> '<NUM>') then begin
                validate_opcode(soap[nSoap].OpStr, soap[nSoap].Op);
            end;
         end;
         soap[nSoap].neg := neg;
         inc(nSoap);
     end;
     CloseFile(f);
  end;

   procedure Check_Symbol_Table(nn: integer; sLabel: string; nAddr: integer; ChTag: char);
   var iFound, npp, n: integer;
   begin
      if (sLabel = '<NUM>') or (sLabel = '') then exit;
      if (length(sLabel)=2) and (sLabel[1] >= '0') and (sLabel[1] <= '9') and
         ((sLabel[2] = 'F') or (sLabel[2] = 'B')) then begin
         // supersoap program point
         npp:= ord(sLabel[1])-ord('0');
         repeat
            if (sLabel[2] = 'B') then dec(nn) else inc(nn);
            if (nn<1) or (nn>nSoap) then begin
               err('Symbol ' + sLabel + ' does not have a matching program point');
            end;
            if (soap[nn].AddrStr<>'<PP>') or (soap[nn].AddrNum<>npp) then continue;
            // program point found
            if (nAddr = soap[nn].Addr) then exit;
            if nAddr >= 9000 then exit;
            if soap[nn].Addr >= 9000 then exit;
            err('Symbol ' + sLabel + ' assembled to ' + IntToStr(nAddr) +
                ' but references a program point at location '+IntToStr(soap[nn].Addr)+
                ' at same addr as line: ' + #13#10 + soap[nn].sLin);
            exit;
         until false;
      end;
      iFound := Search_Symbol_Table(sLabel);
      if iFound < 0 then err('Symbol ' + sLabel + ' not found');
      n := symb[iFound].Addr;
      if ChTag <> ' ' then begin
         if n<9000 then begin
            if ChTag = 'A' then inc(n, 2000) else
            if ChTag = 'B' then inc(n, 4000) else
            if ChTag = 'C' then inc(n, 6000);
         end else begin
            if ChTag = 'A' then inc(n, 200) else
            if ChTag = 'B' then inc(n, 400) else
            if ChTag = 'C' then inc(n, 600);
         end;
      end;
      if (n <> nAddr) and (nAddr <9000) then begin
         if ChTag = ' ' then begin
            err('Symbol ' + sLabel + ' defined as ' + IntToStr(symb[iFound].Addr) +
                ' but assembled to ' + IntToStr(nAddr));
         end else begin
            err('Symbol ' + sLabel + ' defined as ' + IntToStr(symb[iFound].Addr) +
                ' tagged to addr ' + IntToStr(n) + ' (tag ' + ChTag+')' + 
                ' but assembled to ' + IntToStr(nAddr));
         end;
      end;
   end;

  procedure Validate_SOAP_source;
  var nn, nn2, i, n1, n2, iFound: integer;
      s, sLabel, s1: string;
  begin
     for nn := 0 to nSoap-1 do begin
         s := soap[nn].sLin;
         writeln(s);
         if soap[nn].OpStr = 'HED' then begin
            sHED := s[27]; // set head char
            continue;
         end;
         if soap[nn].OpStr = 'REG' then begin
            n1 := soap[nn].DaNum;
            sLabel := soap[nn].IaStr;
            n2 := soap[nn].IaNum;
            AddRegionTo_Symbol_Table(sLabel,n1, n2);
            continue;
         end;
         if (soap[nn].OpStr = 'EQU') or (soap[nn].OpStr = 'BLR') or
            (soap[nn].OpStr = 'SYM') then begin
            sLabel := soap[nn].AddrStr;
            if (sLabel<>'') and (sLabel<>'<NUM>') then begin
                if soap[nn].DaStr='<NUM>' then begin
                   n1 := soap[nn].DaNum;
                end else begin
                   iFound := Search_Symbol_Table(soap[nn].DaStr);
                   if iFound < 0 then n1 := -1
                                 else n1 := symb[iFound].Addr;
                end;
                if n1 >= 0 then begin
                   if (copy(sLabel,2,4)='0000') or (copy(sLabel,2,4)='0001') then begin
                      if (copy(sLabel,2,4)='0000') then inc(n1);
                      s1 := copy(sLabel, 1, 1);
                      AddRegionTo_Symbol_Table(s1,n1, n1+300);
                   end else begin
                      iFound := Search_Symbol_Table(sLabel);
                      if iFound < 0 then begin
                         AddTo_Symbol_Table(sLabel, n1, true);
                      end else begin
                         if symb[iFound].Addr <> n1 then begin
                            err('Trying to define symbol ' + sLabel + ' at '+IntToStr(n1)+
                                ' but already defined at ' + IntToStr(symb[iFound].Addr));
                         end;
                      end;
                   end;
                end;
            end;
         end;
         if soap[nn].neg = 2 then continue;
         if soap[nn].OpStr = '' then continue;
         nn2 := nn;
         repeat inc(nn2) until soap[nn2].neg <> 2;
         if soap[nn].DaStr <> '' then begin
            Check_Symbol_Table(nn, soap[nn].DaStr, soap[nn].Da, soap[nn].sLin[32]);
         end else begin
            if (soap[nn2].Addr <> soap[nn].Da) and (soap[nn].Da < 9000) and (soap[nn2].Addr < 9000) then begin
               err('Assembled da not same as next instr addr');
            end;
         end;
         if soap[nn].IaStr <> '' then begin
            Check_Symbol_Table(nn, soap[nn].IaStr, soap[nn].Ia, soap[nn].sLin[39]);
         end else begin
            if (soap[nn2].Addr <> soap[nn].Ia) and (soap[nn2].Addr < 9000) and (soap[nn].Ia < 9000) then begin
               err('Assembled Ia not same as next instr addr');
            end;
         end;
         if soap[nn].Addr < 2000 then
         for i := nn2 to nSoap-1 do begin
            if soap[nn].Addr <> soap[i].Addr then continue;
            if soap[i].Addr = 0 then continue;
            if copy(soap[i].sLin, 5,1) <> ' ' then continue; // errata fix pathc card. do not compare
            if (soap[nn].Da <> soap[i].Da) or
               (soap[nn].Ia <> soap[i].Ia) then err('different contents at same addr as line: ' + #13#10 + soap[i].sLin);
         end;
     end;
  end;

  function IntToStrN(n, len: integer): string;
  var s: string;
  begin
     s := IntToStr(n);
     while length(s) < len do s := '0' + s;
     result := s;
  end;

  function SymbToStr(s: string; n, len: integer): string;
  begin
     if s = '<NUM>' then begin
        s := ' ' + IntToStrN(n, len-1);
     end else if s = '<PP>' then begin
        s := IntToStrN(n,1);
        while length(s) < len do s := s + ' ';
     end else begin
        s := trim(s);
        while length(s) < len do s := s + ' ';
     end;
     result := s;
  end;

  procedure Generate_SOAP_load_cards;
  var f: text;
      i, j, loc: integer;
      fn, s: string;
      c: char;
  begin
   fn := ParamStr(2);
   writeln;
   writeln('Generating 1-word per card load deck ' + fn);
   writeln;
     AssignFile(f, fn);
     rewrite(f);
     for i := 0 to nSoap-1 do begin
        if soap[i].neg = 2 then continue;
        loc := soap[i].Addr;
        if (loc >= 8000) and (loc < 9000) then continue;
        // 1 word per card format
        s := '6I1954195C' +
             '      0000' +
             '24' + IntToStrN(soap[i].Addr, 4) + '800?' +
             IntToStrN(soap[i].Op, 2) + IntToStrN(soap[i].Da, 4) + IntToStrN(soap[i].Ia, 4);
        // if word negative, add X(11) punch to last digit
        if soap[i].neg <> 0 then begin
           c := s[length(s)];
           if (c = '0') then c := '!' else c := chr(ord('J')-1+ord(c)-ord('0'));
           s[length(s)] := c;
        end;
        // add soap source
        s := s + ' ';
        if soap[i].neg <> 0 then s := s + '-' else s := s + ' ';
        s := s +
             SymbToStr(soap[i].AddrStr, soap[i].AddrNum, 5) +
             SymbToStr(soap[i].OpStr, soap[i].OpNum, 3) +
             SymbToStr(soap[i].DaStr, soap[i].DaNum, 5) +
             SymbToStr(soap[i].IaStr, soap[i].IaNum, 5);
        // add statement number, dash, then comments
        s := s + ' ' +
             copy(soap[i].sLin,1,4) + '-' + trim(copy(soap[i].sLin,42,16));
        // lowecase soap source
        for j := 41 to length(s) do begin
           if (s[j] >= 'A') and (s[j] <= 'Z') then s[j] := chr(ord(s[j]) - ord('A') + ord('a'));
        end;
        writeln(f, s);
     end;
     CloseFile(f);
   writeln;
   writeln('Done');
   writeln;
  end;

  procedure Generate_SOAP_source;
  var f: text;
      i, j: integer;
      fn, s: string;
  begin
   fn := ParamStr(2);
   writeln;
   writeln('Generating SOAP source deck ' + fn);
   writeln;
     AssignFile(f, fn);
     rewrite(f);
     for i := 0 to nSoap-1 do begin
        if soap[i].OpStr = 'COMMENT' then begin
           s := '                                        ' + soap[i].sLin;
        end else begin
           s := '                                          ' +
                SymbToStr(soap[i].AddrStr, soap[i].AddrNum, 5) +
                SymbToStr(soap[i].OpStr, soap[i].OpNum, 3) +
                SymbToStr(soap[i].DaStr, soap[i].DaNum, 5) + ' ' +
                SymbToStr(soap[i].IaStr, soap[i].IaNum, 5) + ' ' +
                trim(copy(soap[i].sLin,42,16));
        end;
        // lowecase soap source
        for j := 41 to length(s) do begin
           if (s[j] >= 'A') and (s[j] <= 'Z') then s[j] := chr(ord(s[j]) - ord('A') + ord('a'));
        end;
        writeln(f, s);
     end;
     CloseFile(f);
   writeln;
   writeln('Done');
   writeln;
  end;

var sExt: string;
begin
   if ParamStr(1) = '' then begin
      writeln('soapval source.txt [ deck.dck | deck_src.txt]');
      Halt(0);
   end;
   bGenerateSourceCode := false;
   sExt := ExtractFileExt(UpperCase(ParamStr(2)));
   if sExt = '.TXT' then bGenerateSourceCode := true;
   writeln;
   writeln('Pass 1');
   writeln;
   Read_SOAP_source;
   if not bGenerateSourceCode then begin
      writeln;
      writeln('Pass 2');
      writeln;
      Validate_SOAP_source;
   end;
   writeln;
   writeln('Finished');
   writeln;
   if sExt = '.DCK' then begin
      Generate_SOAP_load_cards;
   end;
   if bGenerateSourceCode then begin
      Generate_SOAP_source;
   end;
   Halt(0);
end.
