program Link;
{$APPTYPE CONSOLE}
uses
  SysUtils, FileCtrl, Math;

// reads assembled relative program output
// links to needed libraries
// generates prog.txt as absolute program ready to be loaded with SCP LOAD command
// command line:
//    link   [LOG|NOLOG]  file1.txt file2.txt ... libdir

// file1 is the main program
// libdir is the librarly directory. Will scan there lib_asm_NN.txt
// where NN ranges from 01 to 99        
//
// the generated absolute prog.txt is the same as generated usign the PACT compiler
// eg. Decks/22.txt is the same as Decks/S22.txt
// the only difference is the fact that the BASE48 code of symbol is not used here

procedure Halt(n: integer);
begin         
   if n>0 then writeln('Halt: ', n);
   System.Halt(n);
end;

procedure Halt2(s: string);
begin
   writeln('ERROR: ',s);
   System.Halt(9999);
end;


const max_Symbols = 400;
      max_asm     = 8192;
var fOut: text;

    nAsm: integer;
    sAsm: array [0..max_asm] of string; // the whole assembler prog loaded
    sLibFn: array [0..max_Symbols] of string; // lib files to be added together

    nSymbols: integer;
    tSymbols: array [0..max_Symbols] of record // warning: this array starts in 0!
          Name: string[8];
          filename: string[255];   // source assembler file where the symbol is defined
          Resolved: integer;  // = 1 -> symbol resolved in program being assembled
          Loc: integer;       // absolute location of symbol
       end;

    HiPerishableVar: integer; // highest value of P perishable library vars

    mLibSymb: integer;       // symbols defined in current library
    tLibSymb: array[0..max_Symbols] of record
       Name: string[8];
       cType: char; // D -> is defined in lib, U -> is used by lib, but defined in another lib
    end;

    bLog: integer;           // >0 -> print log on screen


function LocateSymbol(Name: string): integer;
var n: integer;
begin
   Name := trim(Name);
   if nSymbols > 0 then for n := 0 to nSymbols-1 do begin // Warning: tSymbol start on [0]
      if tSymbols[n].Name <> Name then continue; // not same symbol
      result := n;
      exit;
   end;
   result := -1;
end;

procedure AddLibraryRoutines;
label ScanLibs, ReadLib, AddLib;
var n, m, nPendingToBeResolved, nLib, bFound, bLibAdded, nLibFiles, nParam: integer;
    fLib: text;
    fn, s, sy, LibDir: string;
begin
   nLibFiles := 0; // number of included libraries
   nPendingToBeResolved := -1;
   nSymbols:= 0;
   nParam := 1; // command line param to parse
   LibDir := ''; // start in command line mode
   bLog := 0; // log not active

ScanLibs:
   nLib := 0;
   bLibAdded := 0; // =1 if a new lib has been added
   repeat
      if nPendingToBeResolved = 0 then break; // nothing pending to be resolved with libs
      if (LibDir = '') then begin
         // processing command line param mode
         fn := trim(ParamStr(nParam));
         if fn = '' then break; // processed all files from command line
         nParam := nParam + 1; // skip filename from cmd line
         if fn = 'LOG' then begin
            bLog := 1; continue;
         end else if fn = 'NOLOG' then begin
            bLog := 0; continue;
         end else if DirectoryExists(fn) then begin
            if (fn[length(fn)]<>'\') then fn := fn + '\';
            LibDir := fn;
            if bLog>0 then writeln('Start library dir "'+fn+'" scan');
         end else begin
            if not FileExists(fn) then halt2(fn + ' not found');
         end;
      end;
      if (LibDir <> '') then begin
         // processing library directory mode
         bFound := 0;
         repeat
            nLib := nLib + 1;
            if nLib > 99 then begin
               break; // scan files "lib_asm_01.txt" up to "lib_asm_99.txt"
            end else if nLib < 10 then begin
               fn := LibDir + 'lib_asm_0'+IntToStr(nLib)+'.txt';
            end else begin
               fn := LibDir + 'lib_asm_'+IntToStr(nLib)+'.txt';
            end;
            if FileExists(fn) then begin
               bFound := 1;
               break;
            end;
         until false;
         if bFound = 0 then begin
            if bLog>0 then writeln('terminate library dir scan');
            break; // all libs scanned
         end;
      end;
      if nPendingToBeResolved < 0 then nPendingToBeResolved := 0;
      // lib file found. read Symbols defined
ReadLib:
      mLibSymb := -1;
      assign(fLib, fn);
      reset(fLib);
      if bLog>0 then writeln('Scan File "'+fn+'"');
      while not eof(fLib) do begin
          readln(fLib, s);
          if (Trim(s)<>'') and (s[1] <> ';') then break;
          if (Copy(s, 1,5) <> '; SY ') then continue;
          m := StrToInt(Trim(Copy(s, 6,3))); // SY number
          if m > max_symbols then Halt2('Too many lib symbols');
          tLibSymb[m].Name := Trim(Copy(s, 22,8)); // long symbol name (8 chars)
          if Copy(s, 11,1) = 'D' then tLibSymb[m].cType := 'D' // is defined
                                 else tLibSymb[m].cType := 'U'; // is used (is called)
          mLibSymb := m;
      end;
      CloseFile(fLib);
      // if main program, goto to include the filename
      if nLibFiles = 0 then goto AddLib;
      // check if a defined symbols in lib is one of the symbol pending to be resolved
      bFound := 0;
      for m:= 0 to mLibSymb do begin
         if tLibSymb[m].cType <> 'D' then continue;
         // symbol defined in lib. check if it resolves a pending symbol in prog
         sy := tLibSymb[m].Name;
         n := LocateSymbol(sy);
         if n<0 then continue; // continue, not in main symbol table
         if tSymbols[n].Resolved > 0 then begin
            if tSymbols[n].filename <> fn then begin
               halt2('File "'+fn+'" defines symbol "'+sy+'" already defined in file '+tSymbols[n].filename); // double definition
            end;
            continue; // skip already defined symbol            
         end;
         tSymbols[n].Resolved := 1;  // mark as resolved
         tSymbols[n].filename := fn; // file where symbol has been defined
         nPendingToBeResolved := nPendingToBeResolved - 1;
         if bLog>0 then writeln('   Defines Symbol "'+sy+'" -> add library');
         bFound := 1;
      end;
      // if lib does not resolve any symbol then proceed to the next lib
      if bFound = 0 then continue;
      // this lib will be imported. Save the filename
AddLib:
      // import symbols from lib
      // add USEd symbols from lib to pending symbols (if not yet pending)
      // add DEfined symbols from lib to resolved symbols (if not yet resolved)
      for m:= 0 to mLibSymb do begin
         sy := tLibSymb[m].Name;
         if (LocateSymbol(sy) < 0) then begin
            // used symbol not in in list -> add to tsymbol list
            n := nSymbols;  // warning! tSymbols start in [0]
            nSymbols := nSymbols + 1;
            if nSymbols > max_Symbols then Halt2('Too many symbols referenced');
            tSymbols[n].Name := sy;
            tSymbols[n].Loc := -1;
            if tLibSymb[m].cType = 'D' then begin
               // symbol defined in lib. add to symbols as resolved if not yet
               tSymbols[n].Resolved := 1;
               tSymbols[n].filename := fn; // file where symbol has been defined
               // this symbol was not pending to be resolved, so no need to decr nPendingToBeResolved
               if bLog>0 then writeln('   Defines New Symbol "'+sy+'"');
            end else begin
               // symbol used in lib. add to to symbols as pending to be resolved if not yet
               tSymbols[n].Resolved := 0; // not resolved
               nPendingToBeResolved := nPendingToBeResolved + 1;
               if bLog>0 then writeln('   Uses New Symbol "'+sy+'"');
            end;
         end;
      end;
      // add the lib to list of libs to be appended to main prog
      sLibFn[nLibFiles]:=fn;
      nLibFiles := nLibFiles + 1;
      bLibAdded := 1; // signal a new lib has been appened to main prog
   until false;
   // all libs scanned
   if nPendingToBeResolved < 0 then Halt2('Missing main file to link');
   if nPendingToBeResolved > 0 then begin
      // get symbols pending to be resolved
      m := 0; sy := '';
      for n := 0 to nSymbols-1 do begin // Warning: tSymbol start on [0]
         if tSymbols[n].Resolved > 0 then continue; // symbol already resolved
         if sy <> '' then sy := sy + ', ';
         sy := sy + tSymbols[n].Name;
         m:=m+1; // m=number of symblos not resolved
      end;
      if (bLog>0) and (m>0) and (bLibAdded>0) then writeln('Symbol(s) not yet resolved: ',sy);
      // if at least one file added from lib, goto to ScanLibs to scan again the lib
      if bLibAdded > 0 then goto ScanLibs; // perform a new scan
      // all libs scanned, nothing added -> the symbol(s) pending to be resolved are not in this lib
      Halt2(IntToStr(m)+' symbol(s) not in libs: '+sy);
   end;
   // sort list of lib files to add so the files "lib_asm_NN.txt" are loaded in order
   // use simple lazy-coder-friendly bubble sort algorithm
   repeat
      bFound := 0; // no swap
      for n := 1 to nLibFiles-1 do begin
         if Pos('\lib_asm_', sLibFn[n-1]) = 0 then continue; // this entry is not an "lib_asm_NN.txt" file to sort
         if Pos('\lib_asm_', sLibFn[n  ]) = 0 then continue; // this entry is not an "lib_asm_NN.txt" file to sort
         if sLibFn[n-1] > sLibFn[n] then begin
            // swap unordered entries
            fn := sLibFn[n-1]; sLibFn[n-1] := sLibFn[n]; sLibFn[n] := fn;
            bFound := 1; // signal a reordering swap has been done
         end;
      end;
      if bFound = 0 then break; // if no reordering swap done -> sort finished
   until false;
   // now add the lib code to main prog (sAsm array)
   // PACTREL format:
   // 123456789012345678901234567890
   //          nnnn +nnnnnn    comments
   //          nnnn -op r nnnn comments
   nAsm := 0;             // number of lines on the resulting asm file with all included libraries
   HiPerishableVar := -1; // start with no perishable vars used
   for nLib := 0 to nLibFiles-1 do begin
      fn := sLibFn[nLib];
      assign(fLib, fn);
      reset(fLib);
      if bLog>0 then writeln('Read File "'+fn+'"');
      reset(fLib);
      while not eof(fLib) do begin
         readln(fLib, s);
         if nAsm >= max_asm then halt2('Program too big');
         if (Copy(s, 1,5) = '; SY ') then begin
            m := StrToInt(Trim(Copy(s, 6,3))); // SY number
            if m > max_symbols then Halt2('Too many lib symbols');
            tLibSymb[m].Name := Trim(Copy(s, 22,8)); // symbol name (8 chars)
         end;
         if (length(s) > 24 ) and (s[1] <> ';') and (Copy(s,16,4) = 'T  S') then begin
            // PACTREL format:
            // 123456789012345678901234567890             x
            //          nnnn -op r nnnn comments
            // replace       "T  S nnnn"
            // by            ">name1234" with

            m := StrToInt(Trim(Copy(s,21,4))); // SY number
            sy := tLibSymb[m].Name; while length(sy)<8 do sy := sy + ' ';
            s := Copy(s,1,15) + '>' + sy + Copy(s, 25, 255);
         end;
         if (length(s) >= 15 ) and (Copy(s,1,5) = ';  P ') then begin
            // get the highest perishable address used by libraries
            m := StrToInt(Trim(Copy(s,21,4))); // P region length
            if HiPerishableVar < m then HiPerishableVar := m;
         end;
         sAsm[nAsm] := s;
         nAsm := nAsm + 1;
      end;
      CloseFile(fLib);
   end; 
end;


procedure SetSymblNumber;
var n,d,m: integer;
    s,sy: string;
begin
   // PACTREL format:
   // 123456789012345678901234567890             x
   //          nnnn -op r nnnn comments
   // replace       ">name1234" with
   // by            "T  S nnnn"

   for n := 0 to nAsm-1 do begin
      s := sAsm[n];
      if (length(s) > 24 ) and (s[1] <> ';') and (s[16] = '>') then begin
         // referencing a symbol
         // replace name by its symbol entry in tSymbol table
         sy := Trim(Copy(s,17,8));
         d := -1;
         for m := 0 to nSymbols-1 do begin
            if sy <> tSymbols[m].Name then continue;
            d := m;
            break;
         end;
         if d <0 then Halt2('Symbol not found');
         sy := IntToStr(d); while length(sy) < 4 do sy := '0'+sy;
         s := Copy(s,1,15) + 'T  S ' + sy + Copy(s, 25, 255);
         sAsm[n]:=s;
      end;
   end;
   // print symbol table
   if bLog>0 then begin
      writeln('Symbol table:');
      for n := 0 to nSymbols-1 do begin
         writeln('   ',tSymbols[n].Name);
      end;
   end;
end;


var tMne: array [0..32] of string[5] = (
   'H ', // OP_STOP           0  Stop and transfer
   'T ', // OP_TR             1  Transfer
   'TF', // OP_TR_OV          2  Transfer on overflow: Transfer if OV=1 (and reset ov)
   'TP', // OP_TR_PLUS        3  Transfer on plus: Transfer if ACC>0
   'TZ', // OP_TR_ZR          4  Transfer on zero: Transfer if ACC=0
   'S ', // OP_SUB            5  Subtract: Acc = Acc - C(x)
   'RS', // OP_R_SUB          6  Reset and Subtract: Acc = - C(x)
   'SV', // OP_SUB_AB         7  Subtract Absolute value: Acc = Acc - |C(x)|
   'N ', // OP_NOOP           8  No Operation
   'A ', // OP_ADD            9  Add: Acc = Acc + C(x)
   'RA', // OP_R_ADD         10  Reset and Add: Acc = C(x)
   'AV', // OP_ADD_AB        11  Add Absolute value: Acc = Acc + |C(x)|
   'ST', // OP_STORE         12  Store: Store accumulator at given address: C(x)= Acc
   'SA', // OP_STORE_A       13  Store Address: Store 12 rightmost bits from Acc at addr x (halfword)
   'SM', // OP_STORE_MQ      14  Store MQ: C(x) = MQ
   'LM', // OP_LOAD_MQ       15  Load MQ: MQ = C(x)
   'M ', // OP_MPY           16  Multiply: Acc:MQ = C(x) * MQ. Acc=35 most significants bits + sign
   'MR', // OP_MPY_R         17  Multiply and Round: same as above, followed by round
   'D ', // OP_DIV           18  Divide: MQ = Acc:MQ / C(x), Acc=remainder
   'R ', // OP_ROUND         19  Round: if leftmost bit of MQ=1 incr Acc
   'LL', // OP_L_LEFT        20  Long Left Shift: Acc:MQ << x, ACC sign taken from MQ
   'LR', // OP_L_RIGHT       21  Long Right Shift: Acc:MQ >> x, MQ sign taken from Acc
   'AL', // OP_A_LEFT        22  Accumulator Left Shift: Acc << x, Acc sign unchanged
   'AR', // OP_A_RIGHT       23  Accumulator Right Shift: Acc >> x, Acc sign unchanged
   'RD', // OP_READ          24  Prepare to Read. Start mechanical movement forward
   'RB', // OP_READ_B        25  Prepare to Read Backward. Start mechanical movement backward
   'W ', // OP_WRITE         26  Prepare to Write. Start mechanical movement
   'WE', // OP_WRITE_EF      27  Write End of File.
   'RW', // OP_REWIND        28  you guessed!
   'SD', // OP_SET_DR        29  Ser Drum Address
   'SE', // OP_SENSE         30  Sense and Skip
   'C ', // OP_COPY          31  Copy and Skip
   'EX'); // OP_EXTR      (13+32) Extract: C(x) = C(x) AND Acc


procedure writefOutD(Loc: integer; cSgn: char; Data: integer; sComment: string);
begin
   writeln(fOut,' ':8, Loc:4,cSgn,Data:6,'  ', sComment);
   if bLog>0 then writeln(' ':8, Loc:4,cSgn,Data:6,'  ', sComment);
end;

procedure writefOutI(Loc: integer; cSgn: char; nOp, Data: integer; sComment: string);
begin
   writeln(fOut,' ':8, Loc:4,cSgn,nOp:2,' ',Data:4,' ', sComment);
   if bLog>0 then writeln(' ':8, Loc:4,cSgn,nOp:2,' ',Data:4,' ', sComment);
end;

procedure MakeEven(var n: integer);
begin
   n := (n + 1) and $fffe;
end;

procedure Load;
var sLin, sCod, sOp, sy: string;
    iAsm, nLen, n, m, iBase, Hi, Loc, Data, nOp: integer;
    I_Org, S_Org, V_Org, N_Org, P_Org, T_Org: integer;
    V0_Reg: integer;
    cSgn, cReg: char;

    procedure Halt3(s: string); begin Halt2(s+' '+IntTostr(iAsm)+': '+sLin); end;

begin
   // init vars to invalid values
   iBase := -1;
   V_Org := -1; N_Org := -1; T_Org := -1; V0_Reg := -1;

   if HiPerishableVar < 0 then HiPerishableVar := 0
                          else MakeEven(HiPerishableVar);
   // Calc I_Org = lo addr where program code starts
   //      S_Org = addr of region origin table
   //      P_Org = addr of perishable vars
   P_Org := 6;
   I_Org := P_Org + HiPerishableVar;
   if I_Org < 48 then I_Org := 48;
   MakeEven(I_Org);
   S_Org := I_Org; // addr for region origin table
   I_Org := I_Org + (nSymbols + 1) * 2; // 2 half-words per symbol.

   Hi := 4094;

   if bLog>0 then writeln('Absolute Program:');

   assign(fOut,'prog.txt');
   rewrite(fOut);
   writeln(fOut,' ':8, 'DEC');
   writeln(fOut,' ':8, 'NNNN OP ADDR COMMENTS');

   // PACTREL format:
   // 123456789012345678901234567890             x
   //          nnnn +nnnnnn    comments
   //          nnnn -op r nnnn comments
   // write assembler code in output file
   for iAsm := 0 to nAsm-1 do begin
      sLin := sAsm[iAsm];
      if Trim(sLin) = '' then continue; // skip empty lines
      sCod := Copy(sLin, 1, 5);
      if sCod = ';  I ' then begin
         // Instr region
         nLen := StrToInt(Trim(Copy(sLin,13,4)));
         MakeEven(I_Org);
         iBase := I_Org;
         I_Org := ibase + nLen;
         if I_Org > 4094 then halt3('Program too big');
         MakeEven(I_Org);
      end else if (sCod = ';  N ') or
                  (sCod = ';  V ') or
                  (sCod = ';  T ') or
                  (sCod = ';  P ') then begin
         // number/variable/temporal/perishable region
         nLen := StrToInt(Trim(Copy(sLin,13,4)));
         if odd(nLen)then nLen := nLen+1; // guarantee nLen is even;
         Hi := Hi - nLen;
         if Hi < 48 then halt3('Program too big');
         if (sCod[4]='N') then N_Org := Hi else
         if (sCod[4]='V') then begin V_Org := Hi; if V0_Reg<0 then V0_Reg:=V_Org; end else
         if (sCod[4]='T') then T_Org := Hi else
         if (sCod[4]='P') then begin end; // ignore perishable region setting. Already set to its max value
      end else if (sCod = '; SY ') then begin
         if Copy(sLin, 11,1) = 'D' then begin
            // define the symbol
            sy := Trim(Copy(sLin, 22,8)); // symbol name (8 chars)
            m  := StrToInt(Trim(Copy(sLin, 16,4))); // addr of symbol defined
            if (m > 4095) then halt3('Addr too big');
            n := LocateSymbol(sy);
            if n<0 then halt3('Symbol not defined');
            if iBase<0 then halt3('I-REG not set');
            tSymbols[n].Loc := iBase + m; // save the absolute location of symbol
         end;
      end else if (sCod[1] = ';') then begin
         // ignore comment lines
      end else if (sLin[16] >= '0') and (sLin[16]<='9') then begin
         // a data number
         m    := StrToInt(Trim(Copy(sLin, 10,4))); // rel addr of num defined
         if (m > 4095) then halt3('Loc too big');
         Loc  := m + iBase; // set absolute location
         cSgn := sLin[15];
         if cSgn <> '-' then cSgn := '+';
         Data := StrToInt(Trim(Copy(sLin,16,6))); // num defined
         writefOutD(Loc, cSgn, Data, Copy(sLin, 26, 255));
      end else if (sCod[1] = ' ') then begin
         // an instr
         m    := StrToInt(Trim(Copy(sLin, 10,4))); // addr of instr
         if (m > 4095) then halt3('Loc too big');
         if iBase < 0 then Halt3('I-REG not set');
         Loc  := m + iBase; // set absolute location
         cSgn := sLin[15];
         if cSgn <> '-' then cSgn := '+';
         sOp  := Copy(sLin, 16,2);
         nOp  := -1;
         for n := 0 to 32 do begin
            if tMne[n]<>sOp then continue;
            nOp := n;
            break;
         end;
         if nOp < 0 then Halt3('Invalid Mnemonic');
         if nOp=32 then begin
            // extract instr
            nOp:=13; if cSgn <> '-' then Halt3('EX instr should be negative');
         end;
         cReg := sLin[19];
         m    := StrToInt(Trim(Copy(sLin,21,4))); // operand of instr
         if (m > 4095) then halt3('Operand too big');
         // get region origin
         if cReg = 'A' then n := 0 else // absolute region -> origin = 0
         if cReg = 'I' then n := iBase else
         if cReg = 'S' then begin n := S_Org; m:=m*2+1; end else // points to entry point in region origin table
         if cReg = 'V' then n := V_Org else
         if cReg = 'N' then n := N_Org else
         if cReg = 'T' then n := T_Org else
         if cReg = 'P' then n := P_Org else Halt3('Invalid region');
         if n<0 then Halt3('Region not set');
         Data := n + m;
         writefOutI(Loc, cSgn, nOp, Data, Copy(sLin, 26, 255));
      end else begin
         Halt3('Invalid line');
      end;
   end;

   // write region origin table (i.e. the symbol table) in output file
   for n := 0 to nSymbols-1 do begin
      m := n*2+S_Org;
      // write 0 as BASE48 symbol name
      writefOutD(0+m, '-',  0, 'ROUTINE '+tSymbols[n].Name);
      writefOutI(1+m, '+',  1, tSymbols[n].Loc, 'ENTRY POINT');
   end;
   if V0_Reg>=0 then begin
      m := nSymbols*2+S_Org;
      writefOutD(0+m, '+',     2, 'VARIABLE REGION');
      writefOutD(1+m, '+',V0_Reg, 'ORIGIN');
   end;

   // write locations 4094 and 4095
   writefOutD(4094, '+',          S_Org, 'REGION ORIGIN TABLE LOCATION');
   writefOutD(4095, '+',tSymbols[0].Loc, 'INITIAL REGION');
   // write transfer to main prog (=1st HEAD:DEF routine)
   writefOutI(0, '+', 1, tSymbols[0].Loc, '         + TR        '+ tSymbols[0].Name+ '         PROGRAM START');

   CloseFile(fOut);

   if bLog>0 then begin
      writeln('Absolute Program generated');
      writeln('   1st Loc Free ',I_Org);
      writeln('   1st Loc Used ',Hi);
      writeln('   Available ',Hi-I_Org);
   end else begin
      writeln('Absolute Program generated. Available ',Hi-I_Org);
   end;
   if Hi < I_Org then halt3('Program too big. Needs '+IntToStr(I_Org-Hi)+' half words');
end;


////////////////////////////////////////////////////////
// Chain stages
////////////////////////////////////////////////////////

begin
  // Add library routines to resolve symbols
  AddLibraryRoutines;
  // set the symbol table & number
  SetSymblNumber;
  // Loader
  Load;
  Halt(0);
end.
