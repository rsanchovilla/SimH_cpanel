program TapeDump;
{$APPTYPE CONSOLE}
uses
  SysUtils;

var f1, f2: file;
  NumRead, NumWritten, fp: Integer;
  Buf: array[0..16*1024*1024] of char;
  OutBuf: array[0..16*1024*1024] of char;
  nRec, RecLen, RecLen2, nOutBuf, nOutFile: integer;

function isAscii: boolean;
var i: integer;
begin
   result := true;
   for i := 0 to RecLen-1 do begin
      case Buf[i] of
         #10,#13,#9:; // ok
         #32..#128:; // ok
         else begin
            result := false;
            break;
         end;
      end;
   end;
end;

var i: integer;
    fn: string;
    c: char;
begin
  // Insert user code here
  if (ParamStr(1) = '') then begin
     writeln('Usage: TapeDump filename');
     Halt(0);
  end;
  assign(f1,ParamStr(1));
  reset(f1,1);
  nRec := -1;
  nOutBuf :=0;
  nOutFile := -1;
  repeat
     inc(nRec);
     fp := FilePos(f1);
     if fp = FileSize(f1) then begin
        writeln('*** End of Tape ***');
        break;
     end;
     writeln('Read Record ',nRec, ' at FilePos ',fp);
     BlockRead(f1, Buf, 4, NumRead);
     if (NumRead <> 4) then begin
        writeln('Record ', nRec, ' ERROR: Expected RecLen (4 bytes) but readed only ', NumRead, ' bytes');
        break;
     end;
     RecLen := ord(Buf[0]) + ord(Buf[1]) * 256 + ord(Buf[2]) * 65536;
     if (Buf[3] = #0) and (RecLen = 0) then begin
        writeln('0x00000000 for tape mark');
        if nOutBuf > 0 then begin
           Str(nOutFile, fn); fn := 'File_' + fn;
           if nOutBuf > 12 then begin
              for i := 6 to 6+6 do begin
                 c := UpCase(OutBuf[i]);
                 if (c = '$') or (c = '.') or
                    ((c >= '0') and ( c <= '9')) or
                    ((c >= 'A') and ( c <= 'Z')) then begin
                    if i = 6 then fn := fn + '_';
                    fn := fn + c;
                 end else begin
                    break;
                 end
              end;
           end;
           Assign(f2, fn);
           rewrite(f2, 1);
           BlockWrite(f2, OutBuf, nOutBuf, NumWritten);
           closefile(f2);
           writeln('Dump ',fn, ', Len ', nOutBuf);
           inc(nOutFile);
           nOutBuf := 0;
        end;
        continue;
     end else if (Buf[3] = #255) and (Buf[2] = #255) then begin
        if (Buf[1] > #127) then begin
           writeln('half gap during reverse reads (with error indicators)');
        end else begin
           writeln('half gap during reverse reads');
        end;
        continue;
     end else if (Buf[3] = #255) and (Buf[2] = #254) then begin
        writeln('0xfffeffff for half gap during forward reads');
        continue;
     end else if (Buf[3] = #255) and (Buf[2] = #255) and (Buf[1] = #255) and (Buf[0] = #255) then begin
        writeln('0xffffffff for end of medium (EOM)');
        continue;
     end else if (Buf[3] = #255) and (Buf[2] = #255) and (Buf[1] = #255) and (Buf[0] = #254) then begin
        writeln('0xfffffffe for of erase gap marker');
        continue;
     end;
     writeln('Record ', nRec, ', RecLen ', RecLen, ' bytes, Error? ', ord(Buf[3]));

     BlockRead(f1, Buf, RecLen, NumRead);
     if (NumRead <> RecLen) then begin
        writeln('Record ', nRec, ' ERROR: Expected ',RecLen,' bytes nut readed only ', NumRead, ' bytes');
        break;
     end;

     if IsAscii then begin
        for i := 0 to RecLen-1 do write(Buf[i]);
        writeln;
     end else begin
        if nOutFile < 0 then nOutFile := 0;
        writeln('Binary Record');
     end;
     if nOutFile >= 0 then begin
        for i := 0 to RecLen-1 do begin
           OutBuf[nOutBuf] := Buf[i];
           inc(nOutBuf);
           c := Buf[i];
           if (c < ' ') or (c > #127) then c := '.';
           write(c);
        end;
        writeln;
     end;
     BlockRead(f1, Buf, 4, NumRead); // read copy of record length at end of data
     if (NumRead <> 4) then begin
        writeln('Record ', nRec, ' ERROR: Expected RecLen (4 bytes) but readed only ', NumRead, ' bytes');
        break;
     end;
     RecLen2 := ord(Buf[0]) + ord(Buf[1]) * 256 + ord(Buf[2]) * 65536;
     if RecLen <> RecLen2 then begin
        writeln('Record ', nRec, ' ERROR: RecLen (',RecLen,') <> RecLen2 (',RecLen2,')');
        break;
     end;
  until false;
  close(f1);
end.