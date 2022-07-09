program sdosfile;
{$APPTYPE CONSOLE}
{$I-}
uses
  SysUtils;



var f1,f2: Text;
    s, s1, s2: string;
    n: integer;
begin
   AssignFile(f1, ParamStr(1));
   AssignFile(f2, 'TMP.TXT');
   Reset(f1);
   ReWrite(f2);
   s1 := ParamStr(2);
   s2 := ParamStr(3);
   while not eof(f1) do begin
      readln(f1, s);
      n:=Pos(s1, s);
      if (n>0) then begin
          s := Copy(s, 1, n-1) + s2 +
              Copy(s, n+length(s1), length(s));
      end;
      writeln(f2, s);
   end;
   CloseFile(f1);
   CloseFile(f2);

   AssignFile(f1, 'TMP.TXT');
   AssignFile(f2, ParamStr(1));
   Reset(f1);
   ReWrite(f2);
   while not eof(f1) do begin
      readln(f1, s);
      writeln(f2, s);
   end;
   CloseFile(f1);
   CloseFile(f2);
   DeleteFile('TMP.TXT');
end.