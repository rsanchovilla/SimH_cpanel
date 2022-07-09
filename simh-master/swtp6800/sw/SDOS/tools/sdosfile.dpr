program sdosfile;
{$APPTYPE CONSOLE}
{$I-}
uses
  SysUtils;

type
    tbuf=array[0..65535] of char;
    tfilebuf=array[0..1024*1024] of char;

var DirListing, s19extract, CreateDiskImgDir, ConvertDiskImgDir,
    ConvertToSDOSLoad, AppendSDOSLoadFile: boolean;
    SDOSRemoveHeader: integer;
    sr: TSearchRec;
    fdisk: file;
    fname, fnameSpec, fnameToAppend: string;
    NBPS, NSPT, NCYL: integer; // physical disk geometry
    buf, Cluster, ClusterList: tbuf;
    dirbuf, filebuf: tfilebuf;
    NSPC, SPIRAL, INTERLEAVE: integer; // logical disk geometry
    MAP: array[0..255] of integer;
    DIRLSN, DIRHLCN: integer;
    dir: array[0..1023] of record
       fname: string;
       hlcn, nclusters, fsize: integer;
    end;
    ndir, fdisksize: integer;

procedure WriteFile(fname: string; fsize: integer; var filebuf: tfilebuf);
var f: file;
begin
   AssignFile(f, fname);
   ReWrite(f,1);
   BlockWrite(f, filebuf, fsize);
   CloseFile(f);
end;

procedure DiskReadTS(var buf: tbuf; tr, sec: integer); // read track/sector=0/0
var pos: integer;
begin
   pos := (sec + tr * NSPT) * NBPS;
   FillChar(buf, NBPS, 0);
   if (pos >= fdisksize) then exit;
   Seek(fdisk, pos);
   BlockRead(fdisk, buf, NBPS);
end;

// read logical sector number
procedure ReadLogicalSector(var buf: tbuf; LSN: integer);
var cyl, tr, sec: integer;
begin
   cyl:= (LSN div NSPT); // NTPC=Number of tracks per cyl = 1
   tr := (LSN - cyl*NSPT) div NSPT;
   sec:= ((cyl * SPIRAL) + MAP[LSN mod NSPT]) mod NSPT;
   DiskReadTS(buf, cyl, sec);
end;

// read logical cluster number
procedure ReadLogicalCluster(var Cluster: tbuf; LCN: integer);
var lsn, p, i, j: integer;
begin
  FillChar(Cluster, sizeof(Cluster), 0);
  lsn :=LCN * NSPC; // calc logical sector number
  if lsn >= NSPT*NCYL then begin
     writeln;
     writeln(' ... skip read to invalid LSN ',lsn,' ...');
     exit;
  end;
  p:=0;
  for i:=0 to NSPC-1 do begin
     ReadLogicalSector(buf, lsn);
     for j:=0 to NBPS-1 do begin
        Cluster[p]:=buf[j];
        inc(p);
     end;
     inc(lsn);
  end;
end;

procedure ReadFileClusters(var filebuf: tfilebuf; hlcn, nclusters: integer; var p:integer);
var i,j: integer;
    lcn: integer;
begin
  p:=0;
  FillChar(filebuf, sizeof(filebuf), 0);
  ReadLogicalCluster(ClusterList, hlcn);
  if (hlcn <> ord(ClusterList[0])*256+ord(ClusterList[1])) then begin
     writeln('first cluster pointer does not point to HeadCluster');
     exit
  end;
  // travel cluster list to read file data
  for i:=0 to 32767 do begin
     lcn:=ord(ClusterList[2 + i*2])*256+ord(ClusterList[3 + i*2]);
     if (lcn = $FFFF) then begin
        if (nclusters < 0) then break;
        FillChar(Cluster, sizeof(Cluster), 0);
     end else begin
        ReadLogicalCluster(Cluster, lcn);
     end;
     for j:=0 to NSPC*NBPS-1 do begin
        filebuf[p]:=Cluster[j];
        inc(p);
     end;
     if (lcn <> $FFFF) and (nclusters > 0) then begin
        dec(nclusters);
        if nclusters=0 then break;
     end;
  end;
end;

// return str with hex buf[n]
function sbuf(n: integer): string;
begin
  result:=IntToHex(ord(buf[n]), 2);
end;

procedure ReadBootSector;
var i,j,k: integer;
    used: boolean;
    s: string;
begin
  DiskReadTS(buf, 0,0); // read track/sector=0/0
  writeln('SDOS FileSystem version: ' + sbuf($10));
  NSPC := ord(buf[$11]); // number of sectors per cluster
  writeln('     NSPC     : ' + IntToHex(NSPC, 2));
  if not DirListing then begin
     writeln('     MINALLOC : ' + sbuf($12) + ' ' + sbuf($13));
     writeln('     MIDALLOC : ' + sbuf($14) + ' ' + sbuf($15));
  end;
  SPIRAL     := ord(buf[$16]); // spiraling
  INTERLEAVE := ord(buf[$17]); // interleave spacing
  writeln('     MAPALG   : ' + IntToHex(SPIRAL, 2) + ' ' + IntToHex(INTERLEAVE, 2));
  writeln('     Init Date: ' + sbuf($18) + '/' + sbuf($19) + '/' + sbuf($1a));
  DIRLSN := ord(buf[$1b]) *256*256 + ord(buf[$1c])*256 + ord(buf[$1d]); // directory.sys logical sectorn number
  if not DirListing then begin
     writeln('     DIRLSN   : ' + IntToHex(DIRLSN, 6));
  end;
  s:='';
  for i:=32 to 63 do s:=s+buf[i];
  writeln('     DISKID   : ' + s);
  // create disk map: Logical to Physical Sector
  MAP[0]:=0;
  k:=INTERLEAVE;
  for i:=1 to NSPT-1 do begin
     // check if k is already used
     repeat
        used:=false;
        for j:=0 to i-1 do if k=MAP[j] then used:=true;
        if not used then break;
        k:=(k+1) mod NSPT;
     until false;
     MAP[i]:=k;
     k:=(k+INTERLEAVE) mod NSPT;
  end;
  if not DirListing then begin
     write('     MAP      : ');
     for i:=0 to NSPT-1 do begin
        if (i>0) then write(',');
        write(IntToStr(MAP[i]));
     end;
     writeln;
  end; 
end;

procedure ReadDir;
var i, p, plen: integer;
    s: string;
    hlcn,hlsn: integer;
    hcsic, nclusters, prot, fsize: integer;
    label nxtentry;
begin
  ReadLogicalSector(buf, DIRLSN); // directory.sys directory entry logical sectorn number
  s:='';
  for i:=0 to 15 do s:=s+buf[i];
  if s <> 'DIRECTORY.SYS   ' then begin
     writeln('DIRECTORY.SYS entry not found');
     exit
  end;
  hlcn:=ord(buf[16])*256+ord(buf[17]); // head logical cluster number taken form dir
  ReadFileClusters(dirbuf, hlcn, -1, plen); // read directory.sys file
  writeln('Directory: ');
  if DirListing then begin
     writeln('FILENAME-------- NCLUST -----FSIZE');
  end else begin
     writeln('FILENAME-------- NCLUST -----FSIZE --HLCN');
  end;
  ndir:=0;
  p:=0;
  repeat
     if (dirbuf[p]=chr(255)) then goto nxtentry;
     s:='';
     for i:=0 to 15 do s:=s+dirbuf[p+i]; // filename
     hlcn:=ord(dirbuf[p+16])*256+ord(dirbuf[p+17]); // head logical cluster for file
     hcsic:=ord(dirbuf[p+18]);
     nclusters:=ord(dirbuf[p+19])*256+ord(dirbuf[p+20]); // number of clusters alloc
     if nclusters=0 then goto nxtentry;
     fsize:=ord(dirbuf[p+21])*256*256*256+ord(dirbuf[p+22])*256*256+ord(dirbuf[p+23])*256+ord(dirbuf[p+24]);
     prot:=ord(dirbuf[p+25]);
     s:=trim(s);
     // avoid duplicate filenames
     if ndir > 0 then begin
        for i:=0 to ndir-1 do begin
           if dir[i].fname=s then begin
              goto nxtentry;
           end;
        end;
     end;
     if DirListing then begin
        writeln(s:16,' ',nclusters:6,' ',fsize:10);
     end else begin
        writeln(s:16,' ',nclusters:6,' ',fsize:10,' ',hlcn:6);
     end;
     if (s='DIRECTORY.SYS') or (s='BADCLUSTERS.SYS') or (s='DISKMAP.SYS') then goto nxtentry;
     if nclusters < 2 then goto nxtentry; // ignore empty file
     // add dir entry
     dir[ndir].fname:=s;
     dir[ndir].hlcn:=hlcn;
     dir[ndir].nclusters:=nclusters-1; // number of data only clusters
     dir[ndir].fsize:=fsize;
     inc(ndir);
   nxtentry:
     p:=p+32;
     if p>=plen then break;
  until false;
end;

procedure DumpFiles;
var i, n, nfile, lcn, plen: integer;
    dname: string;
begin
   if ndir=0 then begin
      writeln('No files to dump');
      exit;
   end else if CreateDiskImgDir then begin
      dname := ExtractFileName(fname); // get name of current disk image being dumped
      dname := trim(Copy(dname, 1, length(dname) - length(ExtractFileExt(dname)))); // remove extension if any
      MkDir(dname);
      write('Dumping ',ndir,' files to folder ', dname);
      dname := dname + '\';
   end else begin
      writeln('Dumping ',ndir,' files ...');
      dname := '';
   end;
   for nfile:=0 to ndir-1 do begin
      writeln('Dump file: ',dir[nfile].fname);
      ReadLogicalCluster(ClusterList, dir[nfile].hlcn);
      n:=dir[nfile].nclusters;
      write('Data Clusters used: ');
      for i:=0 to 32767 do begin
         lcn:=ord(ClusterList[2 + i*2])*256+ord(ClusterList[3 + i*2]);
         if lcn = $FFFF then begin
            write('NA ');
         end else begin
            write(lcn,' ');
            dec(n);
            if n=0 then break;
         end;
      end;
      writeln('(',dir[nfile].nclusters, ' data clusters used)');
      ReadFileClusters(filebuf, dir[nfile].hlcn, dir[nfile].nclusters, plen); // read file
      WriteFile(dname + trim(dir[nfile].fname), dir[nfile].fsize, filebuf);
   end;
end;

procedure ConvertS19ToSDOS(fnameIn, fnameOut: string);
var fIn: text;
    fOut: file;
    sLin, sLin0: string;
    DataAddr, DataLen, len, addr: integer;
    blin: array[0..65536] of byte;

    function GetByte(var sLin: string): integer;
    var s: string;
    begin
        GetByte:=0;
        s:=Copy(sLin,1,2);
        Delete(sLin,1,2);
        if s='' then begin
            writeln('Missing an hex value in: ', sLin0);
            exit;
        end else if length(s)<2 then begin
            writeln('Missing an hex digit in: ', sLin0);
            exit;
        end;
        if (((s[1] >= '0') and (s[1] <= '9')) or ((s[1] >= 'A') and (s[1] <= 'F'))) and
           (((s[2] >= '0') and (s[2] <= '9')) or ((s[2] >= 'A') and (s[2] <= 'F'))) then begin
           // hex num ok
           GetByte:=StrToIntDef('$'+s, 0);
        end else begin
            writeln('Invalid hex digit in: ', sLin0);
            exit;
        end;
    end;

    procedure WriteHeader(id, w1, w2: integer);
    var bhdr: array[0..5] of byte;
    begin
        if SDOSRemoveHeader>0 then begin
           dec(SDOSRemoveHeader);
           exit;
        end;
        bhdr[0]:=id;
        bhdr[1]:=w1 div 256; bhdr[2]:=(w1 and 255);
        bhdr[3]:=w2 div 256; bhdr[4]:=(w2 and 255);
        BlockWrite(fOut, bhdr, 5); // SDOS record header
    end;

    procedure FlushData;
    begin
        WriteHeader(2, DataAddr, DataLen);
        BlockWrite(fOut, blin, DataLen); // SDOS record body
    end;

begin
    // fnameIn = Motorola S19 text format
    // fnameOut = SDOS binary format
    AssignFile(fIn, fnameIn);
    Reset(fIn);
    AssignFile(fOut, fnameOut);
    Rewrite(fOut,1);
    DataAddr:=-1;
    WriteHeader(1,0,$FFFF);
    while not eof(fIn) do begin
        readln(fIn, sLin);
        sLin0:=sLin;
        sLin:=trim(sLin);
        if (Copy(sLin, 1, 2) <> 'S1') then continue;
        Delete(sLin,1,2);
        len:=GetByte(sLin)-3; // num of data bytes (discard chksum and addr)
        if len < 1 then begin
           writeln('Line without data: ', sLin0);
           continue;
        end;
        addr:=GetByte(sLin);
        addr:=addr*256+GetByte(sLin);
        if DataAddr < 0 then begin
           DataAddr := addr;
           DataLen := 0;
        end else begin
           if addr = DataAddr + DataLen then begin
              // on track
           end else begin
              FlushData;
              DataAddr := addr;
              DataLen := 0;
           end;
        end;
        while len > 0 do begin
           blin[DataLen]:=GetByte(sLin);
           inc(DataLen);
           dec(len);
        end;
        len:=GetByte(sLin); // get and discard checksum
    end;
    if DataAddr < 0 then begin
        writeln('No data in S19 file');
    end else begin
       FlushData;
    end;
    CloseFile(fIn);
    CloseFile(fOut);
end;

procedure ConvertToS19(fnameIn, fnameOut: string);
var fIn: file;
    fOut: text;
    fsize, InByte: integer;
    OutAddr: integer;
    blin: array[0..32] of integer;

    function getByte: integer;
    begin
       if InByte >= fsize then begin
          result:=0;
          exit;
       end;
       result := ord(filebuf[InByte]);
       inc(InByte);
    end;

    function getWord: integer;
    var nH, nL: integer;
    begin
       nH := getByte;
       nL := getByte;
       result := nH*256 + nL;
    end;

    procedure flushBytes;
    var crc,i,n: integer;
        s: string;
    begin
       if OutAddr < 0 then exit;
       n:=blin[0];
       inc(blin[0]);
       crc := 0;
       for i:=0 to n do begin
          crc:=crc + blin[i];
       end;
       crc := (not crc) and $FF;
       blin[n+1]:=crc;
       s:='S1';
       for i:=0 to n+1 do begin
          s:=s+ IntTohex(blin[i], 2);
       end;
       writeln(s);
       writeln(fOut, s);
       FillChar(blin, sizeof(blin), 0);
       OutAddr := -1;
    end;

    procedure putByte(addr: integer; b: integer);
    var n: integer; 
    begin
       if (OutAddr >= 0) then begin
          if (addr <> OutAddr+1) then begin
             flushBytes;
          end else if ((addr and 15) = 0) then begin
             flushBytes;
          end;
       end;
       if OutAddr < 0 then begin
          blin[0]:=2; // count
          blin[1]:=addr shr 8;
          blin[2]:=addr and $FF;
       end;
       n:=blin[0]+1;
       blin[n]:=b;
       blin[0]:=n;
       OutAddr := addr;
    end;

var c, w, n: integer;
    s: string;
begin
    // fnameIn = SDOS binary format
    // fnameOut = Motorola S19 text format
    AssignFile(fIn, fnameIn);
    Reset(fIn, 1);
    fsize:=FileSize(fIn);
    BlockRead(fIn, filebuf, fsize);
    CloseFile(fIn);
    AssignFile(fOut, fnameOut);
    Rewrite(fOut);
    writeln(fOut);
    InByte:=0;
    OutAddr:=-1;
    FillChar(blin, sizeof(blin), 0);
    c:= getByte; // Load record type 1 starts at byte 0 of file
    case c of
       1 : s:='6800';
       2 : s:='6809';
       3 : s:='6803';
       7 : s:='6303';
       11: s:='6811';
       else s:='??? (c='+IntTohex(c,2)+')';
    end;
    writeln('... cpu is ', s);
    w := getWord;
    writeln(fOut, 'Start Addr=',IntToHex(w,4));
    writeln(fOut);
    if (w <> ((not getWord) and $FFFF)) then begin
       writeln('... bad load record type 1');
    end;
    repeat
       if InByte >= fsize then break;
       c:=getByte;
       if c = 0 then begin
          // skip record
          n := getWord;
          while n>0 do begin
             if InByte >= fsize then begin
                writeln('... EOF in middle of skip record');
                break;
             end;
             getByte;
             dec(n);
          end;
          continue;
       end;
       if (c = 2) or (c = 3) then begin
          // data record
          w := getWord;
          n := getWord;
          while n>0 do begin
             if InByte >= fsize then begin
                writeln('... EOF in middle of data record');
                break;
             end;
             c:=getByte;
             dec(n);
             putByte(w, c);
             inc(w);
          end;
          continue;
       end;
       writeln('... Invalid load record type c=',IntToHex(c,2));
       break;
    until false;
    flushBytes;
    writeln(fOut, 'S9');
    writeln('S9');
    CloseFile(fOut);
end;

procedure AppendLoadFile(fnameMain, fnameToAppend: string; nStartAddr: integer);
var f: file;
    fsize, InByte, Bufsize: integer;

    function getByte: integer;
    begin
       if InByte >= fsize then begin
          result:=0;
          exit;
       end;
       result := ord(filebuf[InByte]);
       inc(InByte);
    end;

    function getWord: integer;
    var nH, nL: integer;
    begin
       nH := getByte;
       nL := getByte;
       result := nH*256 + nL;
    end;

var c, w, n: integer;
    s: string;
begin
    // read file to be appended to into filebuf
    AssignFile(f, fnameMain);
    Reset(f, 1);
    fsize:=FileSize(f);
    BlockRead(f, filebuf, fsize);
    CloseFile(f);
    // remove type 3 records from filebuf
    InByte:=0;
    c:= getByte; // Load record type 1 starts at byte 0 of file
    w := getWord;
    if (w <> ((not getWord) and $FFFF)) then begin
       writeln('... bad load record type 1 in ', fname);
    end;
    // set new start address if any
    if (nStartAddr >= 0) then begin
       w := nStartAddr;
       filebuf[1] := char((w div 256) and 255);
       filebuf[2] := char(w and 255);
       w :=((not w) and $FFFF);
       filebuf[3] := char((w div 256) and 255);
       filebuf[4] := char(w and 255);
    end;
    repeat
       if InByte >= fsize then break;
       c:=getByte;
       if c = 0 then begin
          // skip record
          n := getWord;
          while n>0 do begin
             if InByte >= fsize then begin
                writeln('... EOF in middle of skip record');
                break;
             end;
             getByte;
             dec(n);
          end;
          continue;
       end;
       if (c = 2) or (c = 3) then begin
          // data record
          if (c = 3) then begin
             // convert type 3 record to type 2 record
             filebuf[InByte-1] := #2;
          end;
          w := getWord;
          n := getWord;
          while n>0 do begin
             if InByte >= fsize then begin
                writeln('... EOF in middle of data record');
                break;
             end;
             getByte;
             dec(n);
          end;
          continue;
       end;
       writeln('... Invalid load record type c=',IntToHex(c,2));
       break;
    until false;
    // read file to be append at end of filebuf
    AssignFile(f, fnameToAppend);
    Reset(f, 1);
    Bufsize:=FileSize(f);
    BlockRead(f, buf, Bufsize);
    CloseFile(f);
    // skip first 5 bytes of file to appen to skip its type 1 record
    for n:=5 to Bufsize-1 do begin
       filebuf[fsize + n - 5] := buf[n];
    end;
    // write resulting file
    AssignFile(f, fnameMain);
    Rewrite(f, 1);
    BlockWrite(f, filebuf, fsize+Bufsize-5);
    CloseFile(f);
    // print load segments
    AssignFile(f, fnameMain);
    Reset(f, 1);
    fsize:=FileSize(f);
    BlockRead(f, filebuf, fsize);
    CloseFile(f);
    // remove type 3 records from filebuf
    InByte:=0;
    c:= getByte; // Load record type 1 starts at byte 0 of file
    case c of
       1 : s:='6800';
       2 : s:='6809';
       3 : s:='6803';
       7 : s:='6303';
       11: s:='6811';
       else s:='??? (c='+IntTohex(c,2)+')';
    end;
    writeln('... cpu is ', s);
    w := getWord;
    if (w <> ((not getWord) and $FFFF)) then begin
       writeln('... bad load record type 1 in ', fname);
    end;
    writeln('... start address is $', IntToHex(w,4));
    // set new start address if any
    repeat
       if InByte >= fsize then break;
       c:=getByte;
       if c = 0 then begin
          // skip record
          n := getWord;
          writeln('... skip record 0 for ', n, ' bytes');
          while n>0 do begin
             if InByte >= fsize then begin
                writeln('... EOF in middle of skip record');
                break;
             end;
             getByte;
             dec(n);
          end;
          continue;
       end;
       if (c = 2) or (c = 3) then begin
          // data record
          w := getWord;
          n := getWord;
          writeln('... data record ',c,' at $',IntToHex(w,4),' for $',IntToHex(n,4), ' bytes');
          while n>0 do begin
             if InByte >= fsize then begin
                writeln('... EOF in middle of data record');
                break;
             end;
             getByte;
             dec(n);
          end;
          continue;
       end;
       writeln('... Invalid load record type c=',IntToHex(c,2));
       break;
    until false;
    writeln('End of SDOS load file');
end;


procedure ConvertDiskImg;
var fOut: file;
    s: string;
    lsn, n, nlsn: integer;

begin
   nlsn := 2*35*18;
   FillChar(filebuf, nlsn*128, 0);
   for lsn := 0 to nlsn-1 do begin
      ReadLogicalSector(buf, lsn);
      if (lsn = 0) then begin
         buf[$16] := #0; buf[$17] := #1;
      end;
      for n:=0 to 127 do begin
         filebuf[lsn*128 + n]:=buf[n];
      end;
   end;
   s := fname;
   if ExtractFileExt(fname) = '.DSK'  then s:=Copy(s, 1, length(s)-4);
   s := s+'.CONV.DSK';
   AssignFile(fOut, s);
   Rewrite(fOut, 1);
   BlockWrite(fOut, filebuf, nlsn*128);
   CloseFile(fOut);
end;


var sw: string;
    n: integer;
begin
  DirListing:=false;
  s19extract:=false; ConvertToSDOSLoad:=false;
  CreateDiskImgDir:=false;
  ConvertDiskImgDir:=false; AppendSDOSLoadFile := false; 
  fnameSpec:=ParamStr(1);
  sw:=UpperCase(fnameSpec);
  if fnameSpec = '' then begin
      writeln('This program expects Software Dynamics SDOS a .DSK disk image ');
      writeln('file/filespec (can have wilcards as *.dsk)');
      writeln('  sdosfile diskfilespec -> will extract files to current directory');
      writeln('  sdosfile -d diskimagespec -> create a folder with same name as');
      writeln('         disk image, and extract files to it');
      writeln('  sdosfile -l diskimagespec -> do not extract files, just list ');
      writeln('         directory of disk image');
      writeln('  sdosfile -s extractedfilespec -> convert SDOS extracted file(s)');
      writeln('         from SDOS load format to Motorola S19 format. ');
      writeln('         Appends .S19 extension');
      writeln('  sdosfile -x|x1|x2 s19filespec -> convert Motorola S19 format file(s)');
      writeln('         to SDOS load format, Appends .680 extension.');
      writeln('         Use -x1 remove cpu type reg, use -x2 to remove also 1st reg hdr');
      writeln('  sdosfile -a loadfile1 loadfile2 [nnnn] -> append SDOS load file loadfile2');
      writeln('         at the end of loadfile1. Converts 03 record at end of loadfile1 to ');
      writeln('         type 02, removes 01 record at start of loadfile2. If nnnn is');
      writeln('         given, set it as start address (nnnn = 4 hex digits number).');
      writeln('         File loadfile1 is modified');
      halt;

  // -S extractedfilespec -> convert filespec file(s) (in SDOS binary format) to Motorola S19 format
  // -C diskfilespec -> create a new disk image (.CONV.DSK) with mapalg=0001, 2sides x 35tr x 18 sec x 128 bytes
  // example: sdosfile image.dsk -> extract files to current dir
  // example: sdosfile bas*.dsk -> extract files to current dir for all disk images
  // example: sdosfile -d *.dsk -> list all directoriy for all *.dsk files in current dir
  // example: sdosfile -s sdos*.680 -> convert extracted file(s) to S19 format (append .S19)
  // example: sdosfile -x sdos*.s19 -> convert S19 format to SDOS load format
  // example: sdosfile -c image.dsk -> convert disk image to mapalg=0001, 35tr x 18sec x 128b x 2sidex = 157,5 KB
  // example: sdosfile -a metam14.680 basic.680  -> appends basic.680 at end of metam14.680


  end;

  if (sw='-L') then begin
     fnameSpec:=ParamStr(2);
     DirListing:=true;
  end else if (sw='-S') then begin
     fnameSpec:=ParamStr(2);
     s19extract:=true;
  end else if (sw='-A') then begin
     fnameSpec:=ParamStr(2);
     fnameToAppend:=ParamStr(3);
     if (ParamStr(4) <> '') then n := StrToInt('$'+ParamStr(4))
                            else n := -1;
     AppendSDOSLoadFile:=true;
  end else if (sw='-X') or (sw='-X1') or (sw='-X2') then begin
     fnameSpec:=ParamStr(2);
     ConvertToSDOSLoad:=true;
     SDOSRemoveHeader:=0;
     if (sw='-X1') then SDOSRemoveHeader:=1;
     if (sw='-X2') then SDOSRemoveHeader:=2;
  end else if (sw='-D') then begin
     fnameSpec:=ParamStr(2);
     CreateDiskImgDir:=true;
  end else if (sw='-C') then begin
     fnameSpec:=ParamStr(2);
     ConvertDiskImgDir:=true;
  end;
  if fnameSpec='' then begin
     writeln('Missing SDOS disk image filename/filespec');
     exit;
  end;
  if FindFirst(fnameSpec, faArchive, sr) <> 0 then begin
     writeln('No files found');
     exit
  end;
  repeat
     fname:=ExtractFilePath(fnameSpec) + UpperCase(sr.Name);
     if s19extract then begin
        writeln('Convert to S19: ', fname);
        if ExtractFileExt(fname) = '.S19'  then begin
           writeln('... skipped');
           continue;
        end;
        ConvertToS19(fname, fname + '.S19');
        if FindNext(sr) <> 0 then break;
        continue;
     end else if ConvertToSDOSLoad then begin
        writeln('Convert S19 to SDOS Load: ', fname);
        if ExtractFileExt(fname) <> '.S19'  then begin
           writeln('... skipped');
           continue;
        end;
        ConvertS19ToSDOS(fname, fname + '.680');
        if FindNext(sr) <> 0 then break;
        continue;
     end else if AppendSDOSLoadFile then begin
        write('Append at end of ', fname, ' the file ', fnameToAppend );
        if (n >= 0) then write(' (start addr $',IntTohex(n,4), ')');
        writeln;
        AppendLoadFile(fname, fnameToAppend, n);
        if FindNext(sr) <> 0 then break;
        continue;
     end;
     if ExtractFileExt(fname) <> '.DSK'  then begin
        writeln('Only works for .DSK disk images');
        continue;
     end;
     writeln('Open disk image: ' + UpperCase(sr.Name));
     Assign(fdisk, fname);
     Reset(fdisk, 1);
     fdisksize:=FileSize(fdisk);
     if fdisksize = 256256 then begin
        NBPS := 128; // bytes per sector    // 8 inches diskette CHM collection
        NSPT := 26;  // sectors per track
        NCYL := 77;  // number of tracks
     end else if fdisksize = 509184 then begin
        NBPS := 256; // bytes per sector   // 5 inches diskette CHM collection
        NSPT := 26;  // sectors per track
        NCYL := 77;  // number of tracks
     end else if fdisksize = 161280 then begin
        NBPS := 128; // bytes per sector   // 5 inches diskette double side, single density
        NSPT := 18;  // sectors per track  // used on MF68 drive on swtpc 6800 real hw
        NCYL := 35*2;  // number of tracks
     end else if fdisksize = 5013504 then begin
        NBPS := 128; // bytes per sector   // 5MB hard disk image file
        NSPT := 64;  // sectors per track
        NCYL := 153*4;  // number of tracks
     end else begin
        NBPS := 128; // bytes per sector
        NSPT := 18;  // sectors per track
        NCYL := fdisksize div (NBPS * NSPT);  // number of tracks, normally 35 or 77
     end;
     ReadBootSector;
     if ConvertDiskImgDir then begin
        ConvertDiskImg;
     end else begin
        ReadDir;
        if not DirListing then begin
           DumpFiles;
        end;
     end;
     CloseFile(fdisk);
     if FindNext(sr) <> 0 then break;
     writeln;
     writeln;
   until false;
   FindClose(sr);
end.