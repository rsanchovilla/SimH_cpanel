//SMPAS    JOB  CLASS=A,MSGLEVEL=(1,1),REGION=512K
//RUN1     EXEC PASCAL,PARM.COMPILE='COMPILATION OPTION LIST'
//COMPILE.INPUT DD *
PROGRAM Hello (Output) ;

BEGIN
  WRITELN(' Hello World!');
END.

//RUN2     EXEC PASCAL,PARM.COMPILE='COMPILATION OPTION LIST',GOTIME=30
//COMPILE.INPUT DD *
PROGRAM fib_demo(OUTPUT) ;
TYPE pos_int = 0..30 ;
VAR  i    : pos_int ;
   time  : INTEGER ;
 FUNCTION fibonacci(j :pos_int) : INTEGER ;
 (*To evaluate fibonacci # j, for j >= 0,
                       subject to integer overflow*)
  BEGIN
  IF j = 0 THEN fibonacci := 0
  ELSE IF j = 1 THEN fibonacci := 1
    ELSE fibonacci := fibonacci(j-1) + fibonacci(j-2) ;
  END ;
 BEGIN (*fib_demo*)
 FOR i := 10 TO 25 DO
  BEGIN  time := CLOCK(0) ;
  WRITELN(' Fibonacci # ', i:3, ' is :', fibonacci(i):6,
  '  (Compute time =', CLOCK(0)-time:5, ' Milli Sec.)') ;
  END ;
 END.
//
