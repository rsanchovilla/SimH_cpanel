//PASCAL JOB   98166,TEST,MSGLEVEL=1
//STEP   EXEC  ALGOFCLG,PARM.ALGOL=(ISO),PARM.GO=(DUMP)
//SYSIN  DD  *
@BEGIN@ @COMMENT@ TEST PROGRAM Q09
THIS PROGRAM GENERATES AND PRINTS THE FIRST TWENTY LINES OF PASCALS
TRIANGLE.THE K TH ELEMENT P%K,J< OF THE J TH LINE SHOULD BE EQUAL TO
THE SUM OF P%K-1,J-1< AND P%K,J-1< FOR K NOTEQUAL 0 AND K NOTEQUAL J.
P%0,J<#P%J,J<#1. THUS BY ADDING TWO BY TWO ALL ELEMENTS IN ONE LINE
PLACING EACH SUM BELOW AND BETWEEN THE TWO ELEMENTS THE NEXT LINE OF
PASCALS TRIANGLE COULD BE EXPANDED .,
@INTEGER@ L,K,N,I,M,POWERTEN.,
@INTEGER@@ARRAY@ A%/0..19/<.,
@BOOLEAN@ C.,
SYSACT%1,6,120<., SYSACT%1,8,62<.,
SYSACT%1,12,1<., SYSACT%1,2,56<.,
OUTSTRING %1,@%@PASCALS TRIANGLE@<@<.,
@FOR@L.#0@STEP@1@UNTIL@19@DO@
    @BEGIN@
    SYSACT%1,14,3<.,
    @IF@ L @LESS@ 19 @THEN@
                         SYSACT%1,2,58-3*L<.,
    A%/L/<.#1.,
    @FOR@K.#L-1@STEP@-1@UNTIL@1@DO@
    A%/K/<.#A%/K-1/<&A%/K/<.,
    @FOR@K.#0@STEP@1@UNTIL@L@DO@
         @BEGIN@
         C.#@TRUE@.,
         M.#A%/K/<.,
         @FOR@I.#5@STEP@-1@UNTIL@0@DO@
              @BEGIN@
              POWERTEN.#10@POWER@I.,
              N.#M@/@POWERTEN.,
              M.#M-N*POWERTEN.,
              @IF@N@EQUAL@0@THEN@
                   @BEGIN@
                   @IF@C@THEN@ OUTSYMBOL %1,@%@ @<@,1<
                   @ELSE@ OUTSYMBOL %1,@%@0@<@,1<.,
                   @END@
                   @ELSE@
                   @BEGIN@
                   C.#@FALSE@.,
                   OUTSYMBOL%1,@%@123456789@<@,N<.,
                   @END@
              @END@
         @END@
    @END@
@END@
/*
//