ASMB,R,L,B
      NAM ECUTE,7 
      ENT ECUTE 
      EXT .PRAM 
      SPC 2 
*     THIS IS AN ALGOL CALLABLE PROCEDURE DESIGNED
*     TO PERFORM THE MOVE EXECUTION REQUIRED BY THE MINI-TECH 
*     CHESS GAME. 
      SPC 1 
*     CALLING SEQUENCE: 
*     DEFINE AN EXTERNAL PROCEDURE
*       PROCEDURE ECUTE(A,B,C,D,E,F,G); INTEGER E, F, G, H; 
*         INTEGER ARRAY A, B, C, D; CODE; 
      SPC 1 
*     THE CALL: 
*       ECUTE(MV,LM,B,PCVAL,DEPTH,F,BW,MATERIAL); 
      SPC 2 
ECUTE NOP           ENTRY/EXIT
      JSB .PRAM 
      OCT 20000 
      OCT 0 
      SPC 1 
MV    OCT 0 
LM    OCT 0 
B     OCT 0 
PCVAL OCT 0 
DPT   OCT 0 
F     OCT 0 
BW    OCT 0 
MATL  OCT 0 
      SPC 1 
      LDA MV
      ADA =D3 
      LDA 0,I 
      STA MV
      LDA LM
      ADA =D3 
      LDA 0,I 
      STA LM
      LDA B 
      ADA =D3 
      LDA 0,I 
      STA B 
      LDA PCVAL 
      ADA =D3 
      LDA 0,I 
      STA PCVAL 
      SPC 1 
      DLD MV,I      MOVE THE CURRENT MOVE 
      DST LM,I      TO MASK POSITION. 
      AND =B177     MASK FOR DESTINATION
      STA THERE     SQUARE PIECE MOVED TO.
      ADA B         ADD ARRAY ADDRESS.
      STA DST       SAVE IT.
      ISZ DPT,I     INCREMENT THE DEPTH.
      SPC 1 
      LDA MV,I      GET THE MOVE WORD BACK. 
*      RRR 7         POSITION FOR SOURCE SQUARE
      OCT 101107
      AND =B177     MASK
      STA HERE      SQUARE PIECE MOVED FROM.
      ADA B         ADD ARRAY ADDRESS.
      STA ORG       SAVE IT.
      SPC 1 
      LDB LM        GET THE LAST MOVE WORD
      INB 
      STB T.ADD     SAVE THIS ADDRESS IN WORK AREA
      LDA 1,I       GET THE SECOND WORD.
      AND =B17      MASK FOR CAPTURE INGO.
      STA CAPT      SAVE IT.
      SZA,RSS       ANY CAPTURE ON THIS MOVE? 
      JMP NOCAP     NO, GO TO UDATE.
      SPC 1 
      RAR 
      SLA,RSS 
      JMP NOTN      NO, IT ISN'T. 
      SPC 1 
      LDA DST,I     GET THE PIECE CAPTURED. 
      STA PIECE     SAVE IT.
      LDA ORG,I     MOVE THE PIECE
      STA DST,I     FROM ORG TO DST.
      LDA CAPT      GET THE CAPTURE INFO
      SLA,RSS       EN PASSANT? 
      JMP UPD1      NO, CONTINUE
      SPC 1 
      LDA F,I       EN PASSANT CAPTURE
      CMA,INA 
      STA PIECE     IF PIECE IS 
      LDB DST         NEGATIVE, 
      SSA,RSS       IF POSITIVE,
      ADB =D-10       SUBTRACT 10.
      SSA             THEN
      ADB =D10        ADD 10 TO POSITION
      CLA           SET THE POSITION
      STA 1,I       TO ZERO, REMOVING THE PIECE.
      SPC 1 
UPD1  LDA PIECE     GET THE REMOVED PIECE 
      ADA =D6       ADJUST FOR POSITIONAL VALUE 
      ADA PCVAL     ADD ARRAY START.
      LDA 0,I       PIECE VALUE 
      LDB BW,I      CHECK 
      SSB,RSS 
      CMA,INA 
      ADA MATL,I    ADD THE OLD MATERIAL
      STA MATL,I    UPDATE MATERIAL.
      SPC 1 
NOTN  LDA CAPT      NO NORMAL CAPTURE.
      RAR,RAR 
      SLA,RSS 
      JMP NOPRO     NO PROMOTION
      SPC 1 
*     PAWN PROMOTION. 
      SPC 1 
      LDA MV,I
      RAL,RAL 
      AND =B3 
      ADA =D2 
      LDB F,I 
      SSB 
      CMA,INA 
      STA DST,I 
      SPC 1 
      LDA =D100     GET VALUE 
      LDB F,I       WHOSE MOVE? 
      SSB,RSS       IF WHITE, 
      CMA,INA         THEN SUBTRACT.
      SPC 1 
      LDB DST,I     GET THE PIECE 
      ADB =D6       ADD 6 
      ADB PCVAL     FROM STARTING ADDRESS.
      LDB 1,I       GET MATERIAL VALUE. 
      ADA 1         ADD TO PROMOTION. 
      LDB BW,I      GET WHITE/BLACK INDICATOR 
      SSB,RSS       IF WHITE, 
      CMA,INA         THEN SUBTRACT.
      ADA MATL,I
      STA MATL,I
      SPC 1 
NOPRO LDA CAPT
      AND =B10
      SZA,RSS 
      JMP NOCAS 
      SPC 1 
*     CASTLE PROCESSING 
      SPC 1 
      LDA ORG,I 
      STA DST,I 
      LDA HERE
      CMA,INA 
      ADA THERE 
      SSA 
      JMP QSIDE 
      SPC 1 
*     KING SIDE CASTLE PROCESSING 
      SPC 1 
      LDB ORG 
      ADB =D3 
      LDA 1,I 
      ADB =D-2
      STA 1,I 
      ADB =D2 
      CLA 
      STA 1,I 
      JMP NOCAS 
      SPC 1 
*     QUEENSIDE CASTLING
      SPC 1 
QSIDE LDB ORG 
      ADB =D-4
      LDA 1,I 
      ADB =D3 
      STA 1,I 
      ADB =D-3
      CLA 
      STA 1,I 
      JMP NOCAS 
      SPC 1 
NOCAP LDA ORG,I 
      STA DST,I 
NOCAS CLA 
      STA ORG,I 
      LDA F,I 
      CMA,INA 
      STA F,I 
      SSA 
      JMP WCAS
      SPC 1 
*     BLACK CASTLE'S
      SPC 1 
      LDA T.ADD,I 
      ALF,ALF 
      ALF 
      AND =B3 
      LDB =D-3
      ADA 1 
      SSA,RSS 
      JMP DONE
      SPC 1 
      LDA B 
      ADA =D95
      LDA 0,I 
      CPA =D-6
      JMP RCHK
      LDA T.ADD,I 
      IOR =B60
      STA T.ADD,I 
      JMP DONE
      SPC 1 
RCHK  LDA B 
      ADA =D91
      LDA 0,I 
      CPA =D-4
      JMP NXT 
      LDA T.ADD,I 
      IOR =B40
      STA T.ADD,I 
      JMP DONE
      SPC 1 
WCAS  LDA T.ADD,I   WHITE CASTLING RIGHTS 
      ALF,RAL 
      ALF,RAL 
      AND =B3 
      LDB =D-3
      ADA 1 
      SSA,RSS 
      JMP DONE
      LDA B 
      ADA =D25
      LDA 0,I 
      CPA =D6 
      JMP WQS 
      LDA T.ADD,I 
      IOR =B300 
      STA T.ADD,I 
      JMP DONE
      SPC 1 
*     WHITE CASTLES 
      SPC 1 
WQS   LDA B 
      ADA =D21
      LDA 0,I 
      CPA =D4 
      JMP DONE
      LDA T.ADD,I   NO, ROOK NOT HERE.
      IOR =B200     SET "ROOK GONE" BIT.
      STA T.ADD,I 
      SPC 1 
*     WHITE KINGS SIDE. 
      SPC 1 
      LDA B         CHECK THE KING'S
      ADA =D28        ROOK SQUARE 
      LDA 0,I       GET THE PIECE IF ANY
      CPA =D4       IS IT A ROOK? 
      JMP DONE
      SPC 1 
      LDA T.ADD,I   NO, ROOK IS GONE. 
      IOR =B100     SET "ROOK GONE" BIT.
      STA T.ADD,I 
      JMP DONE
      SPC 1 
NXT   LDA B 
      ADA =D98
      LDA 0,I 
      CPA =D-4
      JMP DONE
      LDA T.ADD,I 
      IOR =B20
      STA T.ADD,I 
DONE  JMP ECUTE,I 
      SKP 
*     LOCAL CONSTANTS AND STORAGE 
      SPC 1 
HERE  OCT 0 
THERE OCT 0 
T.ADD OCT 0 
ORG   OCT 0 
DST   OCT 0 
CAPT  OCT 0 
PIECE OCT 0 
      SPC 2 
      END 
