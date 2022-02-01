* $$JOB VERFOR01,,,BG
// JOB VERFOR01  IVP BASIC FORTRAN
// OPTION LINK
// EXEC FORTRAN
C     PRIME NUMBER PROBLEM                                              $4510001
  100 WRITE (3,8)                                                       $4510002
    8 FORMAT (52H FOLLOWING IS A LIST OF PRIME NUMBERS FROM 1 TO 1000/  $4510003
     /19X,1H1/19X,1H2/19X,1H3)                                          $4510004
  101 I=5                                                               $4510005
    3 A=I                                                               $4510006
  102 A=SQRT(A)                                                         $4510007
  103 J=A                                                               $4510008
  104 DO 1 K=3,J,2                                                      $4510009
  105 L=I/K                                                             $4510010
  106 IF(L*K-I)1,2,4                                                    $4510011
    1 CONTINUE                                                          $4510012
  107 WRITE (3,5)I                                                      $4510013
    5 FORMAT (I20)                                                      $4510014
    2 I=I+2                                                             $4510015
  108 IF(1000-I)7,4,3                                                   $4510016
    4 WRITE (3,9)                                                       $4510017
    9 FORMAT (14H PROGRAM ERROR)                                        $4510018
    7 WRITE (3,6)                                                       $4510019
    6 FORMAT (31H THIS IS THE END OF THE PROGRAM)                       $4510020
  109 STOP                                                              $4510021
      END                                                               $4510022
/* 
// EXEC LNKEDT
// EXEC
/*
/&
* $$EOJ