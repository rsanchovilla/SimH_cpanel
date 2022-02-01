* $$JOB VERPLI01,,,BG
// JOB VERPLI01
// OPTION LINK
// EXEC PL/I
* PROCESS STMT
 /*PL/I SAMPLE PROGRAM FOR DOS/TOS */
 PL1: PROCEDURE OPTIONS (MAIN);
           DECLARE  ( X, INITVALUE, TERMVALUE, STEP, A(5), B(2) )     
                    FLOAT BINARY (21) ;
 /********************************************************************
  
    THIS PROGRAM COMPUTES A SMALL TABLE CONTAINING A FEW MATH FUNCTIONS

    INPUT DATA.. INITVALUE - START POINT OF TABLE          
                 TERMVALUE   - TERMVALUE POINT TABLE
                 STEP  - STEP WIDTH IN TABLE

    THE ABSOLUTE VALUES OF INITVALUE AND TERMVALUE MAY NOT BE GREATER
    THAN  999.999, THE ABSOLUTE VALUE OF STEP MAY NOT BE LESS THAN 
    0.001.

    BASED ON THE IBM SUPPLIED SAMPLE PL/I INSTALLATION VERIFICATION
    PROGRAM, BUT RE-CODED TO USE MODERN STRUCTURED PROGRAMMING
    TECHNIQUES.  

 ******************************************************************* */

           GET EDIT (INITVALUE, TERMVALUE, STEP)(3 F(8,3)) ;     

           PUT EDIT ('X     SQRT(X**2+1)      X**2','X**3','SIN(X)
  COS(X)      SQRT(X)     SQRT(X**3)') (X(4),A(41),A(14),A(61)) ;

           IF (ABS(INITVALUE) < 1000.) & (ABS(STEP) >= .001)
              THEN DO X=INITVALUE TO TERMVALUE+STEP/1E3 BY STEP ;
                    A(2)=X*X ;           
                    A(1)=SQRT(A(2)+1);  
                    A(3)=A(2)*X  ;      
                    A(4)=SIN(X) ;       
                    A(5)=COS(X) ;       
                    PUT EDIT (X,A) (F(8,3),F(13,3),2 E(16,5),2 F(13,6));
                    IF X < 0 
                       THEN PUT EDIT ('--','--') (X(7), A(15), A(19)) ;
                       ELSE BEGIN ;
                          B(1)=SQRT(X)    ;    
                          B(2)=SQRT(A(3)) ;    
                          PUT EDIT (B,' ') (F(12,4), E(16,5),A(13)) ;
                       END ;
                 END  /* DO LOOP */ ; 
              ELSE BEGIN ;
                 PUT EDIT ('INPUT DATA ERROR') (A(16)) ;
                 STOP /* END ON WRONG INPUT */ ;
                 END ;

           PUT SKIP EDIT (' NORMAL END')(A);
           RETURN;/* NORMAL END */ ;        
           END  /* TABLE */  ;              
/*
// EXEC LNKEDT
// EXEC
    0.       3.      .1     
/*
/&
* $$EOJ