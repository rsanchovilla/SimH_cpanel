// JOB FIXCBL51
/*
/* This patch applies to the FCOBOL compiler received on the 5731-AA1
/*    tape, available to early DOS/VS users.  But the fix applies to
/*    the ANS COBOL compiler included in the DOS 26.2 distribution as 
/*    well.  
/* This patch is a DOS retrofit of the patch provided by Bill Carlborg 
/*    in the Yahoo Group for Hercules DOS/VS.  
/* Wrong code is generated when compiling an ANS COBOL subroutine
/*    (PROCEDURE DIVISION USING) with OPTION XREF in effect because
/*    registers are saved/restored incorrectly in the subroutine that 
/*    adds an entry to the cross-reference list.  
/* The workaround is to use OPTION NOXREF.  
/* The correction patches ILACBL51 to save the correct registers to the
/*    patch area included in ILACBL51.  
/* In real DOS, we would have punched out relocatable module ILACBL51,
/*    multi-punched a couple of Linkage Editor REP cards, recataloged
/*    ILACBL51, and (re-)run the ANS COBOL link edit.  
/* DOS does not include the PDZAP utility, so we cannot just patch 
/*    phase.  Also missing is OBJMAINT, so we cannot easily apply REP
/*    cards to a SYSPCH file of ILACBL51.  We'll need to do this 
/*    another way.  
/*
/*  PUNCH OUT MODULE TO BE REP'D
// DLBL IJSYSPH,'DOS.BG.WORKFILE.004',0,SD
// EXTENT SYSPCH,WRK14A,1,1,2600,100  (IJSYS04)
ASSGN SYSPCH,X'192'
// EXEC RSERV
 PUNCH ILACBL51
/*
CLOSE SYSPCH,X'00D'
/* COPY MODULE, ADDING REP CARDS
// DLBL UIN,'DOS.BG.WORKFILE.004',0,SD
// EXTENT SYS004,WRK14A,1,1,2600,100  (IJSYS04)
// DLBL UOUT,'DOS.BG.WORKFILE.004.2',0,SD
// EXTENT SYS005,WRK14A,1,1,2700,100  (IJSYS04)
// ASSGN SYS000,X'00C'
// ASSGN SYS004,X'192'
// ASSGN SYS005,X'192'
// EXEC ZAPRELO
+REP  000C72 00190E3,DC80
+REP  000C90 00198E3,DC80
/*
// DLBL IJSYSIN,'DOS.BG.WORKFILE.004.2',0,SD
// EXTENT SYSIPT,WRK14A,1,1,2700,100  (IJSYS04)
ASSGN SYSIPT,X'192'
// EXEC MAINT
CLOSE SYSIPT,X'00C'
// EXEC RSERV
 DSPLY ILACBL51
/*
/&
ASSGN SYSIPT,X'00C'   OR CLOSE SYSIPT,X'00C' IF JOB CANCELS