//DANIBT01 JOB (2582,12,0120,9999,9999,,1),'45855,PRN,PROD',
//   CLASS=A,MSGCLASS=4
//*LOGONID PRD2584
//JOBLIB DD DSN=SYS1.PGMLIB,DISP=SHR
//       DD DSN=ETS.SYSLIB,DISP=SHR
//*  *******************************************************************
//*  =-->
//*  =-->               INSTRUCTIONS TO THE OPERATOR
//*  =-->
//*  =-->  THIS JOB PICKS UP THE PRIME IBT FILE FROM BALITMORE
//*  =-->  ON THE BALTIMORE SFTP SERVER AND IS TRANSFERRED TO
//*  =-->  THE ETS MAINFRAME FOR PROCESSING TO DAN.I01.IBT
//*  =-->
//** **************************************************************
//DANIBT01 EXEC PROC=DANIBT