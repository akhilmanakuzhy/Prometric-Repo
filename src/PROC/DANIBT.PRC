//**********************************************************
//*  DANB10IC REPLACES DANB06                   ************
//**********************************************************
//DANIBT   PROC
//DANI03  EXEC  PGM=DANI03                                              00113515
//*INDIR    DD   DSN=DAN.FTP.IBTLIST,DISP=SHR
//FTP      DD   DSN=DAN.IBT.SCRIPT,DISP=SHR
//PRT1     DD   SYSOUT=*
//SYSOUX   DD   SYSOUT=*
//SYSOUT   DD   SYSOUT=*
//SYSPRINT DD   SYSOUT=*
//EZTVFM   DD   UNIT=HSTEMP,SPACE=(CYL,(25,5),RLSE)