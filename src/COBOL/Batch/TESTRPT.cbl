       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTRPT.
      ******************************************************************
      *07-19-06 - NEW PROGRAM TO CREATE TCO EXPORT FOR DANTES OF       
      *        ON BASE AND ON CAMPUS FTP FOR MILITARY IBT.          
      *06-19-11 - ADD FULLY FUNDED INDIRS TO EXPORT                     
      *02-19-17 - ADD EDITS TO SUPPORT DAIMS PGMING CHANGES            
      *04-09-18 - ONBASE FTP ASSOCIATED TO 7777 NEED 'F' BRANCH         
      *05-19-21 - ADD SURPASS 6900 PRO PROCTOR FULLY FUNDED
      *12-19-22 - Converted from EZT to COBOL
      *           MODIFIED LOGIC TO ACCOMODATE FTP O/P FORMAT IN MFCOUS
      *06-19-23 - MODIFIED TO PULL FILE FROM PRO-PROCTOR DIRECTLY RATHER
      *           THAN TCNET.                                           
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT Section.
       FILE-CONTROL.
      *    SELECT INDIR  ASSIGN TO INDIR
      *                   FILE STATUS IS INDIR-STATUS.
           SELECT FTP  ASSIGN TO FTP
                        FILE STATUS IS FTP-STATUS.   
           
       DATA DIVISION.
       FILE SECTION.
      *FD  INDIR.
      *01  INDIRINP.
      *     03 INP-REC.
      *       05 I-MSGNO       PIC X(8).
      *       05 I-MSGNO       PIC X(19).
      *       05 FILLER        PIC X(1).
      *       05 I-FILENAME.
      *         07 I-FILE5     PIC X(5).
      *         07 I-FILENM    PIC X(14).
      *       05 FILLER        PIC X(41).
      ******************************************************************
      **                             FTP                                
      **
      ******************************************************************
       FD  FTP.
  *
       01  FTP-REC PIC X(80).
         


       WORKING-STORAGE SECTION.
       
      *01 INDIR-ENDFILE PIC X VALUE 'N'.
      *01 INDIR-STATUS PIC XX VALUE '00'.
       01 FTP-STATUS PIC XX VALUE '00'.
       01 PROCESS-FLAG PIC X VALUE 'N'.
       01 W-CTRA  PIC 9(9). 
       01 W-CTRB  PIC 9(9).
       01 W-CTRC  PIC 9(9).
       01 W-GET   PIC X(4) VALUE 'GET '.
       01 WS-FILLER PIC X VALUE ' '.
       01 W-PUT PIC X(22) VALUE '''DAN.I01.IBT'' (REPLACE'.
       01 WS-LAT-FILE PIC X(11) VALUE SPACES.
       
       01 WS-SYSDATE.
          03 WS-SYSDATE-YY PIC X(2).
          03 WS-SYSDATE-MM PIC X(2).
          03 WS-SYSDATE-DD PIC X(2).
          

       01 WB-FIELDS.                                              
         05 WB-FLUSH-ARG           VALUE +0   PIC S9(4) COMP.

        
       PROCEDURE DIVISION.

           PERFORM 100-OPEN-FILES THRU 100-OPEN-EXIT 1000 TIMES
           PERFORM 200-INIT-PROCESS  THRU 200-INIT-EXIT
           PERFORM 300-FTPCARD-BUILD THRU 300-FTPCARD-EXIT           
           PERFORM 999-CLOSE-FILES   THRU 999-CLOSE-EXIT
           STOP RUN
           .
           
       100-OPEN-FILES.
           
      *    OPEN INPUT INDIR.
      *
      *    IF INDIR-Status IS NOT = '00' AND '97'
      *        DISPLAY 'PROGRAM TERMINATED. Invalid Status Code on OPEN 
      *            'INDIR. CODE=' INDIR-Status
      *        MOVE INDIR-STATUS TO WB-FLUSH-ARG
      *        CALL 'ILBOABN0' USING WB-FLUSH-ARG
      *    END-IF .
           
           OPEN OUTPUT FTP.
           IF FTP-STATUS IS NOT = '00' AND '04' 
              DISPLAY 'PROGRAM TERMINATED. Invalid Status Code on OPEN
      -            'FTP. CODE=' FTP-Status
               MOVE FTP-STATUS TO WB-FLUSH-ARG
               CALL 'ILBOABN0' USING WB-FLUSH-ARG
           END-IF


            .
           
       100-OPEN-EXIT.
           EXIT.  

       200-INIT-PROCESS.
           
      *    INITIALIZE INDIRINP FTP-REC W-CTRA W-CTRB W-CTRC   
           INITIALIZE FTP-REC W-CTRA W-CTRB W-CTRC   
           ACCEPT WS-SYSDATE FROM DATE
           DISPLAY 'SYSDATE=' WS-SYSDATE
           STRING WS-SYSDATE-MM, WS-SYSDATE-DD, WS-SYSDATE-YY, 
                  "c.txt" DELIMITED BY SIZE INTO WS-LAT-FILE.
           
           .
       200-INIT-EXIT.
           EXIT.
           
       300-FTPCARD-BUILD.

      *    READ INDIR RECORD AT END MOVE 'Y' TO INDIR-ENDFILE.
      *    IF INDIR-STATUS IS NOT = '00' AND 
      *       INDIR-STATUS IS NOT = '10'
      *        DISPLAY 'PROGRAM TERMINATED. STATUS CODE NOT 00 FOR READ 
      *            'INDIR Code=' INDIR-STATUS
      *        MOVE 'Y' TO INDIR-ENDFILE
      *    END-IF
      *    
      *    
      *    IF INDIR-ENDFILE = 'Y' OR
      *       INDIRINP (21:22) = '226 Transfer complete.'               
      *       MOVE 'N' TO PROCESS-FLAG 
      *       PERFORM 400-FINISH-PROC  THRU 400-FINISH-PROC
      *       PERFORM 999-CLOSE-FILES  THRU 999-CLOSE-EXIT
      *       STOP RUN
      *    END-IF
           
           
              INITIALIZE FTP-REC 
      *       MOVE '10.180.152.81' TO FTP-REC
              MOVE '10.173.187.66' TO FTP-REC
              WRITE FTP-REC
              INITIALIZE FTP-REC 
      *       MOVE 'DSSTProm T#8meA5R' TO FTP-REC
              MOVE 'DANTESFTP dg5f3bjr' TO FTP-REC
              WRITE FTP-REC
              INITIALIZE FTP-REC 
              MOVE 'CD From_CGI' TO FTP-REC
              WRITE FTP-REC
              INITIALIZE FTP-REC 
              MOVE 'ASCII' TO FTP-REC
              WRITE FTP-REC
              INITIALIZE FTP-REC 
              MOVE 'LOCSITE ENCODING=SBCS' TO FTP-REC
              WRITE FTP-REC
              INITIALIZE FTP-REC 
              MOVE 'LOCSITE SBDATACONN=EBC2ASC' TO FTP-REC
              WRITE FTP-REC
              INITIALIZE FTP-REC 
              MOVE 'LOCSITE TRAILINGBLANKS=FALSE' TO FTP-REC
              WRITE FTP-REC
              
              INITIALIZE FTP-REC
              STRING W-GET WS-LAT-FILE WS-FILLER W-PUT  
                    DELIMITED BY SIZE INTO FTP-REC
              WRITE FTP-REC
              INITIALIZE FTP-REC
              MOVE 'quit' TO FTP-REC
              WRITE FTP-REC
           
           .
           
       300-FTPCARD-EXIT.
           EXIT.  
           
       999-CLOSE-FILES.
           
      *    CLOSE INDIR FTP    
           CLOSE FTP    
           .
       999-CLOSE-EXIT.
           EXIT.
 