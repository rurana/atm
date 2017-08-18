 NAME  PROG02
 IDENTIFICATION DIVISION.
 PROGRAM-ID. P26AP01.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01  WS-CA       PIC X(01).
 01  WS-IN1      PIC 9(02).
 01  WS-IN1O REDEFINES WS-IN1 PIC X(02).
 01  WS-IN2      PIC 9(02).
 01  WS-IN2O REDEFINES WS-IN2 PIC X(02).
 01  WS-ADD      PIC 9(03).
 01  WS-SUB      PIC 9(03).
 01  OP-SUB      PIC -9(03).
 01  WS-MUL      PIC 9(03).
 01  WS-DIV      PIC 9(03).
 01  OP-DIV      PIC 9(01).99.
 01  WS-MESSAGE  PIC X(30) VALUE
     'END OF THE PROGRAM, BYE, BYE!!'.
     COPY P26AS01.
     COPY DFHAID.
 LINKAGE SECTION.
 01  DFHCOMMAREA     PIC X(01).
 PROCEDURE DIVISION.
 MAIN-PARA.
     IF EIBCALEN = ZERO
         PERFORM FIRST-PARA
     ELSE
         PERFORM NEXT-PARA.
 END-PARA.
     EXEC CICS RETURN
         TRANSID('P26A')
         COMMAREA(WS-CA)
     END-EXEC.
 FIRST-PARA.
     MOVE LOW-VALUES TO CALCO
     PERFORM SEND-MAP.
 SEND-MAP.
     EXEC CICS SEND
         MAP('CALC')
         MAPSET('P26AS01')
         FROM (CALCO)
         ERASE
     END-EXEC.
 NEXT-PARA.
     EVALUATE EIBAID
        WHEN DFHPF3
         EXEC CICS SEND TEXT
             FROM(WS-MESSAGE)
             ERASE
         END-EXEC
         EXEC CICS RETURN
         END-EXEC
        WHEN DFHENTER
         PERFORM PROCESS-PARA
        WHEN OTHER
         MOVE 'INVALID KEY PRESSED' TO MESSAGEO
     END-EVALUATE.
     PERFORM SEND-MAP.
 PROCESS-PARA.
     PERFORM RECEIVE-MAP.
     IF INPUT1L = ZERO OR INPUT2L = ZERO
         MOVE 'PLEASE ENTER VALID INPUTS' TO MESSAGEO
     ELSE
        IF INPUT1I IS NUMERIC AND INPUT2I IS NUMERIC
             MOVE INPUT1I TO WS-IN1O
             MOVE INPUT2I TO WS-IN2O
             COMPUTE WS-ADD = WS-IN1 + WS-IN2
             COMPUTE WS-SUB = WS-IN1 - WS-IN2
             COMPUTE WS-MUL = WS-IN1 * WS-IN2
             COMPUTE WS-DIV = WS-IN1 / WS-IN2
             MOVE WS-ADD  TO ADDO
*            MOVE WS-SUB TO OP-SUB
             MOVE WS-SUB  TO SUBO
             MOVE WS-MUL  TO MULO
*            MOVE WS-DIV TO OP-DIV
             MOVE WS-DIV  TO DIVO
             MOVE 'SUM OF 2 NUMBERS IS DISPLAYED' TO MESSAGEO
        ELSE
             MOVE 'INPUTS ARE NOT NUMERIC' TO MESSAGEO.
 RECEIVE-MAP.
     EXEC CICS RECEIVE
         MAP('CALC')
         MAPSET('P26AS01')
         INTO(CALCI)
     END-EXEC.

*******************************************************************

 NAME  P26AP03
 IDENTIFICATION DIVISION.
 PROGRAM-ID. P26AP03.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01  WS-CA       PIC X(01).
 01  WS-RESP-CODE PIC S9(08) COMP VALUE ZERO.
 01  WS-EARN     PIC ZZ,ZZ9.99 VALUE ZERO.
 01  WS-DEDN     PIC ZZ,ZZ9.99 VALUE ZERO.
 01  WS-SAL-O    PIC ZZ,ZZ9.99 VALUE ZERO.
 01  WS-SAL      PIC 9(05)V99 VALUE ZERO.
 01  WS-DATE-TIME PIC S9(15) COMP-3 VALUE ZERO.
     COPY EMPREC.
     COPY P26AS03.
     COPY DFHAID.
 LINKAGE SECTION.
 01  DFHCOMMAREA     PIC X(01).
 PROCEDURE DIVISION.
 MAIN-PARA.
     IF EIBCALEN = ZERO
         PERFORM FIRST-PARA
     ELSE
         PERFORM NEXT-PARA.
 END-PARA.
     EXEC CICS RETURN
         TRANSID('P26C')
         COMMAREA(WS-CA)
     END-EXEC.
 FIRST-PARA.
     MOVE LOW-VALUES TO EMPINQO
     PERFORM DATE-TIME-PARA
     PERFORM SEND-MAP.
 SEND-MAP.
     EXEC CICS SEND
         MAP('EMPINQ')
         MAPSET('P26AS03')
         FROM (EMPINQI)
         ERASE
     END-EXEC.
     PERFORM DATE-TIME-PARA.
 NEXT-PARA.
     EVALUATE EIBAID
        WHEN DFHPF3
         EXEC CICS XCTL
             PROGRAM('P26AP02')
         END-EXEC
        WHEN DFHENTER
            PERFORM PROCESS-PARA
        WHEN OTHER
            MOVE 'INVALID KEY PRESSED' TO MESSAGEO
     END-EVALUATE
     PERFORM SEND-MAP.
 PROCESS-PARA.
     PERFORM RECEIVE-MAP.
     IF IDL = ZERO
         MOVE 'PLEASE ENTER AN EMPLOYEE ID' TO MESSAGEO
     ELSE
         IF IDI = SPACES
            MOVE 'PLEASE ENTER A VALID EMPLOYEE ID' TO MESSAGE
         ELSE
            PERFORM READ-EMPLOYEE
         END-IF
     END-IF.
 READ-EMPLOYEE.
     EXEC CICS READ
         FILE('P26AFILE')
         RIDFLD(IDI)
         INTO(EMPLOYEE-RECORD)
         RESP(WS-RESP-CODE)
     END-EXEC.
     EVALUATE WS-RESP-CODE
         WHEN DFHRESP(NORMAL)
             MOVE EMP-ID   TO IDO
             MOVE EMP-NAME TO NAMEO
             MOVE EMP-TECH TO TECHO
             MOVE EMP-LOC  TO LOCO
             MOVE EMP-EARN TO WS-EARN
             PERFORM MOVE-DOB
             MOVE WS-EARN  TO EARNO
             MOVE EMP-DEDN TO WS-DEDN
             MOVE WS-DEDN  TO DEDNO
             COMPUTE WS-SAL = EMP-EARN - EMP-DEDN
             MOVE WS-SAL   TO WS-SAL-O
             MOVE WS-SAL-O TO SALO
             MOVE 'EMPLOYEE DETAILS FOUND' TO MESSAGEO
         WHEN DFHRESP(NOTFND)
             MOVE SPACES   TO NAMEO, DOBO, TECHO, LOCO,
                              EARNO, DEDNO, SALO
             MOVE 'EMPLOYEE NOT FOUND    ' TO MESSAGEO
         WHEN OTHER
             MOVE 'ERROR ACCESSING FILE  ' TO MESSAGEO
             MOVE SPACES   TO NAMEO, DOBO, TECHO, LOCO,
                              EARNO, DEDNO, SALO
     END-EVALUATE.
 MOVE-DOB.
     MOVE EMP-DOB TO DOBO.
 RECEIVE-MAP.
     EXEC CICS RECEIVE
         MAP('EMPINQ')
         MAPSET('P26AS03')
         INTO(EMPINQI)
     END-EXEC.
 DATE-TIME-PARA.
     EXEC CICS ASKTIME
         ABSTIME(WS-DATE-TIME)
     END-EXEC.
     EXEC CICS FORMATTIME
         ABSTIME(WS-DATE-TIME)
         DDMMYYYY(SYSDATEO)
         DATESEP
         TIME(SYSTIMEO)
         TIMESEP
     END-EXEC.
*******************************************************************
 NAME  P26AP01
 IDENTIFICATION DIVISION.
 PROGRAM-ID. P26AP01.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01  WS-CA       PIC X(01).
 01  WS-IN1      PIC 9(02).
 01  WS-IN1O REDEFINES WS-IN1 PIC X(02).
 01  WS-IN2      PIC 9(02).
 01  WS-IN2O REDEFINES WS-IN2 PIC X(02).
 01  WS-ADD      PIC 9(03).
 01  OP-ADD      PIC ZZ9.
 01  WS-SUB      PIC S9(03).
 01  OP-SUB      PIC -9(03).
 01  WS-MUL      PIC 9(03).
 01  OP-MUL      PIC ZZ9.
 01  WS-DIV      PIC 9(01)V99.
 01  OP-DIV      PIC 9(01).99.
 01  WS-MESSAGE  PIC X(30) VALUE
     'END OF THE PROGRAM, BYE, BYE!!'.
     COPY P26AS01.
     COPY DFHAID.
 LINKAGE SECTION.
 01  DFHCOMMAREA     PIC X(01).
 PROCEDURE DIVISION.
 MAIN-PARA.
     IF EIBCALEN = ZERO
         PERFORM FIRST-PARA
     ELSE
         PERFORM NEXT-PARA.
 END-PARA.
     EXEC CICS RETURN
         TRANSID('P26A')
         COMMAREA(WS-CA)
     END-EXEC.
 FIRST-PARA.
     MOVE LOW-VALUES TO CALCO
     PERFORM SEND-MAP.
 SEND-MAP.
     EXEC CICS SEND
         MAP('CALC')
         MAPSET('P26AS01')
         FROM (CALCO)
         ERASE
     END-EXEC.
 NEXT-PARA.
     EVALUATE EIBAID
        WHEN DFHPF3
         EXEC CICS SEND TEXT
             FROM(WS-MESSAGE)
             ERASE
         END-EXEC
         EXEC CICS RETURN
         END-EXEC
        WHEN DFHENTER
         PERFORM PROCESS-PARA
        WHEN OTHER
         MOVE 'INVALID KEY PRESSED' TO MESSAGEO
     END-EVALUATE.
     PERFORM SEND-MAP.
 PROCESS-PARA.
     PERFORM RECEIVE-MAP.
     IF INPUT1L = ZERO OR INPUT2L = ZERO
         MOVE 'PLEASE ENTER VALID INPUTS' TO MESSAGEO
     ELSE
        IF INPUT1I IS NUMERIC AND INPUT2I IS NUMERIC
             MOVE INPUT1I TO WS-IN1O
             MOVE INPUT2I TO WS-IN2O
             COMPUTE WS-ADD = WS-IN1 + WS-IN2
             COMPUTE WS-SUB = WS-IN1 - WS-IN2
             COMPUTE WS-MUL = WS-IN1 * WS-IN2
             COMPUTE WS-DIV = WS-IN1 / WS-IN2
             MOVE WS-ADD  TO ADDO
*            MOVE WS-SUB TO OP-SUB
             MOVE WS-SUB  TO SUBO
             MOVE WS-MUL  TO MULO
             MOVE WS-DIV TO OP-DIV
             MOVE OP-DIV  TO DIVO
             MOVE 'SUM OF 2 NUMBERS IS DISPLAYED' TO MESSAGEO
        ELSE
             MOVE 'INPUTS ARE NOT NUMERIC' TO MESSAGEO.
 EXIT-PARA.
     EVALUATE EIBAID
        WHEN DFHPF3
           EXEC CICS XCTL
              PROGRAM('P26AP02')
           END-EXEC
     END-EVALUATE.

 RECEIVE-MAP.
     EXEC CICS RECEIVE
         MAP('CALC')
         MAPSET('P26AS01')
         INTO(CALCI)
     END-EXEC.
******************************************************************* 
 
 NAME  P26AP02
 IDENTIFICATION DIVISION.
 PROGRAM-ID. P26AP02.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01  WS-CA       PIC X(01).
 01  WS-CHOICE   PIC 9(01) VALUE ZERO.
     88  VALID-CHOICE      VALUE 1 THRU 4.
 01  WS-CHOICE-X REDEFINES WS-CHOICE PIC X(01).
 01  WS-DATE-TIME PIC S9(15) COMP-3 VALUE ZERO.
 01  WS-MESSAGE  PIC X(30) VALUE
     'END OF THE PROGRAM, BYE, BYE!!'.
     COPY P26AS02.
     COPY DFHAID.
 LINKAGE SECTION.
 01  DFHCOMMAREA     PIC X(01).
 PROCEDURE DIVISION.
 MAIN-PARA.
     IF EIBCALEN = ZERO
         PERFORM FIRST-PARA
     ELSE
         PERFORM NEXT-PARA.
 END-PARA.
     EXEC CICS RETURN
         TRANSID('P26B')
         COMMAREA(WS-CA)
     END-EXEC.
 FIRST-PARA.
     MOVE LOW-VALUES TO CHOICEO
     PERFORM DATE-TIME-PARA
     PERFORM SEND-MAP.
 SEND-MAP.
     EXEC CICS SEND
         MAP('MENUMAP')
         MAPSET('P26AS02')
         FROM (MENUMAPI)
         ERASE
     END-EXEC.
 NEXT-PARA.
     EVALUATE EIBAID
        WHEN DFHPF3
         EXEC CICS SEND TEXT
             FROM(WS-MESSAGE)
             ERASE
         END-EXEC
         EXEC CICS RETURN
         END-EXEC
        WHEN DFHENTER
            PERFORM PROCESS-PARA
        WHEN OTHER
            MOVE 'INVALID KEY PRESSED' TO MESSAGEO
     END-EVALUATE.
     PERFORM SEND-MAP.
 PROCESS-PARA.
     PERFORM RECEIVE-MAP.
     MOVE CHOICEO TO WS-CHOICE-X
     IF VALID-CHOICE
         PERFORM TRANSFER-PARA
     ELSE
         MOVE 'INVALID CHOICE' TO MESSAGEO
     END-IF.
 TRANSFER-PARA.
     EVALUATE WS-CHOICE
        WHEN 1
           EXEC CICS XCTL
              PROGRAM('P26AP01')
           END-EXEC
        WHEN 2
           EXEC CICS XCTL
              PROGRAM('P26AP03')
           END-EXEC
        WHEN 3
           EXEC CICS XCTL
              PROGRAM('P26AP04')
           END-EXEC
        WHEN 4
           EXEC CICS XCTL
              PROGRAM('P26AP05')
           END-EXEC
        WHEN OTHER
           MOVE 'PROGRAM NOT READY' TO MESSAGEO
     END-EVALUATE.
 RECEIVE-MAP.
     EXEC CICS RECEIVE
         MAP('MENUMAP')
         MAPSET('P26AS02')
         INTO(MENUMAPO)
     END-EXEC.
 DATE-TIME-PARA.
     EXEC CICS ASKTIME
         ABSTIME(WS-DATE-TIME)
     END-EXEC.
     EXEC CICS FORMATTIME
         ABSTIME(WS-DATE-TIME)
         DDMMYYYY(SYSDATEO)
         DATESEP
         TIME(SYSTIMEO)
         TIMESEP
     END-EXEC.
*******************************************************************

 NAME  P26AP09
 IDENTIFICATION DIVISION.
 PROGRAM-ID. P26AP09.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01  WS-RESP-CODE      PIC S9(08) COMP VALUE ZERO.
 01  WS-TABLE                          VALUE SPACES.
     03  WS-TAB        PIC X(16) OCCURS 12 TIMES.
 01  PARA-SUB          PIC 9(02) VALUE ZERO.
 01  WS-CA.
     03  WS-PROCESS-FLAG      PIC X(01) VALUE SPACES.
         88  PROCESS-FSTMAP             VALUE '1'.
         88  PROCESS-DEL                VALUE '2'.
         88  PROCESS-ADD-MOD            VALUE '3'.
         88  PROCESS-SECMAP             VALUE '2', '3'.
     03  WS-EMP-ID            PIC X(05) VALUE SPACES.
 01  WS-DEPT     PIC X(08) VALUE SPACES.
     88  HR VALUE '   HR   '.
     88  ADMIN VALUE ' ADMIN  '.
     88  SALES VALUE ' SALES  '.
     88  TRAINING VALUE 'TRAINING'.
 01  WS-MESSAGE  PIC X(30) VALUE
     'END OF THE PROGRAM'.
     COPY P26AS09.
     COPY DFHAID.
     COPY DFHBMSCA.
     COPY EMP.
 01  WS-DATE-TIME             PIC S9(15) COMP-3 VALUE ZERO.
 01  WS-DOB.
     03  WS-DOB-YY            PIC 9(04) VALUE ZERO.
     03  WS-DOB-MM            PIC 9(02) VALUE ZERO.
     03  WS-DOB-DD            PIC 9(02) VALUE ZERO.
 01  WS-DOJ.
     03  WS-DOJ-YY            PIC 9(04) VALUE ZERO.
     03  WS-DOJ-MM            PIC 9(02) VALUE ZERO.
     03  WS-DOJ-DD            PIC 9(02) VALUE ZERO.
 01  WS-VALID-FLAG            PIC X(01) VALUE 'N'.
     88  VALID-DATA                     VALUE 'Y'.

 LINKAGE SECTION.
 01  DFHCOMMAREA  PIC X(06).

 PROCEDURE DIVISION.
 MAIN-PARA.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'MAIN-PARA       ' TO WS-TAB(PARA-SUB)
     END-IF
     IF EIBCALEN = ZERO
        PERFORM FIRST-PARA
     ELSE
        MOVE DFHCOMMAREA TO WS-CA
        PERFORM NEXT-PARA THRU EXIT-PARA
     END-IF.
**********************************************************
 END-PARA.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'END-PARA        ' TO WS-TAB(PARA-SUB)
     END-IF
     EXEC CICS RETURN
          TRANSID('P26I')
          COMMAREA(WS-CA)
     END-EXEC.
********************************************************
 FIRST-PARA.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'FIRST-PARA      ' TO WS-TAB(PARA-SUB)
     END-IF
     MOVE LOW-VALUES TO FSTMAPO
     PERFORM SEND-FIRST-MAP.
********************************************************
 SEND-FIRST-MAP.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'SEND-FIRST-MAP  ' TO WS-TAB(PARA-SUB)
     END-IF
     PERFORM DATE-TIME-PARA.
     PERFORM SEND-FST-MAP.
     SET PROCESS-FSTMAP TO TRUE.
********************************************************
 SEND-FST-MAP.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'SEND-FST-MAP    ' TO WS-TAB(PARA-SUB)
     END-IF
     MOVE -1 TO ID1L
     EXEC CICS SEND
         MAP('FSTMAP') MAPSET('P26AS09')
         FROM(FSTMAPO) CURSOR ERASE
     END-EXEC.
********************************************************
 NEXT-PARA.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'NEXT-PARA       ' TO WS-TAB(PARA-SUB)
     END-IF
     EVALUATE TRUE
        WHEN PROCESS-FSTMAP
           PERFORM PROCESS-FSTMAP-PARA
        WHEN PROCESS-SECMAP
           PERFORM PROCESS-SECMAP-PARA
     END-EVALUATE.
********************************************************
 SEND-PARA.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'SEND-PARA       ' TO WS-TAB(PARA-SUB)
     END-IF
     EVALUATE TRUE
        WHEN PROCESS-FSTMAP
           PERFORM SEND-FST-MAP
        WHEN PROCESS-SECMAP
           PERFORM SEND-SEC-MAP
     END-EVALUATE.
 EXIT-PARA.
     EXIT.
********************************************************
 PROCESS-FSTMAP-PARA.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'PROC FSTMAP PARA' TO WS-TAB(PARA-SUB)
     END-IF
     EVALUATE EIBAID
        WHEN DFHENTER
           PERFORM RECEIVE-PROCESS-FSTMAP
        WHEN DFHPF3
           PERFORM EXIT-PARA
        WHEN OTHER
           MOVE 'INVALID KEY PRESSED' TO MSG1O
     END-EVALUATE.
********************************************************
 RECEIVE-PROCESS-FSTMAP.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'RCV PROC FSTMAP ' TO WS-TAB(PARA-SUB)
     END-IF
     PERFORM RECEIVE-FST-MAP
     IF ID1L = ZERO OR ID1I = SPACES
        MOVE 'PLEASE ENTER EMPLOYEE NUMBER' TO MSG1O
     ELSE
        PERFORM READ-EMPLOYEE
     END-IF.
********************************************************
 RECEIVE-FST-MAP.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'RECEIVE KEY MAP ' TO WS-TAB(PARA-SUB)
     END-IF
     EXEC CICS RECEIVE
         MAP('FSTMAP') MAPSET('P26AS09')
         INTO(FSTMAPI)
     END-EXEC.
********************************************************
 READ-EMPLOYEE.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'READ EMPLOYEE   ' TO WS-TAB(PARA-SUB)
     END-IF
     MOVE LOW-VALUES TO SECMAPO
     MOVE ID1I TO WS-EMP-ID, ID2O
     EXEC CICS READ
         FILE('P26EFILE') RIDFLD(WS-EMP-ID)
         INTO(EMPLOYEE-RECORD) RESP(WS-RESP-CODE)
     END-EXEC.
     EVALUATE WS-RESP-CODE
         WHEN DFHRESP(NORMAL)
            MOVE EMP-NAME  TO NAMEO
            MOVE EMP-ADDR  TO ADDRO
            MOVE EMP-DEPT  TO DEPTO
            MOVE EMP-DOB-YY  TO DOBYYO
            MOVE EMP-DOB-MM  TO DOBMMO
            MOVE EMP-DOB-DD  TO DOBDDO
            MOVE EMP-DOJ-YY  TO DOJYYO
            MOVE EMP-DOJ-MM  TO DOJMMO
            MOVE EMP-DOJ-DD  TO DOJDDO
            MOVE -1          TO NAMEL
            SET PROCESS-ADD-MOD TO TRUE
            MOVE 'MODIFY' TO PF2O
         WHEN DFHRESP(NOTFND)
            MOVE -1                TO NAMEL
            MOVE SPACES TO NAMEO, ADDRO, DEPTO,
                 DOBYYO, DOBMMO, DOBDDO,
                 DOJYYO, DOJMMO, DOJDDO
            SET PROCESS-ADD-MOD TO TRUE
            MOVE 'ADD' TO PF2O
            MOVE DFHBMDAR TO PF4A
         WHEN OTHER
            MOVE 'ERROR PROCESSING FILE' TO MSG1O
     END-EVALUATE.
********************************************************
 SEND-SEC-MAP.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'SEND-SEC-MAP ' TO WS-TAB(PARA-SUB)
     END-IF
     PERFORM DATE-TIME-PARA.
     EXEC CICS SEND
         MAP('SECMAP')
         MAPSET('P26AS09')
         FROM(SECMAPO)
         CURSOR
         ERASE
     END-EXEC.
********************************************************
 PROCESS-SECMAP-PARA.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'PROC SECMAP PARA' TO WS-TAB(PARA-SUB)
     END-IF
     EVALUATE EIBAID
         WHEN DFHPF2
            IF PF2O = 'ADD'
               PERFORM ADD-PARA
            END-IF
            IF PF2O = 'MODIFY'
               PERFORM UPDATE-PARA
            ELSE
               MOVE 'PF2 NOT ENABLED' TO MSG2O
            END-IF
         WHEN DFHPF3
            SET PROCESS-FSTMAP TO TRUE
         EXEC CICS SEND TEXT
             FROM(WS-MESSAGE)
             ERASE
         END-EXEC
         EXEC CICS RETURN
         END-EXEC
         WHEN DFHPF4
            IF PROCESS-DEL
               PERFORM DELETE-PARA
            ELSE
               MOVE 'PF4 NOT ENABLED' TO MSG2O
            END-IF
         WHEN DFHPF5
               MOVE LOW-VALUES TO FSTMAPO
               MOVE LOW-VALUES TO SECMAPO
         WHEN OTHER
            MOVE 'INVALID KEY PRESSED' TO MSG2O
     END-EVALUATE.
********************************************************
 RECEIVE-SEC-MAP.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'RCV DETAIL MAP  ' TO WS-TAB(PARA-SUB)
     END-IF
     EXEC CICS RECEIVE
         MAP('SECMAP') MAPSET('P26AS09')
         INTO(SECMAPI)
     END-EXEC.
********************************************************
 DELETE-PARA.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'DELETE-PARA     ' TO WS-TAB(PARA-SUB)
     END-IF
     EXEC CICS DELETE
         FILE('P26EFILE') RIDFLD(WS-EMP-ID)
         RESP(WS-RESP-CODE)
     END-EXEC
     EVALUATE WS-RESP-CODE
         WHEN DFHRESP(NORMAL)
            MOVE 'DELETE SUCCESSFUL' TO MSG1O
         WHEN OTHER
            MOVE 'DELETE NOT SUCCESSFUL' TO MSG1O
     END-EVALUATE
     SET PROCESS-FSTMAP TO TRUE
     MOVE WS-EMP-ID TO ID1O.
********************************************************
 ADD-PARA.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'DEL-PARA        ' TO WS-TAB(PARA-SUB)
     END-IF
     PERFORM RECEIVE-SEC-MAP
     PERFORM VALIDATE-PARA
     IF VALID-DATA
        PERFORM MOVE-PARA
        PERFORM WRITE-PARA
     END-IF.
********************************************************
 WRITE-PARA.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'WRITE-PARA      ' TO WS-TAB(PARA-SUB)
     END-IF
     EXEC CICS WRITE
         FILE('P26EFILE')
         RIDFLD(WS-EMP-ID)
         FROM (EMPLOYEE-RECORD)
         RESP(WS-RESP-CODE)
     END-EXEC
     EVALUATE WS-RESP-CODE
         WHEN DFHRESP(NORMAL)
            MOVE 'ADD SUCCESSFUL' TO MSG1O
         WHEN OTHER
            MOVE 'ADD NOT SUCCESSFUL' TO MSG1O
     END-EVALUATE
     SET PROCESS-FSTMAP TO TRUE
     MOVE WS-EMP-ID TO ID1O.
********************************************************
 UPDATE-PARA.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'UPDATE-PARA     ' TO WS-TAB(PARA-SUB)
     END-IF
     PERFORM RECEIVE-SEC-MAP
     PERFORM VALIDATE-PARA
     IF VALID-DATA
        PERFORM REWRITE-PARA
     END-IF.
********************************************************
 REWRITE-PARA.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'REWRITE-PARA    ' TO WS-TAB(PARA-SUB)
     END-IF
     EXEC CICS READ
         FILE('P26EFILE')
         RIDFLD(WS-EMP-ID)
         INTO(EMPLOYEE-RECORD)
         RESP(WS-RESP-CODE)
         UPDATE
     END-EXEC
     EVALUATE WS-RESP-CODE
         WHEN DFHRESP(NORMAL)
            PERFORM MOVE-PARA
            EXEC CICS REWRITE
                FILE('P26EFILE')
                FROM(EMPLOYEE-RECORD)
                RESP(WS-RESP-CODE)
            END-EXEC
            EVALUATE WS-RESP-CODE
              WHEN DFHRESP(NORMAL)
                 MOVE 'UPDATE SUCCESSFUL' TO MSG1O
              WHEN OTHER
                 MOVE 'UPDATE NOT SUCCESSFUL' TO MSG1O
            END-EVALUATE
         WHEN OTHER
            MOVE 'ERROR PROCESSING FILE' TO MSG1O
     END-EVALUATE
     SET PROCESS-FSTMAP TO TRUE
     MOVE WS-EMP-ID TO ID1O.
********************************************************
 VALIDATE-PARA.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'VALIDATE-PARA   ' TO WS-TAB(PARA-SUB)
     END-IF
         IF EMP-NAME = SPACES OR ZERO OR
            EMP-NAME IS NUMERIC
             MOVE -1 TO NAMEL
             MOVE 'N' TO WS-VALID-FLAG.
         IF EMP-ADDR = SPACES OR ZERO
             MOVE -1 TO ADDRL
             MOVE 'N' TO WS-VALID-FLAG.
         IF EMP-DEPT = SPACES OR ZERO
*        OR EMP-DEPT IS NOT 'HR' OR
*           EMP-DEPT IS NOT 'ADMIN' OR
*           EMP-DEPT IS NOT 'SALES' OR
*           EMP-DEPT IS NOT 'TRAINING'
             MOVE -1 TO DEPTL
             MOVE 'N' TO WS-VALID-FLAG.
         IF EMP-DOJ-YY = SPACES OR ZERO AND
             EMP-DOJ-YY < EMP-DOB-YY
             MOVE -1 TO DOJYYL
             MOVE 'N' TO WS-VALID-FLAG
         ELSE
             IF EMP-DOJ-MM = SPACES OR ZERO OR
                EMP-DOJ-MM = 1 OR EMP-DOJ-MM = 3 OR
                EMP-DOJ-MM = 5 OR EMP-DOJ-MM = 7 OR
                EMP-DOJ-MM = 8 OR EMP-DOJ-MM = 10 OR
                EMP-DOJ-MM = 12 AND
                EMP-DOJ-DD = SPACES OR ZERO OR
                EMP-DOJ-DD > 31
                 MOVE -1 TO DOJDDL
                 MOVE 'N' TO WS-VALID-FLAG.
               IF EMP-DOJ-DD > 30
                 MOVE -1 TO DOJDDL
                 MOVE 'N' TO WS-VALID-FLAG
               IF EMP-DOJ-MM = 2 AND EMP-DOJ-DD > 28
                 MOVE -1 TO DOJDDL
                 MOVE 'N' TO WS-VALID-FLAG.
         IF EMP-DOB-YY = SPACES OR ZERO AND
             EMP-DOB-YY > EMP-DOJ-YY
             MOVE -1 TO DOBYYL
             MOVE 'N' TO WS-VALID-FLAG
         ELSE
             IF EMP-DOB-MM = SPACES OR ZERO OR
                EMP-DOB-MM = 1 OR EMP-DOJ-MM = 3 OR
                EMP-DOB-MM = 5 OR EMP-DOJ-MM = 7 OR
                EMP-DOB-MM = 8 OR EMP-DOJ-MM = 10 OR
                EMP-DOB-MM = 12 AND
                EMP-DOB-DD = SPACES OR ZERO OR
                EMP-DOB-DD > 31
                 MOVE -1 TO DOBDDL
                 MOVE 'N' TO WS-VALID-FLAG.
               IF EMP-DOB-DD > 30
                 MOVE -1 TO DOBDDL
                 MOVE 'N' TO WS-VALID-FLAG
               IF EMP-DOB-MM = 2 AND EMP-DOB-DD > 28
                 MOVE -1 TO DOBDDL
                 MOVE 'N' TO WS-VALID-FLAG.
     MOVE 'Y' TO WS-VALID-FLAG.
********************************************************
 MOVE-PARA.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'MOVE-PARA       ' TO WS-TAB(PARA-SUB)
     END-IF
     MOVE WS-EMP-ID             TO EMP-ID
     MOVE NAMEI                 TO EMP-NAME
     MOVE ADDRI                  TO EMP-ADDR
     MOVE DEPTI                 TO EMP-DEPT
     MOVE DOBDDI                TO EMP-DOB-DD
     MOVE DOBMMI                TO EMP-DOB-MM
     MOVE DOBYYI                TO EMP-DOB-YY
     MOVE DOJDDI                TO EMP-DOJ-DD
     MOVE DOJMMI                TO EMP-DOJ-MM
     MOVE DOJYYI                TO EMP-DOJ-YY.
********************************************************
 DATE-TIME-PARA.
     ADD 1 TO PARA-SUB
     IF PARA-SUB < 13
        MOVE 'DATE-TIME-PARA  ' TO WS-TAB(PARA-SUB)
     END-IF
     EXEC CICS ASKTIME
         ABSTIME(WS-DATE-TIME)
     END-EXEC
     EXEC CICS FORMATTIME
         ABSTIME(WS-DATE-TIME)
         DDMMYYYY(DATE1O)
         DATESEP
     END-EXEC
     MOVE DATE1O TO DATE2O.
 
******************************************************************* 
 
 NAME  P26AP04
 IDENTIFICATION DIVISION.
 PROGRAM-ID. P26AP04.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01  WS-RESP-CODE            PIC S9(08) COMP.
 01  WS-TABLE.
     03  WS-TAB           PIC X(16) OCCURS 12 TIMES.
 01  WS-RESP-CODE1           PIC S9(08) COMP.
 01  WS-CA.
     03  WS-PROCESS-FLAG     PIC X(01) VALUE SPACES.
         88  PROCESS-KEY            VALUE '1'.
         88  PROCESS-ADD               VALUE '2'.
         88  PROCESS-UPD-DEL           VALUE '3'.
         88  PRCOESS-DET            VALUE '2', '3'.
     03  WS-ID               PIC X(05).

 01  WS-SUB              PIC 9(02) VALUE ZERO.
 01  WS-EMP-ID           PIC X(05) VALUE SPACES.

 01  WS-DATE-TIME            PIC S9(15) COMP-3 VALUE ZERO.
 01  WS-DOB.
     03  WS-YY               PIC 9(04) VALUE ZERO.
     03  WS-MM               PIC 9(02) VALUE ZERO.
     03  WS-DD               PIC 9(02) VALUE ZERO.

 01  WS-EARN                 PIC ZZ,ZZ9.99 VALUE ZERO.
 01  WS-DEDN                 PIC ZZ,ZZ9.99 VALUE ZERO.
 01  WS-SAL-O                PIC ZZ,ZZ9.99 VALUE ZERO.
 01  WS-SAL                  PIC 9(05)V99 VALUE ZERO.

 01  WS-AMOUNT               PIC 9(05)V99 VALUE ZERO.

 01  FILLER  REDEFINES  WS-AMOUNT.
     03  WS-RS                PIC 9(05).
     03  WS-PS                PIC 9(02).

 01  WS-VALID-FLAG            PIC X(01) VALUE 'N'.
     88  VALID-DATA                     VALUE 'Y'.

     COPY P26AS04.
     COPY DFHAID.
     COPY DFHBMSCA.
     COPY EMPREC.

 LINKAGE SECTION.
 01  DFHCOMMAREA  PIC X(06).

**************************************************************

 PROCEDURE DIVISION.
 MAIN-PARA.
     ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'MAIN PARA       ' TO WS-TAB(WS-SUB)
         END-IF.
     IF EIBCALEN = ZERO
*       PERFORM SEND-MAP1
        PERFORM FIRST-PARA
     ELSE
        MOVE DFHCOMMAREA TO WS-CA
        PERFORM NEXT-PARA THRU NEXT-PARA-X
     END-IF.

 END-PARA.
     ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'END PARA        ' TO WS-TAB(WS-SUB)
         END-IF.
     EXEC CICS RETURN
          TRANSID('P26D')
          COMMAREA(WS-CA)
     END-EXEC.

 MAIN-MAP.
     ADD 1 TO WS-SUB
         IF WS-SUB < 1
             MOVE 'MAIN MAP        ' TO WS-TAB(WS-SUB)
         END-IF.
     EVALUATE EIBAID
        WHEN DFHENTER
           PERFORM RECEIVE-PROCESS-KEY
        WHEN DFHPF3
           EXEC CICS XCTL
              PROGRAM('P26AP02')
           END-EXEC
        WHEN OTHER
           MOVE 'INVALID KEY PRESSED' TO MSGO
     END-EVALUATE.

 FIRST-PARA.
     ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'FIRST PARA      ' TO WS-TAB(WS-SUB)
         END-IF.
     MOVE LOW-VALUES TO FSTMAPO
     PERFORM SEND-MAP1-PARA.

 SEND-MAP1-PARA.
     ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'MAP1-PARA       ' TO WS-TAB(WS-SUB)
         END-IF.
     PERFORM DATE-TIME-PARA.
     PERFORM SEND-MAP1.
     SET PROCESS-KEY TO TRUE.

 SEND-MAP1.
     ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'SEND-MAP1       ' TO WS-TAB(WS-SUB)
         END-IF.
     MOVE -1 TO ID1L
     EXEC CICS SEND
         MAP('FSTMAP') MAPSET('P26AS04')
         FROM(FSTMAPO) CURSOR ERASE
     END-EXEC.

 NEXT-PARA.
     ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'NEXT PARA       ' TO WS-TAB(WS-SUB)
         END-IF.
     EVALUATE TRUE
        WHEN PROCESS-KEY
           PERFORM MAIN-MAP
        WHEN PRCOESS-DET
           PERFORM DETAIL-PARA
     END-EVALUATE.

 SEND-PARA.
     ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'SEND PARA       ' TO WS-TAB(WS-SUB)
         END-IF.
     EVALUATE TRUE
        WHEN PROCESS-KEY
           PERFORM SEND-MAP1
        WHEN PRCOESS-DET
           PERFORM SEND-MAP2
     END-EVALUATE.

 NEXT-PARA-X.
     EXIT.

 RECEIVE-PROCESS-KEY.
     ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'REC-PRO-KEY     ' TO WS-TAB(WS-SUB)
         END-IF.
     PERFORM RECEIVE-MAP1
     IF ID1L = ZERO OR ID1I = SPACES
        MOVE 'PLEASE ENTER AN EMPLOYEE ID' TO MSGO
     ELSE
        PERFORM READ-PARA
     END-IF.

 RECEIVE-MAP1.
     ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'RECEIVE MAP 1   ' TO WS-TAB(WS-SUB)
         END-IF.
     EXEC CICS RECEIVE
         MAP('FSTMAP') MAPSET('P26AS04')
         INTO(FSTMAPI)
     END-EXEC.
*    PERFORM ID-CHECK-PARA.

*ID-CHECK-PARA.
*    ADD 1 TO WS-SUB
*        IF WS-SUB < 13
*            MOVE 'ID CHECK PARA   ' TO WS-TAB(WS-SUB)
*        END-IF.
*    PERFORM RECEIVE-MAP1
*    IF ID1L = ZERO
*        MOVE 'PLEASE ENTER AN EMPLOYEE ID' TO MSGO
*    ELSE
*        IF ID1I = SPACES
*           MOVE 'PLEASE ENTER A VALID EMPLOYEE ID' TO MSGO
*        ELSE
*           PERFORM READ-PARA
*        END-IF
*    END-IF.

 READ-PARA.
     ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'READ PARA       ' TO WS-TAB(WS-SUB)
         END-IF.
     MOVE LOW-VALUES TO SECMAPO
     MOVE ID1I TO WS-EMP-ID, EIDO
     EXEC CICS READ
         FILE('P26AFILE') RIDFLD(WS-EMP-ID)
         INTO(EMPLOYEE-RECORD) RESP(WS-RESP-CODE)
     END-EXEC.
     EVALUATE WS-RESP-CODE
         WHEN DFHRESP(NORMAL)
            MOVE EMP-NAME  TO NAMEO
            MOVE EMP-LOC   TO LOCO
            MOVE EMP-TECH  TO TECHO
            MOVE EMP-DOB(1:4) TO DOBYYO
            MOVE EMP-DOB(5:2) TO DOBMMO
            MOVE EMP-DOB(7:2) TO DOBDDO
            MOVE EMP-EARN  TO WS-AMOUNT
            MOVE WS-RS TO EARNRSO
            MOVE WS-PS TO EARNPSO
            MOVE EMP-DEDN  TO WS-AMOUNT
            MOVE WS-RS TO DEDNRSO
            MOVE WS-PS TO DEDNPSO
            COMPUTE WS-SAL = EMP-EARN - EMP-DEDN
            MOVE WS-SAL    TO WS-SAL-O
            MOVE WS-SAL-O TO SALO
            MOVE 'UPDATE / DELETE' TO MODEO
            MOVE -1                TO NAMEL
            SET PROCESS-UPD-DEL TO TRUE
            MOVE DFHBMDAR TO PF1A
            MOVE DFHBMPRO TO PF4A
            MOVE DFHBMPRO TO PF5A
         WHEN DFHRESP(NOTFND)
            MOVE -1                TO NAMEL
            MOVE SPACES TO NAMEO, LOCO, TECHO,
                 DOBYYO, DOBMMO, DOBDDO, SALO,
                 EARNRSO, EARNPSO, DEDNRSO, DEDNPSO
            MOVE 'ADD EMPLOYEE'  TO MODEO
            SET PROCESS-ADD TO TRUE
            MOVE DFHBMPRO TO PF1A
            MOVE DFHBMDAR TO PF4A
            MOVE DFHBMDAR TO PF5A
         WHEN OTHER
            MOVE 'ERROR PROCESSING FILE' TO MSGO
     END-EVALUATE.

 SEND-MAP2.
     ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'SEND-MAP2       ' TO WS-TAB(WS-SUB)
         END-IF.
     PERFORM DATE-TIME-PARA.
     EXEC CICS SEND
         MAP('SECMAP') MAPSET('P26AS04')
         FROM(SECMAPO) CURSOR ERASE
     END-EXEC.

 DETAIL-PARA.
     ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'DETAIL          ' TO WS-TAB(WS-SUB)
         END-IF.
     EVALUATE EIBAID
         WHEN DFHPF1
            IF PROCESS-ADD
               PERFORM ADD-PARA
            ELSE
               MOVE 'PF1 NOT ENABLED' TO MESSAGEO
            END-IF
         WHEN DFHPF3
            SET PROCESS-KEY TO TRUE
         WHEN DFHPF4
            IF PROCESS-UPD-DEL
               PERFORM UPDATE-PARA
            ELSE
               MOVE 'PF4 NOT ENABLED' TO MESSAGEO
            END-IF
         WHEN DFHPF5
            IF PROCESS-UPD-DEL
               PERFORM DELETE-PARA
            ELSE
               MOVE 'PF5 NOT ENABLED' TO MESSAGEO
            END-IF
         WHEN OTHER
            MOVE 'INVALID KEY PRESSED' TO MESSAGEO
     END-EVALUATE.

 RECEIVE-MAP2.
     ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'REC-MAP2        ' TO WS-TAB(WS-SUB)
         END-IF.
     EXEC CICS RECEIVE
         MAP('SECMAP') MAPSET('P26AS04')
         INTO(SECMAPI)
     END-EXEC.

 DELETE-PARA.
      ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'DELETE          ' TO WS-TAB(WS-SUB)
         END-IF.
      MOVE ID1I TO WS-EMP-ID
     EXEC CICS DELETE
         FILE('P26AFILE') RIDFLD(WS-EMP-ID)
         RESP(WS-RESP-CODE)
     END-EXEC
     EVALUATE WS-RESP-CODE
         WHEN DFHRESP(NORMAL)
            MOVE 'DELETE SUCCESSFUL' TO MSGO
         WHEN OTHER
            MOVE 'DELETE NOT SUCCESSFUL' TO MSGO
     END-EVALUATE
     SET PROCESS-KEY TO TRUE
     MOVE WS-EMP-ID TO ID1O.

 ADD-PARA.
     ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'ADD PARA        ' TO WS-TAB(WS-SUB)
         END-IF.
     PERFORM RECEIVE-MAP2
     PERFORM VALIDATE-PARA
     IF VALID-DATA
        PERFORM MOVE-PARA
        PERFORM WRITE-PARA
     END-IF.

 WRITE-PARA.
      ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'WRITE           ' TO WS-TAB(WS-SUB)
         END-IF.
     EXEC CICS WRITE
         FILE('P26AFILE') RIDFLD(WS-EMP-ID)
         FROM (EMPLOYEE-RECORD)
         RESP(WS-RESP-CODE)
     END-EXEC
     EVALUATE WS-RESP-CODE
         WHEN DFHRESP(NORMAL)
            MOVE 'ADD SUCCESSFUL' TO MSGO
         WHEN OTHER
            MOVE 'ADD NOT SUCCESSFUL' TO MSGO
     END-EVALUATE
     SET PROCESS-KEY TO TRUE
     MOVE WS-EMP-ID TO ID1O.

 UPDATE-PARA.
      ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'UPDATE          ' TO WS-TAB(WS-SUB)
         END-IF.
     PERFORM RECEIVE-MAP2
     PERFORM VALIDATE-PARA
     IF VALID-DATA
        PERFORM REWRITE-PARA
     END-IF.

 REWRITE-PARA.
      ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'REWRITE         ' TO WS-TAB(WS-SUB)
         END-IF.
         MOVE ID1I TO WS-EMP-ID
     EXEC CICS READ
         FILE('P26AFILE') RIDFLD(WS-EMP-ID)
         INTO(EMPLOYEE-RECORD) RESP(WS-RESP-CODE)
         UPDATE
     END-EXEC
     EVALUATE WS-RESP-CODE
         WHEN DFHRESP(NORMAL)
            PERFORM MOVE-PARA
            EXEC CICS REWRITE
                FILE('P26AFILE')
                FROM(EMPLOYEE-RECORD)
                RESP(WS-RESP-CODE1)
            END-EXEC
            EVALUATE WS-RESP-CODE1
              WHEN DFHRESP(NORMAL)
                 MOVE 'UPDATE SUCCESSFUL' TO MSGO
              WHEN OTHER
                 MOVE 'UPDATE NOT SUCCESSFUL' TO MSGO
            END-EVALUATE
         WHEN OTHER
            MOVE 'ERROR PROCESSING FILE' TO MSGO
     END-EVALUATE
     SET PROCESS-KEY TO TRUE
     MOVE WS-EMP-ID TO ID1O.

 VALIDATE-PARA.
      ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'VALIDATE        ' TO WS-TAB(WS-SUB)
         END-IF.
         IF EMP-NAME = SPACES OR ZERO
             MOVE -1 TO NAMEL.
         IF EMP-LOC = SPACES OR ZERO
             MOVE -1 TO LOCL.
         IF EMP-DOB = SPACES OR ZERO
             MOVE -1 TO DOBDDL.
         IF EMP-TECH = SPACES OR ZERO
             MOVE -1 TO TECHL.
         IF EMP-EARN = ZERO
             MOVE -1 TO EARNRSL.
         IF EMP-DEDN = ZERO
             MOVE -1 TO DEDNRSL.
     MOVE 'Y' TO WS-VALID-FLAG.

 MOVE-PARA.
     ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'MOVE-PARA       ' TO WS-TAB(WS-SUB)
         END-IF.
     MOVE WS-EMP-ID             TO EMP-ID
     MOVE NAMEI                 TO EMP-NAME
     MOVE LOCI                  TO EMP-LOC
     MOVE TECHI                 TO EMP-TECH
     MOVE DOBDDI                TO EMP-DOB(7:2)
     MOVE DOBMMI                TO EMP-DOB(5:2)
     MOVE DOBYYI                TO EMP-DOB(1:4)
     MOVE EARNRSI               TO WS-RS
     MOVE EARNPSI               TO WS-PS
     MOVE WS-AMOUNT             TO EMP-EARN
     MOVE DEDNRSI               TO WS-RS
     MOVE DEDNPSI               TO WS-PS
     MOVE WS-AMOUNT             TO EMP-DEDN.

 DATE-TIME-PARA.
     ADD 1 TO WS-SUB
         IF WS-SUB < 13
             MOVE 'DATE-TIME       ' TO WS-TAB(WS-SUB)
         END-IF.
     EXEC CICS ASKTIME
         ABSTIME(WS-DATE-TIME)
     END-EXEC
     EXEC CICS FORMATTIME
         ABSTIME(WS-DATE-TIME)
         DDMMYYYY(DATE1O)
         DATESEP
         TIME(TIME1O)
         TIMESEP
     END-EXEC
     MOVE DATE1O TO DATE2O
     MOVE TIME1O TO TIME2O.
