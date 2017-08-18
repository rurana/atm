000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID. P26AP03.                                             00020000
000300 DATA DIVISION.                                                   00030000
000400 WORKING-STORAGE SECTION.                                         00040000
000500 01  WS-CA       PIC X(01).                                       00050000
000600 01  WS-RESP-CODE PIC S9(08) COMP VALUE ZERO.                     00060001
000700 01  WS-EARN     PIC ZZ,ZZ9.99 VALUE ZERO.                        00070001
000800 01  WS-DEDN     PIC ZZ,ZZ9.99 VALUE ZERO.                        00080001
000900 01  WS-SAL-O    PIC ZZ,ZZ9.99 VALUE ZERO.                        00090001
001000 01  WS-SAL      PIC 9(05)V99 VALUE ZERO.                         00100001
001100 01  WS-DATE-TIME PIC S9(15) COMP-3 VALUE ZERO.                   00110000
001200     COPY EMPREC.                                                 00120001
001300     COPY P26AS03.                                                00130000
001400     COPY DFHAID.                                                 00140000
001500 LINKAGE SECTION.                                                 00150000
001600 01  DFHCOMMAREA     PIC X(01).                                   00160000
001700 PROCEDURE DIVISION.                                              00170000
001800 MAIN-PARA.                                                       00180000
001900     IF EIBCALEN = ZERO                                           00190000
002000         PERFORM FIRST-PARA                                       00200000
002100     ELSE                                                         00210000
002200         PERFORM NEXT-PARA.                                       00220000
002300 END-PARA.                                                        00230000
002400     EXEC CICS RETURN                                             00240000
002500         TRANSID('P26C')                                          00250001
002600         COMMAREA(WS-CA)                                          00260000
002700     END-EXEC.                                                    00270000
002800 FIRST-PARA.                                                      00280000
002900     MOVE LOW-VALUES TO EMPINQO                                   00290002
003000     PERFORM DATE-TIME-PARA                                       00300000
003100     PERFORM SEND-MAP.                                            00310000
003200 SEND-MAP.                                                        00320000
003300     EXEC CICS SEND                                               00330000
003400         MAP('EMPINQ')                                            00340001
003500         MAPSET('P26AS03')                                        00350001
003600         FROM (EMPINQI)                                           00360001
003700         ERASE                                                    00370000
003800     END-EXEC.                                                    00380000
003900     PERFORM DATE-TIME-PARA.                                      00390005
004000 NEXT-PARA.                                                       00400000
004100     EVALUATE EIBAID                                              00410000
004200        WHEN DFHPF3                                               00420000
004300         EXEC CICS XCTL                                           00430001
004400             PROGRAM('P26AP02')                                   00440001
004500         END-EXEC                                                 00450002
004600        WHEN DFHENTER                                             00460000
004700            PERFORM PROCESS-PARA                                  00470000
004800        WHEN OTHER                                                00480000
004900            MOVE 'INVALID KEY PRESSED' TO MESSAGEO                00490000
005000     END-EVALUATE                                                 00500002
005100     PERFORM SEND-MAP.                                            00510000
005200 PROCESS-PARA.                                                    00520000
005300     PERFORM RECEIVE-MAP.                                         00530000
005400     IF IDL = ZERO                                                00540001
005500         MOVE 'PLEASE ENTER AN EMPLOYEE ID' TO MESSAGEO           00550001
005600     ELSE                                                         00560000
005700         IF IDI = SPACES                                          00570001
005800            MOVE 'PLEASE ENTER A VALID EMPLOYEE ID' TO MESSAGEO   00580001
005900         ELSE                                                     00590001
006000            PERFORM READ-EMPLOYEE                                 00600001
006100         END-IF                                                   00610002
006200     END-IF.                                                      00620000
006300 READ-EMPLOYEE.                                                   00630001
006400     EXEC CICS READ                                               00640001
006500         FILE('P26AFILE')                                         00650001
006600         RIDFLD(IDI)                                              00660001
006700         INTO(EMPLOYEE-RECORD)                                    00670002
006800         RESP(WS-RESP-CODE)                                       00680001
006900     END-EXEC.                                                    00690001
007000     EVALUATE WS-RESP-CODE                                        00700001
007100         WHEN DFHRESP(NORMAL)                                     00710001
007200             MOVE EMP-ID   TO IDO                                 00720001
007300             MOVE EMP-NAME TO NAMEO                               00730001
007400             MOVE EMP-TECH TO TECHO                               00740001
007500             MOVE EMP-LOC  TO LOCO                                00750001
007600             MOVE EMP-EARN TO WS-EARN                             00760001
007700             PERFORM MOVE-DOB                                     00770001
007800             MOVE WS-EARN  TO EARNO                               00780001
007900             MOVE EMP-DEDN TO WS-DEDN                             00790002
008000             MOVE WS-DEDN  TO DEDNO                               00800001
008100             COMPUTE WS-SAL = EMP-EARN - EMP-DEDN                 00810002
008200             MOVE WS-SAL   TO WS-SAL-O                            00820001
008300             MOVE WS-SAL-O TO SALO                                00830001
008400             MOVE 'EMPLOYEE DETAILS FOUND' TO MESSAGEO            00840001
008500         WHEN DFHRESP(NOTFND)                                     00850002
008600             MOVE SPACES   TO NAMEO, DOBO, TECHO, LOCO,           00860003
008700                              EARNO, DEDNO, SALO                  00870001
008800             MOVE 'EMPLOYEE NOT FOUND    ' TO MESSAGEO            00880001
008900         WHEN OTHER                                               00890001
009000             MOVE 'ERROR ACCESSING FILE  ' TO MESSAGEO            00900001
009100             MOVE SPACES   TO NAMEO, DOBO, TECHO, LOCO,           00910001
009200                              EARNO, DEDNO, SALO                  00920001
009300     END-EVALUATE.                                                00930001
009400 MOVE-DOB.                                                        00940001
009500     MOVE EMP-DOB TO DOBO.                                        00950001
009600 RECEIVE-MAP.                                                     00960000
009700     EXEC CICS RECEIVE                                            00970000
009800         MAP('EMPINQ')                                            00980001
009900         MAPSET('P26AS03')                                        00990001
010000         INTO(EMPINQI)                                            01000001
010100     END-EXEC.                                                    01010000
010200 DATE-TIME-PARA.                                                  01020000
010300     EXEC CICS ASKTIME                                            01030000
010400         ABSTIME(WS-DATE-TIME)                                    01040000
010500     END-EXEC.                                                    01050000
010600     EXEC CICS FORMATTIME                                         01060000
010700         ABSTIME(WS-DATE-TIME)                                    01070000
010800         DDMMYYYY(SYSDATEO)                                       01080000
010900         DATESEP                                                  01090000
011000         TIME(SYSTIMEO)                                           01100000
011100         TIMESEP                                                  01110000
011200     END-EXEC.                                                    01120000
