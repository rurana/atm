000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID. P26AP01.                                             00020000
000300 DATA DIVISION.                                                   00030000
000400 WORKING-STORAGE SECTION.                                         00040000
000500 01  WS-CA       PIC X(01).                                       00050000
000600 01  WS-IN1      PIC 9(02).                                       00060000
000700 01  WS-IN1O REDEFINES WS-IN1 PIC X(02).                          00070000
000800 01  WS-IN2      PIC 9(02).                                       00080000
000900 01  WS-IN2O REDEFINES WS-IN2 PIC X(02).                          00090000
001000 01  WS-ADD      PIC 9(03).                                       00100000
001100 01  OP-ADD      PIC ZZ9.                                         00110001
001200 01  WS-SUB      PIC S9(03).                                      00120002
001300 01  OP-SUB      PIC -9(03).                                      00130000
001400 01  WS-MUL      PIC 9(03).                                       00140000
001500 01  OP-MUL      PIC ZZ9.                                         00150001
001600 01  WS-DIV      PIC 9(01)V99.                                    00160002
001700 01  OP-DIV      PIC 9(01).99.                                    00170001
001800 01  WS-MESSAGE  PIC X(30) VALUE                                  00180001
001900     'END OF THE PROGRAM, BYE, BYE!!'.                            00190001
002000     COPY P26AS01.                                                00200001
002100     COPY DFHAID.                                                 00210001
002200 LINKAGE SECTION.                                                 00220001
002300 01  DFHCOMMAREA     PIC X(01).                                   00230001
002400 PROCEDURE DIVISION.                                              00240001
002500 MAIN-PARA.                                                       00250001
002600     IF EIBCALEN = ZERO                                           00260001
002700         PERFORM FIRST-PARA                                       00270001
002800     ELSE                                                         00280001
002900         PERFORM NEXT-PARA.                                       00290001
003000 END-PARA.                                                        00300001
003100     EXEC CICS RETURN                                             00310001
003200         TRANSID('P26A')                                          00320001
003300         COMMAREA(WS-CA)                                          00330001
003400     END-EXEC.                                                    00340001
003500 FIRST-PARA.                                                      00350001
003600     MOVE LOW-VALUES TO CALCO                                     00360001
003700     PERFORM SEND-MAP.                                            00370001
003800 SEND-MAP.                                                        00380001
003900     EXEC CICS SEND                                               00390001
004000         MAP('CALC')                                              00400001
004100         MAPSET('P26AS01')                                        00410001
004200         FROM (CALCO)                                             00420001
004300         ERASE                                                    00430001
004400     END-EXEC.                                                    00440001
004500 NEXT-PARA.                                                       00450001
004600     EVALUATE EIBAID                                              00460001
004700        WHEN DFHPF3                                               00470001
004800         EXEC CICS SEND TEXT                                      00480001
004900             FROM(WS-MESSAGE)                                     00490001
005000             ERASE                                                00500001
005100         END-EXEC                                                 00510001
005200         EXEC CICS RETURN                                         00520001
005300         END-EXEC                                                 00530001
005400        WHEN DFHENTER                                             00540001
005500         PERFORM PROCESS-PARA                                     00550001
005600        WHEN OTHER                                                00560001
005700         MOVE 'INVALID KEY PRESSED' TO MESSAGEO                   00570001
005800     END-EVALUATE.                                                00580001
005900     PERFORM SEND-MAP.                                            00590001
006000 PROCESS-PARA.                                                    00600001
006100     PERFORM RECEIVE-MAP.                                         00610001
006200     IF INPUT1L = ZERO OR INPUT2L = ZERO                          00620001
006300         MOVE 'PLEASE ENTER VALID INPUTS' TO MESSAGEO             00630001
006400     ELSE                                                         00640001
006500        IF INPUT1I IS NUMERIC AND INPUT2I IS NUMERIC              00650001
006600             MOVE INPUT1I TO WS-IN1O                              00660001
006700             MOVE INPUT2I TO WS-IN2O                              00670001
006800             COMPUTE WS-ADD = WS-IN1 + WS-IN2                     00680001
006900             COMPUTE WS-SUB = WS-IN1 - WS-IN2                     00690001
007000             COMPUTE WS-MUL = WS-IN1 * WS-IN2                     00700001
007100             COMPUTE WS-DIV = WS-IN1 / WS-IN2                     00710001
007200             MOVE WS-ADD  TO ADDO                                 00720001
007300*            MOVE WS-SUB TO OP-SUB                                00730001
007400             MOVE WS-SUB  TO SUBO                                 00740001
007500             MOVE WS-MUL  TO MULO                                 00750001
007600             MOVE WS-DIV TO OP-DIV                                00760002
007700             MOVE OP-DIV  TO DIVO                                 00770002
007800             MOVE 'SUM OF 2 NUMBERS IS DISPLAYED' TO MESSAGEO     00780001
007900        ELSE                                                      00790001
008000             MOVE 'INPUTS ARE NOT NUMERIC' TO MESSAGEO.           00800001
008100 EXIT-PARA.                                                       00810003
008200     EVALUATE EIBAID                                              00820003
008300        WHEN DFHPF3                                               00830003
008400           EXEC CICS XCTL                                         00840003
008500              PROGRAM('P26AP02')                                  00850003
008600           END-EXEC                                               00860003
008700     END-EVALUATE.                                                00870003
008800                                                                  00880003
008900 RECEIVE-MAP.                                                     00890001
009000     EXEC CICS RECEIVE                                            00900001
009100         MAP('CALC')                                              00910001
009200         MAPSET('P26AS01')                                        00920001
009300         INTO(CALCI)                                              00930001
009400     END-EXEC.                                                    00940001
