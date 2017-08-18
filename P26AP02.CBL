000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID. P26AP02.                                             00020000
000300 DATA DIVISION.                                                   00030000
000400 WORKING-STORAGE SECTION.                                         00040000
000500 01  WS-CA       PIC X(01).                                       00050000
000600 01  WS-CHOICE   PIC 9(01) VALUE ZERO.                            00060002
000700     88  VALID-CHOICE      VALUE 1 THRU 4.                        00070002
000800 01  WS-CHOICE-X REDEFINES WS-CHOICE PIC X(01).                   00080002
000900 01  WS-DATE-TIME PIC S9(15) COMP-3 VALUE ZERO.                   00090002
001000 01  WS-MESSAGE  PIC X(30) VALUE                                  00100000
001100     'END OF THE PROGRAM, BYE, BYE!!'.                            00110000
001200     COPY P26AS02.                                                00120002
001300     COPY DFHAID.                                                 00130000
001400 LINKAGE SECTION.                                                 00140000
001500 01  DFHCOMMAREA     PIC X(01).                                   00150000
001600 PROCEDURE DIVISION.                                              00160000
001700 MAIN-PARA.                                                       00170000
001800     IF EIBCALEN = ZERO                                           00180000
001900         PERFORM FIRST-PARA                                       00190000
002000     ELSE                                                         00200000
002100         PERFORM NEXT-PARA.                                       00210000
002200 END-PARA.                                                        00220000
002300     EXEC CICS RETURN                                             00230000
002400         TRANSID('P26B')                                          00240004
002500         COMMAREA(WS-CA)                                          00250000
002600     END-EXEC.                                                    00260000
002700 FIRST-PARA.                                                      00270000
002800     MOVE LOW-VALUES TO CHOICEO                                   00280004
002900     PERFORM DATE-TIME-PARA                                       00290005
003000     PERFORM SEND-MAP.                                            00300000
003100 SEND-MAP.                                                        00310000
003200     EXEC CICS SEND                                               00320000
003300         MAP('MENUMAP')                                           00330004
003400         MAPSET('P26AS02')                                        00340003
003500         FROM (MENUMAPI)                                          00350004
003600         ERASE                                                    00360000
003700     END-EXEC.                                                    00370000
003800 NEXT-PARA.                                                       00380000
003900     EVALUATE EIBAID                                              00390000
004000        WHEN DFHPF3                                               00400000
004100         EXEC CICS SEND TEXT                                      00410000
004200             FROM(WS-MESSAGE)                                     00420000
004300             ERASE                                                00430000
004400         END-EXEC                                                 00440000
004500         EXEC CICS RETURN                                         00450000
004600         END-EXEC                                                 00460000
004700        WHEN DFHENTER                                             00470000
004800            PERFORM PROCESS-PARA                                  00480002
004900        WHEN OTHER                                                00490000
005000            MOVE 'INVALID KEY PRESSED' TO MESSAGEO                00500002
005100     END-EVALUATE.                                                00510000
005200     PERFORM SEND-MAP.                                            00520000
005300 PROCESS-PARA.                                                    00530000
005400     PERFORM RECEIVE-MAP.                                         00540000
005500     MOVE CHOICEO TO WS-CHOICE-X                                  00550004
005600     IF VALID-CHOICE                                              00560002
005700         PERFORM TRANSFER-PARA                                    00570002
005800     ELSE                                                         00580002
005900         MOVE 'INVALID CHOICE' TO MESSAGEO                        00590002
006000     END-IF.                                                      00600002
006100 TRANSFER-PARA.                                                   00610002
006200     EVALUATE WS-CHOICE                                           00620002
006300        WHEN 1                                                    00630002
006400           EXEC CICS XCTL                                         00640002
006500              PROGRAM('P26AP01')                                  00650002
006600           END-EXEC                                               00660002
006700        WHEN 2                                                    00670002
006800           EXEC CICS XCTL                                         00680002
006900              PROGRAM('P26AP03')                                  00690002
007000           END-EXEC                                               00700002
007100        WHEN 3                                                    00710002
007200           EXEC CICS XCTL                                         00720002
007300              PROGRAM('P26AP04')                                  00730002
007400           END-EXEC                                               00740002
007500        WHEN 4                                                    00750002
007600           EXEC CICS XCTL                                         00760002
007700              PROGRAM('P26AP05')                                  00770002
007800           END-EXEC                                               00780002
007900        WHEN OTHER                                                00790002
008000           MOVE 'PROGRAM NOT READY' TO MESSAGEO                   00800002
008100     END-EVALUATE.                                                00810002
008200 RECEIVE-MAP.                                                     00820000
008300     EXEC CICS RECEIVE                                            00830000
008400         MAP('MENUMAP')                                           00840004
008500         MAPSET('P26AS02')                                        00850004
008600         INTO(MENUMAPO)                                           00860004
008700     END-EXEC.                                                    00870000
008800 DATE-TIME-PARA.                                                  00880002
008900     EXEC CICS ASKTIME                                            00890002
009000         ABSTIME(WS-DATE-TIME)                                    00900002
009100     END-EXEC.                                                    00910002
009200     EXEC CICS FORMATTIME                                         00920002
009300         ABSTIME(WS-DATE-TIME)                                    00930003
009400         DDMMYYYY(SYSDATEO)                                       00940005
009500         DATESEP                                                  00950003
009600         TIME(SYSTIMEO)                                           00960003
009700         TIMESEP                                                  00970003
009800     END-EXEC.                                                    00980003
