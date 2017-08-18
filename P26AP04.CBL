000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID. P26AP04.                                             00020000
000300 DATA DIVISION.                                                   00030000
000400 WORKING-STORAGE SECTION.                                         00040000
000500 01  WS-RESP-CODE            PIC S9(08) COMP.                     00050005
000600 01  WS-TABLE.                                                    00060005
000700     03  WS-TAB           PIC X(16) OCCURS 12 TIMES.              00070005
000800 01  WS-RESP-CODE1           PIC S9(08) COMP.                     00080011
000900 01  WS-CA.                                                       00090000
001000     03  WS-PROCESS-FLAG     PIC X(01) VALUE SPACES.              00100000
001100         88  PROCESS-KEY            VALUE '1'.                    00110000
001200         88  PROCESS-ADD               VALUE '2'.                 00120000
001300         88  PROCESS-UPD-DEL           VALUE '3'.                 00130000
001400         88  PRCOESS-DET            VALUE '2', '3'.               00140000
001500     03  WS-ID               PIC X(05).                           00150007
001600                                                                  00160000
001700 01  WS-SUB              PIC 9(02) VALUE ZERO.                    00170001
001800 01  WS-EMP-ID           PIC X(05) VALUE SPACES.                  00180001
001900                                                                  00190000
002000 01  WS-DATE-TIME            PIC S9(15) COMP-3 VALUE ZERO.        00200000
002100 01  WS-DOB.                                                      00210000
002200     03  WS-YY               PIC 9(04) VALUE ZERO.                00220000
002300     03  WS-MM               PIC 9(02) VALUE ZERO.                00230000
002400     03  WS-DD               PIC 9(02) VALUE ZERO.                00240000
002500                                                                  00250000
002600 01  WS-EARN                 PIC ZZ,ZZ9.99 VALUE ZERO.            00260000
002700 01  WS-DEDN                 PIC ZZ,ZZ9.99 VALUE ZERO.            00270000
002800 01  WS-SAL-O                PIC ZZ,ZZ9.99 VALUE ZERO.            00280000
002900 01  WS-SAL                  PIC 9(05)V99 VALUE ZERO.             00290000
003000                                                                  00300000
003100 01  WS-AMOUNT               PIC 9(05)V99 VALUE ZERO.             00310000
003200                                                                  00320000
003300 01  FILLER  REDEFINES  WS-AMOUNT.                                00330000
003400     03  WS-RS                PIC 9(05).                          00340000
003500     03  WS-PS                PIC 9(02).                          00350000
003600                                                                  00360000
003700 01  WS-VALID-FLAG            PIC X(01) VALUE 'N'.                00370000
003800     88  VALID-DATA                     VALUE 'Y'.                00380000
003900                                                                  00390000
004000     COPY P26AS04.                                                00400000
004100     COPY DFHAID.                                                 00410000
004200     COPY DFHBMSCA.                                               00420000
004300     COPY EMPREC.                                                 00430000
004400                                                                  00440000
004500 LINKAGE SECTION.                                                 00450000
004600 01  DFHCOMMAREA  PIC X(06).                                      00460000
004700                                                                  00470000
004800******************************************************************00480000
004900                                                                  00490000
005000 PROCEDURE DIVISION.                                              00500000
005100 MAIN-PARA.                                                       00510000
005200     ADD 1 TO WS-SUB                                              00520000
005300         IF WS-SUB < 13                                           00530000
005400             MOVE 'MAIN PARA       ' TO WS-TAB(WS-SUB)            00540000
005500         END-IF.                                                  00550000
005600     IF EIBCALEN = ZERO                                           00560000
005700*       PERFORM SEND-MAP1                                         00570000
005800        PERFORM FIRST-PARA                                        00580000
005900     ELSE                                                         00590000
006000        MOVE DFHCOMMAREA TO WS-CA                                 00600000
006100        PERFORM NEXT-PARA THRU NEXT-PARA-X                        00610000
006200     END-IF.                                                      00620000
006300                                                                  00630000
006400 END-PARA.                                                        00640000
006500     ADD 1 TO WS-SUB                                              00650002
006600         IF WS-SUB < 13                                           00660000
006700             MOVE 'END PARA        ' TO WS-TAB(WS-SUB)            00670000
006800         END-IF.                                                  00680000
006900     EXEC CICS RETURN                                             00690000
007000          TRANSID('P26D')                                         00700000
007100          COMMAREA(WS-CA)                                         00710000
007200     END-EXEC.                                                    00720000
007300                                                                  00730000
007400 MAIN-MAP.                                                        00740000
007500     ADD 1 TO WS-SUB                                              00750000
007600         IF WS-SUB < 1                                            00760000
007700             MOVE 'MAIN MAP        ' TO WS-TAB(WS-SUB)            00770000
007800         END-IF.                                                  00780000
007900     EVALUATE EIBAID                                              00790000
008000        WHEN DFHENTER                                             00800000
008100           PERFORM RECEIVE-PROCESS-KEY                            00810000
008200        WHEN DFHPF3                                               00820000
008300           EXEC CICS XCTL                                         00830000
008400              PROGRAM('P26AP02')                                  00840000
008500           END-EXEC                                               00850000
008600        WHEN OTHER                                                00860000
008700           MOVE 'INVALID KEY PRESSED' TO MSGO                     00870002
008800     END-EVALUATE.                                                00880000
008900                                                                  00890000
009000 FIRST-PARA.                                                      00900000
009100     ADD 1 TO WS-SUB                                              00910000
009200         IF WS-SUB < 13                                           00920000
009300             MOVE 'FIRST PARA      ' TO WS-TAB(WS-SUB)            00930000
009400         END-IF.                                                  00940000
009500     MOVE LOW-VALUES TO FSTMAPO                                   00950000
009600     PERFORM SEND-MAP1-PARA.                                      00960000
009700                                                                  00970000
009800 SEND-MAP1-PARA.                                                  00980000
009900     ADD 1 TO WS-SUB                                              00990004
010000         IF WS-SUB < 13                                           01000004
010100             MOVE 'MAP1-PARA       ' TO WS-TAB(WS-SUB)            01010004
010200         END-IF.                                                  01020004
010300     PERFORM DATE-TIME-PARA.                                      01030000
010400     PERFORM SEND-MAP1.                                           01040000
010500     SET PROCESS-KEY TO TRUE.                                     01050000
010600                                                                  01060000
010700 SEND-MAP1.                                                       01070000
010800     ADD 1 TO WS-SUB                                              01080004
010900         IF WS-SUB < 13                                           01090004
011000             MOVE 'SEND-MAP1       ' TO WS-TAB(WS-SUB)            01100004
011100         END-IF.                                                  01110004
011200     MOVE -1 TO ID1L                                              01120000
011300     EXEC CICS SEND                                               01130000
011400         MAP('FSTMAP') MAPSET('P26AS04')                          01140004
011500         FROM(FSTMAPO) CURSOR ERASE                               01150004
011600     END-EXEC.                                                    01160000
011700                                                                  01170000
011800 NEXT-PARA.                                                       01180000
011900     ADD 1 TO WS-SUB                                              01190000
012000         IF WS-SUB < 13                                           01200000
012100             MOVE 'NEXT PARA       ' TO WS-TAB(WS-SUB)            01210000
012200         END-IF.                                                  01220000
012300     EVALUATE TRUE                                                01230000
012400        WHEN PROCESS-KEY                                          01240000
012500           PERFORM MAIN-MAP                                       01250000
012600        WHEN PRCOESS-DET                                          01260000
012700           PERFORM DETAIL-PARA                                    01270000
012800     END-EVALUATE.                                                01280000
012900                                                                  01290000
013000 SEND-PARA.                                                       01300000
013100     ADD 1 TO WS-SUB                                              01310000
013200         IF WS-SUB < 13                                           01320000
013300             MOVE 'SEND PARA       ' TO WS-TAB(WS-SUB)            01330000
013400         END-IF.                                                  01340000
013500     EVALUATE TRUE                                                01350000
013600        WHEN PROCESS-KEY                                          01360000
013700           PERFORM SEND-MAP1                                      01370000
013800        WHEN PRCOESS-DET                                          01380000
013900           PERFORM SEND-MAP2                                      01390000
014000     END-EVALUATE.                                                01400000
014100                                                                  01410000
014200 NEXT-PARA-X.                                                     01420000
014300     EXIT.                                                        01430000
014400                                                                  01440000
014500 RECEIVE-PROCESS-KEY.                                             01450000
014600     ADD 1 TO WS-SUB                                              01460004
014700         IF WS-SUB < 13                                           01470004
014800             MOVE 'REC-PRO-KEY     ' TO WS-TAB(WS-SUB)            01480004
014900         END-IF.                                                  01490004
015000     PERFORM RECEIVE-MAP1                                         01500000
015100     IF ID1L = ZERO OR ID1I = SPACES                              01510000
015200        MOVE 'PLEASE ENTER AN EMPLOYEE ID' TO MSGO                01520002
015300     ELSE                                                         01530000
015400        PERFORM READ-PARA                                         01540000
015500     END-IF.                                                      01550000
015600                                                                  01560000
015700 RECEIVE-MAP1.                                                    01570000
015800     ADD 1 TO WS-SUB                                              01580000
015900         IF WS-SUB < 13                                           01590000
016000             MOVE 'RECEIVE MAP 1   ' TO WS-TAB(WS-SUB)            01600004
016100         END-IF.                                                  01610000
016200     EXEC CICS RECEIVE                                            01620000
016300         MAP('FSTMAP') MAPSET('P26AS04')                          01630004
016400         INTO(FSTMAPI)                                            01640000
016500     END-EXEC.                                                    01650000
016600*    PERFORM ID-CHECK-PARA.                                       01660000
016700                                                                  01670000
016800*ID-CHECK-PARA.                                                   01680004
016900*    ADD 1 TO WS-SUB                                              01690004
017000*        IF WS-SUB < 13                                           01700004
017100*            MOVE 'ID CHECK PARA   ' TO WS-TAB(WS-SUB)            01710004
017200*        END-IF.                                                  01720004
017300*    PERFORM RECEIVE-MAP1                                         01730004
017400*    IF ID1L = ZERO                                               01740004
017500*        MOVE 'PLEASE ENTER AN EMPLOYEE ID' TO MSGO               01750004
017600*    ELSE                                                         01760004
017700*        IF ID1I = SPACES                                         01770004
017800*           MOVE 'PLEASE ENTER A VALID EMPLOYEE ID' TO MSGO       01780004
017900*        ELSE                                                     01790004
018000*           PERFORM READ-PARA                                     01800004
018100*        END-IF                                                   01810004
018200*    END-IF.                                                      01820004
018300                                                                  01830000
018400 READ-PARA.                                                       01840000
018500     ADD 1 TO WS-SUB                                              01850000
018600         IF WS-SUB < 13                                           01860000
018700             MOVE 'READ PARA       ' TO WS-TAB(WS-SUB)            01870000
018800         END-IF.                                                  01880000
018900     MOVE LOW-VALUES TO SECMAPO                                   01890002
019000     MOVE ID1I TO WS-EMP-ID, EIDO                                 01900002
019100     EXEC CICS READ                                               01910000
019200         FILE('P26AFILE') RIDFLD(WS-EMP-ID)                       01920004
019300         INTO(EMPLOYEE-RECORD) RESP(WS-RESP-CODE)                 01930004
019400     END-EXEC.                                                    01940000
019500     EVALUATE WS-RESP-CODE                                        01950000
019600         WHEN DFHRESP(NORMAL)                                     01960000
019700            MOVE EMP-NAME  TO NAMEO                               01970000
019800            MOVE EMP-LOC   TO LOCO                                01980000
019900            MOVE EMP-TECH  TO TECHO                               01990000
020000            MOVE EMP-DOB(1:4) TO DOBYYO                           02000000
020100            MOVE EMP-DOB(5:2) TO DOBMMO                           02010000
020200            MOVE EMP-DOB(7:2) TO DOBDDO                           02020000
020300            MOVE EMP-EARN  TO WS-AMOUNT                           02030000
020400            MOVE WS-RS TO EARNRSO                                 02040002
020500            MOVE WS-PS TO EARNPSO                                 02050002
020600            MOVE EMP-DEDN  TO WS-AMOUNT                           02060000
020700            MOVE WS-RS TO DEDNRSO                                 02070002
020800            MOVE WS-PS TO DEDNPSO                                 02080002
020900            COMPUTE WS-SAL = EMP-EARN - EMP-DEDN                  02090000
021000            MOVE WS-SAL    TO WS-SAL-O                            02100002
021100            MOVE WS-SAL-O TO SALO                                 02110002
021200            MOVE 'UPDATE / DELETE' TO MODEO                       02120000
021300            MOVE -1                TO NAMEL                       02130000
021400            SET PROCESS-UPD-DEL TO TRUE                           02140000
021500            MOVE DFHBMDAR TO PF1A                                 02150004
021600            MOVE DFHBMPRO TO PF4A                                 02160004
021700            MOVE DFHBMPRO TO PF5A                                 02170004
021800         WHEN DFHRESP(NOTFND)                                     02180000
021900            MOVE -1                TO NAMEL                       02190000
022000            MOVE SPACES TO NAMEO, LOCO, TECHO,                    02200000
022100                 DOBYYO, DOBMMO, DOBDDO, SALO,                    02210000
022200                 EARNRSO, EARNPSO, DEDNRSO, DEDNPSO               02220000
022300            MOVE 'ADD EMPLOYEE'  TO MODEO                         02230000
022400            SET PROCESS-ADD TO TRUE                               02240000
022500            MOVE DFHBMPRO TO PF1A                                 02250004
022600            MOVE DFHBMDAR TO PF4A                                 02260004
022700            MOVE DFHBMDAR TO PF5A                                 02270004
022800         WHEN OTHER                                               02280004
022900            MOVE 'ERROR PROCESSING FILE' TO MSGO                  02290004
023000     END-EVALUATE.                                                02300004
023100                                                                  02310004
023200 SEND-MAP2.                                                       02320004
023300     ADD 1 TO WS-SUB                                              02330004
023400         IF WS-SUB < 13                                           02340004
023500             MOVE 'SEND-MAP2       ' TO WS-TAB(WS-SUB)            02350004
023600         END-IF.                                                  02360004
023700     PERFORM DATE-TIME-PARA.                                      02370004
023800     EXEC CICS SEND                                               02380004
023900         MAP('SECMAP') MAPSET('P26AS04')                          02390004
024000         FROM(SECMAPO) CURSOR ERASE                               02400004
024100     END-EXEC.                                                    02410004
024200                                                                  02420004
024300 DETAIL-PARA.                                                     02430004
024400     ADD 1 TO WS-SUB                                              02440004
024500         IF WS-SUB < 13                                           02450004
024600             MOVE 'DETAIL          ' TO WS-TAB(WS-SUB)            02460004
024700         END-IF.                                                  02470004
024800     EVALUATE EIBAID                                              02480004
024900         WHEN DFHPF1                                              02490004
025000            IF PROCESS-ADD                                        02500004
025100               PERFORM ADD-PARA                                   02510004
025200            ELSE                                                  02520004
025300               MOVE 'PF1 NOT ENABLED' TO MESSAGEO                 02530004
025400            END-IF                                                02540004
025500         WHEN DFHPF3                                              02550004
025600            SET PROCESS-KEY TO TRUE                               02560004
025700         WHEN DFHPF4                                              02570004
025800            IF PROCESS-UPD-DEL                                    02580004
025900               PERFORM UPDATE-PARA                                02590004
026000            ELSE                                                  02600004
026100               MOVE 'PF4 NOT ENABLED' TO MESSAGEO                 02610004
026200            END-IF                                                02620004
026300         WHEN DFHPF5                                              02630004
026400            IF PROCESS-UPD-DEL                                    02640004
026500               PERFORM DELETE-PARA                                02650004
026600            ELSE                                                  02660004
026700               MOVE 'PF5 NOT ENABLED' TO MESSAGEO                 02670004
026800            END-IF                                                02680004
026900         WHEN OTHER                                               02690004
027000            MOVE 'INVALID KEY PRESSED' TO MESSAGEO                02700004
027100     END-EVALUATE.                                                02710004
027200                                                                  02720004
027300 RECEIVE-MAP2.                                                    02730004
027400     ADD 1 TO WS-SUB                                              02740004
027500         IF WS-SUB < 13                                           02750004
027600             MOVE 'REC-MAP2        ' TO WS-TAB(WS-SUB)            02760004
027700         END-IF.                                                  02770004
027800     EXEC CICS RECEIVE                                            02780004
027900         MAP('SECMAP') MAPSET('P26AS04')                          02790004
028000         INTO(SECMAPI)                                            02800004
028100     END-EXEC.                                                    02810004
028200                                                                  02820004
028300 DELETE-PARA.                                                     02830004
028400      ADD 1 TO WS-SUB                                             02840004
028500         IF WS-SUB < 13                                           02850004
028600             MOVE 'DELETE          ' TO WS-TAB(WS-SUB)            02860004
028700         END-IF.                                                  02870004
028800      MOVE ID1I TO WS-EMP-ID                                      02880010
028900     EXEC CICS DELETE                                             02890004
029000         FILE('P26AFILE') RIDFLD(WS-EMP-ID)                       02900004
029100         RESP(WS-RESP-CODE)                                       02910004
029200     END-EXEC                                                     02920004
029300     EVALUATE WS-RESP-CODE                                        02930004
029400         WHEN DFHRESP(NORMAL)                                     02940004
029500            MOVE 'DELETE SUCCESSFUL' TO MSGO                      02950004
029600         WHEN OTHER                                               02960004
029700            MOVE 'DELETE NOT SUCCESSFUL' TO MSGO                  02970004
029800     END-EVALUATE                                                 02980004
029900     SET PROCESS-KEY TO TRUE                                      02990004
030000     MOVE WS-EMP-ID TO ID1O.                                      03000004
030100                                                                  03010004
030200 ADD-PARA.                                                        03020004
030300     ADD 1 TO WS-SUB                                              03030004
030400         IF WS-SUB < 13                                           03040004
030500             MOVE 'ADD PARA        ' TO WS-TAB(WS-SUB)            03050004
030600         END-IF.                                                  03060004
030700     PERFORM RECEIVE-MAP2                                         03070004
030800     PERFORM VALIDATE-PARA                                        03080004
030900     IF VALID-DATA                                                03090004
031000        PERFORM MOVE-PARA                                         03100004
031100        PERFORM WRITE-PARA                                        03110004
031200     END-IF.                                                      03120004
031300                                                                  03130004
031400 WRITE-PARA.                                                      03140004
031500      ADD 1 TO WS-SUB                                             03150004
031600         IF WS-SUB < 13                                           03160004
031700             MOVE 'WRITE           ' TO WS-TAB(WS-SUB)            03170004
031800         END-IF.                                                  03180004
031900     EXEC CICS WRITE                                              03190004
032000         FILE('P26AFILE') RIDFLD(WS-EMP-ID)                       03200004
032100         FROM (EMPLOYEE-RECORD)                                   03210004
032200         RESP(WS-RESP-CODE)                                       03220004
032300     END-EXEC                                                     03230004
032400     EVALUATE WS-RESP-CODE                                        03240004
032500         WHEN DFHRESP(NORMAL)                                     03250004
032600            MOVE 'ADD SUCCESSFUL' TO MSGO                         03260004
032700         WHEN OTHER                                               03270004
032800            MOVE 'ADD NOT SUCCESSFUL' TO MSGO                     03280004
032900     END-EVALUATE                                                 03290004
033000     SET PROCESS-KEY TO TRUE                                      03300004
033100     MOVE WS-EMP-ID TO ID1O.                                      03310004
033200                                                                  03320004
033300 UPDATE-PARA.                                                     03330004
033400      ADD 1 TO WS-SUB                                             03340004
033500         IF WS-SUB < 13                                           03350004
033600             MOVE 'UPDATE          ' TO WS-TAB(WS-SUB)            03360004
033700         END-IF.                                                  03370004
033800     PERFORM RECEIVE-MAP2                                         03380004
033900     PERFORM VALIDATE-PARA                                        03390004
034000     IF VALID-DATA                                                03400004
034100        PERFORM REWRITE-PARA                                      03410004
034200     END-IF.                                                      03420004
034300                                                                  03430004
034400 REWRITE-PARA.                                                    03440004
034500      ADD 1 TO WS-SUB                                             03450004
034600         IF WS-SUB < 13                                           03460004
034700             MOVE 'REWRITE         ' TO WS-TAB(WS-SUB)            03470004
034800         END-IF.                                                  03480004
034900         MOVE ID1I TO WS-EMP-ID                                   03490008
035000     EXEC CICS READ                                               03500004
035100         FILE('P26AFILE') RIDFLD(WS-EMP-ID)                       03510007
035200         INTO(EMPLOYEE-RECORD) RESP(WS-RESP-CODE)                 03520004
035300         UPDATE                                                   03530004
035400     END-EXEC                                                     03540004
035500     EVALUATE WS-RESP-CODE                                        03550004
035600         WHEN DFHRESP(NORMAL)                                     03560004
035700            PERFORM MOVE-PARA                                     03570004
035800            EXEC CICS REWRITE                                     03580004
035900                FILE('P26AFILE')                                  03590004
036000                FROM(EMPLOYEE-RECORD)                             03600004
036100                RESP(WS-RESP-CODE1)                               03610006
036200            END-EXEC                                              03620004
036300            EVALUATE WS-RESP-CODE1                                03630006
036400              WHEN DFHRESP(NORMAL)                                03640004
036500                 MOVE 'UPDATE SUCCESSFUL' TO MSGO                 03650004
036600              WHEN OTHER                                          03660004
036700                 MOVE 'UPDATE NOT SUCCESSFUL' TO MSGO             03670004
036800            END-EVALUATE                                          03680004
036900         WHEN OTHER                                               03690004
037000            MOVE 'ERROR PROCESSING FILE' TO MSGO                  03700004
037100     END-EVALUATE                                                 03710004
037200     SET PROCESS-KEY TO TRUE                                      03720004
037300     MOVE WS-EMP-ID TO ID1O.                                      03730004
037400                                                                  03740004
037500 VALIDATE-PARA.                                                   03750004
037600      ADD 1 TO WS-SUB                                             03760004
037700         IF WS-SUB < 13                                           03770004
037800             MOVE 'VALIDATE        ' TO WS-TAB(WS-SUB)            03780004
037900         END-IF.                                                  03790004
038000         IF EMP-NAME = SPACES OR ZERO                             03800004
038100             MOVE -1 TO NAMEL.                                    03810004
038200         IF EMP-LOC = SPACES OR ZERO                              03820004
038300             MOVE -1 TO LOCL.                                     03830004
038400         IF EMP-DOB = SPACES OR ZERO                              03840004
038500             MOVE -1 TO DOBDDL.                                   03850004
038600         IF EMP-TECH = SPACES OR ZERO                             03860004
038700             MOVE -1 TO TECHL.                                    03870004
038800         IF EMP-EARN = ZERO                                       03880004
038900             MOVE -1 TO EARNRSL.                                  03890004
039000         IF EMP-DEDN = ZERO                                       03900004
039100             MOVE -1 TO DEDNRSL.                                  03910004
039200     MOVE 'Y' TO WS-VALID-FLAG.                                   03920004
039300                                                                  03930004
039400 MOVE-PARA.                                                       03940004
039500     ADD 1 TO WS-SUB                                              03950004
039600         IF WS-SUB < 13                                           03960004
039700             MOVE 'MOVE-PARA       ' TO WS-TAB(WS-SUB)            03970004
039800         END-IF.                                                  03980004
039900     MOVE WS-EMP-ID             TO EMP-ID                         03990004
040000     MOVE NAMEI                 TO EMP-NAME                       04000004
040100     MOVE LOCI                  TO EMP-LOC                        04010004
040200     MOVE TECHI                 TO EMP-TECH                       04020004
040300     MOVE DOBDDI                TO EMP-DOB(7:2)                   04030004
040400     MOVE DOBMMI                TO EMP-DOB(5:2)                   04040004
040500     MOVE DOBYYI                TO EMP-DOB(1:4)                   04050004
040600     MOVE EARNRSI               TO WS-RS                          04060004
040700     MOVE EARNPSI               TO WS-PS                          04070004
040800     MOVE WS-AMOUNT             TO EMP-EARN                       04080004
040900     MOVE DEDNRSI               TO WS-RS                          04090004
041000     MOVE DEDNPSI               TO WS-PS                          04100004
041100     MOVE WS-AMOUNT             TO EMP-DEDN.                      04110004
041200                                                                  04120004
041300 DATE-TIME-PARA.                                                  04130004
041400     ADD 1 TO WS-SUB                                              04140004
041500         IF WS-SUB < 13                                           04150004
041600             MOVE 'DATE-TIME       ' TO WS-TAB(WS-SUB)            04160004
041700         END-IF.                                                  04170004
041800     EXEC CICS ASKTIME                                            04180004
041900         ABSTIME(WS-DATE-TIME)                                    04190004
042000     END-EXEC                                                     04200004
042100     EXEC CICS FORMATTIME                                         04210004
042200         ABSTIME(WS-DATE-TIME)                                    04220004
042300         DDMMYYYY(DATE1O)                                         04230004
042400         DATESEP                                                  04240004
042500         TIME(TIME1O)                                             04250004
042600         TIMESEP                                                  04260004
042700     END-EXEC                                                     04270004
042800     MOVE DATE1O TO DATE2O                                        04280004
042900     MOVE TIME1O TO TIME2O.                                       04290004
