         TITLE 'PLISTRIP - PL/I EQUIVALENT OF REXX ''STRIP'' FUNCTION'  00170001
PLISTRIP CSECT                                                          00180001
*********************************************************************** 00190001
*                                                                     * 00200001
*        PLISTRIP: PROVIDE PL/I EQUIVALENT OF REXX 'STRIP' FUNCTION.  * 00210001
*                                                                     * 00230001
*        AUTHOR: PETER FLASS - NYS LEGISLATIVE BILL DRAFTING COMM     * 00240001
*                MARCH, 1996                                          * 00250001
*                                                                     * 00260001
*        FUNCTION: PROGRAM IS CALLED WITH A CHARACTER STRING          * 00270001
*                  CONTAINING A LIST OF THE DATA ITEMS TO BE          * 00280001
*                  OBTAINED AND THE ADDRESS OF A BUFFER FOR THE       * 00290001
*                  RETURNED INFORMATION.  THE LIST IS SCANNED AND     * 00300001
*                  THE REQUESTED ITEMS ARE MOVED ONE AT A TIME TO     * 00310001
*                  THE CALLERS BUFFER.  UNRECOGNIZED LIST ITEMS ARE   * 00320001
*                  IGNORED.  THE FUNCTION TERMINATES WHEN THE END     * 00321001
*                  OF THE LIST IS ENCOUNTERED OR THE CALLERS BUFFER   * 00321101
*                  IS FULL.                                           * 00321201
*                                                                     * 00321301
*        CALLING SEQUENCE:                                            * 00321401
*                DCL STRIP ENTRY( CHAR(*), CHAR(*), CHAR(*) );        * 00321501
*                CALL STRIP( STRING, OPTION, CHAR );                  * 00321709
*                  STRING IS THE CHARACTER STRING TO BE PROCESSED     * 00322101
*                  OPTION IS A STRING WHOSE FIRST CHARACTER IS:       * 00322201
*                    L - REMOVES LEADING CHARACTERS FROM STRING       * 00322301
*                    T - REMOVES TRAILING CHARACTERS FROM STRING      * 00322401
*                    B - REMOVES BOTH LEADING AND TRAILING CHARACTERS * 00322501
*                  CHAR IS THE CHARACTER TO BE REMOVED                * 00322801
*                                                                     * 00323301
*********************************************************************** 00323401
         EJECT                                                          00323501
         REGS  ,                  REGISTER EQUATES                      00323601
         PLIREGS ,                PL/I EQUATES                          00323701
CODE     EQU   PLIBASE            CSECT BASE (R2)                       00323801
         SPACE 3                                                        00324401
LOCAL    DSECT                    LOCAL VARIABLE STORAGE                00324501
STRADDR  DS    A                  A(STRING)                             00324701
STRLEN   DS    F                  CURRENT L'STRING                      00324801
OLDADDR  DS    A                  ORIGINAL A(STRING)                    00324901
OLDLEN   DS    F                  ORIGINAL L'STRING                     00325001
OPTION   DS    C                  OPTION BYTE                           00325101
CHAR     DS    C                  CHARACTER TO STRIP                    00325201
FLAGS    DS    BL1                FLAGS BYTE                            00325301
FLVAR    EQU   X'80'              BUFFER IS VARYING STRING              00325401
LOCALL   EQU   *-LOCAL            LENGTH OF LOCAL STORAGE               00325501
         EJECT                                                          00325601
PLISTRIP CSECT ,                  RESUME CSECT                          00325701
PLISTRIP AMODE 31                                                       00325810
PLISTRIP RMODE ANY                                                      00325910
STRIP    PLIENTRY ,LOCAL=(LOCAL,LOCALL)                                 00326010
         MVI   FLAGS,X'00'        INITIALIZE FLAGS BYTE                 00326110
         MVI   OPTION,C'B'        INITIALIZE OPTION BYTE                00326210
         MVI   CHAR,C' '          INITIALIZE CHARACTER TO STRIP         00326310
         SPACE 1                                                        00326410
*--------------------------------*                                      00326510
* PROCESS 'STRING' ARGUMENT      *                                      00326610
*--------------------------------*                                      00326710
GETARG1  EQU   *                                                        00326810
         PLIARG 1,STRING,ADDR=R14,LEN=R15 LOAD ARG1 ADDRESS AND LENGTH  00326910
         LTR   R15,R15            TEST STRING LENGTH                    00327010
         BNP   RETURN             EXIT IF NOT AT LEAST ONE CHAR         00327110
         LA    R14,0(,R14)        ENSURE VALID ADDRESS                  00327210
         STM   R14,R15,STRADDR    SAVE FOR LATER                        00327310
         STM   R14,R15,OLDADDR      "   "    "                          00327410
         L     R14,0(,R1)         PICK UP 1ST ARG DESC ADDR             00327510
         TM    6(R14),X'80'       Q/ BUFFER IS VARYING STRING?          00327610
         BNO   GETARG2            .. NO                                 00327710
         OI    FLAGS,FLVAR        INDICATE VARYING STRING               00327810
         SPACE 1                                                        00327910
*--------------------------------*                                      00328010
* PROCESS 'OPTION' ARGUMENT      *                                      00328110
*--------------------------------*                                      00328210
GETARG2  EQU   *                                                        00328310
         TM    0(R1),X'80'        Q/ DO WE HAVE SECOND ARG?             00328410
         BO    ARGDONE            .. NO, SKIP FURTHER ARGUMENT PROCESS  00328510
         PLIARG 2,STRING,ADDR=R14,LEN=R15 LOAD ARG2 ADDRESS AND LENGTH  00328610
         LTR   R15,R15            CHECK ARGUMENT LENGTH                 00328710
         BNP   GETARG3            .. OMITTED                            00328810
         MVC   OPTION,0(R14)      SAVE ARGUMENT VALUE                   00328910
         OI    OPTION,X'40'       TRANSLATE TO UPPER-CASE               00329010
         CLI   OPTION,C'B'        CHECK FOR VALID OPTION                00329110
         BE    GETARG3            +                                     00329210
         CLI   OPTION,C'T'        +                                     00329310
         BE    GETARG3            +                                     00330001
         MVI   OPTION,C'L'        ELSE DEFAULT IS 'L'                   00331001
*--------------------------------*                                      00332001
* PROCESS 'CHAR' ARGUMENT        *                                      00333001
*--------------------------------*                                      00334001
GETARG3  EQU   *                                                        00335001
         TM    4(R1),X'80'        Q/ DO WE HAVE THIRD ARG?              00335101
         BO    ARGDONE            .. NO, SKIP FURTHER ARGUMENT PROCESS  00335201
         PLIARG 3,STRING,ADDR=R14,LEN=R15 LOAD ARG3 ADDRESS AND LENGTH  00335301
         LTR   R15,R15            CHECK ARGUMENT LENGTH                 00335401
         BNP   ARGDONE            .. OMITTED                            00335501
         MVC   CHAR,0(R14)        SAVE ARGUMENT VALUE                   00335601
*                                                                       00335701
ARGDONE  EQU   *                  DONE PROCESSING ARGUMENTS             00335801
         SR    R0,R0              JUST FOR GRINS ;-)                    00335905
         IC    R0,CHAR            PICK UP CHARACTER TO STRIP            00336005
*                                                                       00336201
*--------------------------------*                                      00336301
* FIND END OF STRING             *                                      00336401
*--------------------------------*                                      00336501
*                                                                       00336601
FIND_END DS    0H                                                       00336701
         CLI   OPTION,C'L'        Q/ LEADING ONLY?                      00336901
         BE    FIND_START         .. YES, SKIP END PROCESSING           00337001
         LM    R14,R15,STRADDR    PICK UP ADDRESS AND LENGTH            00337101
         LA    R1,0(R15,R14)      POINT TO LAST+1 CHAR                  00337202
ENDLOOP  EQU   *                                                        00337301
         BCTR  R1,0               BACK UP ONE CHARACTER                 00337401
         C     R1,STRADDR         Q/ BACK TO BEGINNING?                 00337505
         BL    FOUND_END          .. YES                                00337601
         CLM   R0,B'0001',0(R1)   Q/ IS THIS CHARACTER TO STRIP?        00337701
         BNE   FOUND_END          .. NO, DONE                           00337801
         B     ENDLOOP            .. YES, CONTINUE                      00337901
FOUND_END EQU  *                                                        00338001
         SR    R1,R14             COMPUTE NEW LENGTH                    00338101
         AH    R1,=H'1'           +                                     00338208
         ST    R1,STRLEN          +                                     00338308
*                                                                       00338401
*--------------------------------*                                      00338501
* FIND BEGINNING OF STRING       *                                      00338601
*--------------------------------*                                      00338701
*                                                                       00338801
FIND_START DS  0H                                                       00338901
         CLI   OPTION,C'T'        Q/ TRAILING ONLY?                     00339101
         BE    RETURN_STRING      .. YES, SKIP START PROCESSING         00339201
         LM    R14,R15,STRADDR    PICK UP ADDRESS AND LENGTH            00339305
         LA    R1,0(R15,R14)      POINT TO LAST+1 CHAR                  00339402
STRTLOOP EQU   *                                                        00339501
         CLM   R0,B'0001',0(R14)  Q/ IS THIS CHAR TO STRIP?             00339601
         BNE   FOUND_START        .. NO, DONE                           00339701
         CR    R14,R1             Q/ AT END OF STRING? (SNO)            00339801
         BE    FOUND_START        .. YES                                00339901
         LA    R14,1(,R14)        POINT TO NEXT CHARACTER               00340001
         B     STRTLOOP           CONTINUE                              00340101
FOUND_START EQU *                                                       00340201
         LR    R0,R14             COMPUTE NEW LENGTH                    00340301
         S     R0,STRADDR         +                                     00340402
         SR    R15,R0             +                                     00340701
         STM   R14,R15,STRADDR    SAVE UPDATED ADDRESS AND LENGTH       00340801
*                                                                       00340901
*--------------------------------*                                      00341001
* RETURN TRUNCATED STRING        *                                      00341101
*--------------------------------*                                      00341201
*                                                                       00341301
RETURN_STRING DS 0H               RETURN STRING TO USER                 00341401
         LM   R14,R15,STRADDR     GET UPDATED ADDR AND LENGTH           00341701
         L    R1,OLDADDR          GET ORIGINAL STARTING ADDRESS         00341801
         CR   R14,R1              Q/ BEGINNING CHARS TRUNCATED?         00341901
         BE   EXIT                .. NO, NOTHING TO MOVE                00342008
RETLOOP  EQU  *                   MOVE STRING                           00342101
         CH   R15,=H'256'         Q/ AT LEAST 256 BYTES LEFT?           00342201
         BL   RETDONE             .. NO, LAST MOVE                      00342301
         MVC  0(256,R1),0(R14)    .. YES, MOVE 256                      00342401
         LA   R1,256(,R1)         UPDATE POINTERS AND LENGTH            00342501
         LA   R14,256(,R14)       +                                     00342601
         SH   R15,=H'256'         +                                     00342701
         B    RETLOOP                                                   00342801
RETDONE  EQU  *                   MOVE LAST 256 CHAR OR LESS            00342901
         LTR  R15,R15             Q/ ANYTHING LEFT TO MOVE?             00343001
         BNP  EXIT                .. NO                                 00343101
         BCTR R15,0               .. YES, MOVE LAST SEGMENT             00343201
         EX   R15,MVCL            *** MVC 0(*-*,R1),0(R14)              00343301
*                                                                       00343501
EXIT     EQU   *                                                        00343601
         TM    FLAGS,FLVAR        Q/ 'STRING' = VARYING?                00343901
         BNO   BLANKIT            .. NO, BLANK REST OF STRING           00344001
         L     R1,OLDADDR         POINT TO START OF STRING              00344101
         SH    R1,=H'2'           BACK UP TO LENGTH FIELD               00344201
         L     R15,STRLEN         UPDATE LENGTH                         00344301
         STH   R15,0(,R1)         +                                     00344401
         B     RETURN                                                   00344501
*                                                                       00344601
*  NOTE -- THIS OPERATION (CALLING STRIP WITH A NON-VARYING STRING      00344701
*          ARGUMENT) MAKES VERY LITTLE SENSE, BUT WE'LL PROVIDE FOR     00344801
*          IT ANYWAY.  IF CHARACTERS ARE STRIPPED OFF THE FRONT OF      00344901
*          THE STRING WE'LL BLANK-FILL THE STRING TO THE END.           00345001
*                                                                       00345101
BLANKIT  EQU   *                                                        00345201
         LM    R14,R15,STRADDR    GET UPDATED ADDRESS AND LENGTH        00345301
         L     R1,OLDLEN          GET ORIGINAL LENGTH                   00345408
         SR    R1,R15             GET NUMBER OF CHARACTERS TO BLANK     00345501
         LA    R0,0(R15,R14)      POINT TO LAST CHAR+1                  00345601
         L     R15,=X'40000000'   LOAD PAD CHARACTER                    00345701
         MVCL  R0,R14             BLANK TO END OF STRING                00345805
*                                                                       00345901
RETURN   EQU   *                  RETURN TO CALLER                      00346001
         PLIRET                                                         00346201
         SPACE 1                                                        00346301
*        EXECUTED INSTRUCTIONS                                          00346401
MVCL     MVC  0(*-*,R1),0(R14)                                          00347001
         SPACE 1                                                        00350001
         LTORG                                                          00410001
         SPACE 1                                                        00420001
         END                                                            00434301
