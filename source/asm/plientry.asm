         MACRO
&NAME    PLIENTRY &BASE=2,&LOCAL=
.*
.*       PL/I ENTRY LINKAGE FOR ASSEMBLER SUBROUTINES
.*       MODIFIED 3/28/96 FOR PL/I FOR MVS AND VM  PRF
.*                1/08/97 FIX PROBLEM WITH 7-CHAR NAMES PRF
.*
         LCLA  &KNAME,
         LCLC  &LSIZE,&LNAME
&KNAME   SETA  K'&NAME             SIZE OF NAME
         AIF   (&KNAME LE 7).LENOK
&KNAME   SETA  7
.LENOK   ANOP
         ENTRY &NAME
         DS    0H                  ALIGNMENT
         AIF   (&KNAME EQ 7).LENSVN
         DC    CL(7-&KNAME)' '
.LENSVN  ANOP
         DC    CL(&KNAME)'&NAME'   ENTRY POINT NAME
         DC    AL1(&KNAME)         LENGTH OF NAME
***********************************************************************
*              PL/I CALLABLE ENTRY POINT                              *
***********************************************************************
&NAME    DS    0H                  ENTRY POINT
         STM   14,12,12(13)        SAVE REGISTERS IN DSA
         B     *-&NAME+16(,15)     SKIP OVER CONSTANTS
*----------------------------------
         DC    A(0)                A(STATEMENT NO. TABLE)
         AIF   (T'&LOCAL EQ 'O').NOLOCAL
&LSIZE   SETC  '&LOCAL(2)'
&LNAME   SETC  '&LOCAL(1)'
         DC    A(208+&LSIZE.)      SIZE OF DSA
         DC    A(0)                A(STATIC)
*----------------------------------
         LA    0,208+7+&LSIZE      LOAD LENGTH OF DSA REQUIRED
         SRL   0,3                 ROUNDED TO
         SLL   0,3                   DOUBLEWORD BOUNDARY
         AGO   .COM
.NOLOCAL ANOP                      LOCAL VARIABLE BYPASS
*----------------------------------
         LA    0,208               LOAD BASIC DSA LENGTH
.COM     ANOP
         L     1,76(,13)           LOAD A(NEXT_AVAIL_BYTE)
         ALR   0,1                 COMPUTE END OF NEW DSA
         CL    0,12(,12)           Q/ ENOUGH STORAGE AVAILABLE?
         BNH   *-&NAME+10(,15)     .. YES
         L     15,116(,12)         LOAD A(OVERFLOW ROUTINE)
         BALR  14,15               OBTAIN MORE STORAGE
         L     14,72(,13)          .
         LR    15,0
         STM   14,0,72(1)
         ST    13,4(,1)            STORE DSA BACK CHAIN
         ST    1,8(,13)            STORE DSA FOREWARD CHAIN
         MVI   0(1),X'80'          SET FLAGS
         MVI   1(1),X'25'
         MVI   118(1),X'02'        SYMBOLIC DUMP FLAG
         LA    13,0(,1)            LOAD A(NEW_DSA)
         L     1,4(,13)            RESTORE PARAMETER LIST ADDRESS
         L     1,24(,1)            .
         BALR  &BASE,0             LOAD NEW BASE REGISTER
         USING *,&BASE
         MVC   84(4,13),=X'91E091E0'
         AIF   (T'&LOCAL EQ 'O').EXIT
         USING &LNAME-208,13       ADDRESSIBILITY FOR LOCAL VARIABLES
* . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . *
         SPACE 3
.EXIT    MEXIT
         MEND
