         TITLE 'PLIABEND - FORCE ABEND'                                 00170000
PLIABEND CSECT                                                          00180000
*********************************************************************** 00190000
*                                                                     * 00200000
*        PLIABEND: FORCE ABEND                                        * 00210000
*                                                                     * 00230000
*        AUTHOR: PETER FLASS - NYS LEGISLATIVE BILL DRAFTING COMM     * 00240000
*                MAY, 1997                                            * 00250000
*                                                                     * 00260000
*        FUNCTION:                                                    * 00270000
*                                                                     * 00360000
*        CALLING SEQUENCE:                                            * 00370000
*                CALL ABEND;                                          * 00380000
*                                                                     * 00495000
*********************************************************************** 00496000
         EJECT                                                          00497000
         REGS  ,                  REGISTER EQUATES                      00498000
         PLIREGS ,                PL/I EQUATES                          00499000
CODE     EQU   PLIBASE            CSECT BASE (R2)                       00500000
         EJECT                                                          00630000
PLIABEND RMODE ANY                                                      00640000
PLIABEND AMODE 31                                                       00650000
PLIABEND CSECT ,                  RESUME CSECT                          00660000
PLIABEND VERSION 1.0                                                    00661001
ABEND    PLIENTRY                                                       00670000
         ABEND 999,DUMP                                                 00680000
                                                                        02230000
         END                                                            02240000
