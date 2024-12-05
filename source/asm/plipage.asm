         TITLE 'PAGE ALLOCATION/FREE ROUTINES FOR PL/I'
PLIPAGE  CSECT ,
***********************************************************************
*                                                                     *
* MODULE ID:   PLIPAGE                                                *
*                                                                     *
* AUTHOR:      PETER FLASS                                            *
*              NEW YORK STATE LEGISLATIVE BILL DRAFTING COMMISSION    *
*              FEBRUARY, 1994                                         *
*                                                                     *
* FUNCTION:    ALLOCATE/FREE STORAGE ON PAGE BOUNDARIES FOR           *
*              DATA WINDOWING SERVICES.                               *
*                                                                     *
* CALLING SEQUENCE:                                                   *
*              AREA_ADDR = PGALLOC( NUM_PAGES );                      *
*                  ALLOCATES 'NUM_PAGES' 4K PAGES ON PAGE BOUNDARY    *
*                  RETURNS ADDRESS OF ALLOCATED AREA                  *
*              CALL PGFREE ( AREA_ADDR, NUM_PAGES );                  *
*                  FREES 'NUM_PAGES' 4K PAGES AT ADDRESS 'AREA_ADDR'  *
*                  STORAGE MUST HAVE BEEN PREVIOUSLY ALLOCATED.       *
*                                                                     *
***********************************************************************
*
PLIPAGE  AMODE 31
PLIPAGE  RMODE ANY
*
         SPACE 3
         REGS
         PLIREGS
         EJECT
***********************************************************************
*        A C Q U I R E   S T O R A G E                                *
***********************************************************************
PGALLOC  PLIENTRY                 <ENTRY TO ACQUIRE PAGE-ALIGNED STG>
         LR    R10,R1              SAVE PARAMETER LIST ADDRESS
         L     R11,0(,R10)         GET A(NUM_PAGES)
         L     R0,0(,R11)          GET NUM_PAGES
         SLL   R0,12               CONVERT TO BYTES
         STORAGE OBTAIN,           ACQUIRE STORAGE                     X
               LENGTH=(0),         NUMBER OF BYTES REQUESTED           X
               BNDRY=PAGE,         PAGE-ALIGNED                        X
               LOC=ANY             ABOVE THE LINE
         L     R11,4(,R10)         GET A(AREA_ADDR)
         ST    R1,0(,R11)          SAVE ACQUIRED AREA ADDRESS
         PLIRET                    EXIT
         SPACE 1
         LTORG *
         DROP  PLIBASE
*
         SPACE 3
***********************************************************************
*        F R E E   S T O R A G E                                      *
***********************************************************************
PGFREE   PLIENTRY                  <ENTRY TO FREE PAGE-ALIGNED STG>
         LR    R10,R1              SAVE PARAMETER LIST ADDRESS
         L     R11,4(,R10)         GET A(NUM_PAGES)
         L     R0,0(,R11)          GET NUM_PAGES
         L     R1,0(,R10)          GET A(AREA_ADDR)
         L     R1,0(,R1)           GET AREA_ADDR
         SLL   R0,12               CONVERT PAGES TO BYTES
         STORAGE RELEASE,          FREE STORAGE                        X
               LENGTH=(0),         NUMBER OF BYTES REQUESTED           X
               ADDR=(1)            ADDRESS PASSED BY CALLER
         PLIRET                    EXIT
         SPACE 1
         LTORG *
         DROP  PLIBASE
*
         END
 MODE AMODE(31)
 MODE RMODE(ANY)
 ALIAS PGALLOC,PGFREE
 NAME PLIPAGE(R)
