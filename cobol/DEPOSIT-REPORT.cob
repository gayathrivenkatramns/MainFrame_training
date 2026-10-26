       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK-REPORT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 INPUT-LINE       PIC X(30).
       01 BRANCH-CODE      PIC 9(3).
       01 AMOUNT-TEXT      PIC X(9).
       01 DEPOSIT-AMOUNT   PIC 9(9).
       01 TOTAL-RECORDS    PIC 99 VALUE ZERO.

       01 CURRENT-BRANCH   PIC 9(3).
       01 BRANCH-TOTAL     PIC 9(12) VALUE ZERO.
       01 GRAND-TOTAL      PIC 9(12) VALUE ZERO.

       01 DISPLAY-TOTAL    PIC Z(11)9.
       01 DISPLAY-GRAND    PIC Z(11)9.

       01 RECORD-COUNT     PIC 99 VALUE ZERO.

       PROCEDURE DIVISION.

       MAIN-PROCESS.

           DISPLAY "Enter number of records:"
           ACCEPT TOTAL-RECORDS

           PERFORM UNTIL RECORD-COUNT = TOTAL-RECORDS

               
               ACCEPT INPUT-LINE

               MOVE INPUT-LINE(1:3) TO BRANCH-CODE
               MOVE INPUT-LINE(5:9) TO AMOUNT-TEXT

               COMPUTE DEPOSIT-AMOUNT =
                   FUNCTION NUMVAL(AMOUNT-TEXT)

               IF RECORD-COUNT = 0
                   MOVE BRANCH-CODE TO CURRENT-BRANCH
               END-IF

               IF BRANCH-CODE NOT = CURRENT-BRANCH

                   MOVE BRANCH-TOTAL TO DISPLAY-TOTAL

                   DISPLAY 'Branch '
                       CURRENT-BRANCH ': '
                       FUNCTION TRIM(DISPLAY-TOTAL)

                   MOVE BRANCH-CODE TO CURRENT-BRANCH
                   MOVE ZERO TO BRANCH-TOTAL

               END-IF

               ADD DEPOSIT-AMOUNT TO BRANCH-TOTAL
               ADD DEPOSIT-AMOUNT TO GRAND-TOTAL
               ADD 1 TO RECORD-COUNT

           END-PERFORM

           MOVE BRANCH-TOTAL TO DISPLAY-TOTAL

           DISPLAY 'Branch '
               CURRENT-BRANCH ': '
               FUNCTION TRIM(DISPLAY-TOTAL)

           MOVE GRAND-TOTAL TO DISPLAY-GRAND

           DISPLAY 'Grand total: '
               FUNCTION TRIM(DISPLAY-GRAND)

           STOP RUN.