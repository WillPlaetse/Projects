      ******************************************************************
      * Author: Will Verplaetse
      * Date: 4/28/2024
      * Purpose: This program reads an input file with control breaks
      *          writes a report on which employees are performing well
      *          and which ones are performing poorly.
      *
      *          If I had access to a compiler with the report writer,
      *          I would use that. However, I don't so alignment is done
      *          manually.
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. EmployeeClassifier.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT inputFile ASSIGN TO
           "input.txt"
           ORGANIZATION IS LINE SEQUENTIAL.


           SELECT badEmps ASSIGN TO "Poor Employee Report"
           ORGANIZATION IS LINE SEQUENTIAL.


           SELECT goodEmps ASSIGN TO "Good Employee Report"
           ORGANIZATION IS LINE SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.

       FD inputFile.
       01   stateRecord.
         88 endOfFile              VALUE HIGH-VALUE.

      *    Setting up type codes to trigger control breaks
         02 typeCode               PIC X.
           88  isStateRecord       VALUE "S".
           88  isBranchRecord      VALUE "B".
           88  isEmployeeRecord    VALUE "E".

         02 stateName              PIC XX.


       01   branchRecord.
         02 typeCode               PIC X.
         02 branchName             PIC X(15).


       01   employeeRecord.
         02 typeCode               PIC X.
         02 employeeData.
         03 employeeName           PIC X(15).
         03 totalSalesAmount       PIC 9(6)V99.
           88 phenomenalSales      VALUE 100000 THRU 1000000.
           88 goodSales            VALUE 50000 THRU 99999.99.
           88 poorSales            VALUE 0 THRU 4999.99.
         03 FILLER                 PIC X.
         03 numOfNewCustomers      PIC 99.
           88 lotsOfCustomers   VALUE 25 THRU 99.
           88 noCustomers       VALUE 0.
         03 FILLER                 PIC X.
         03 attendanceRatio        PIC 9V99.
           88 perfectAttendance     VALUE 1.00.
           88 poorAttendance       VALUE 0 THRU 0.50.






       FD badEmps.

       01  badStateHeader             PIC XX.

       01   badStateSummary.
           02 badStateCaption  PIC X(28).
           02 badStateTotal   PIC ZZ9.

       01  BadBranchHeader           PIC X(15).

       01  badColumnHeaders.
           02 nameCol          PIC X(15).
           02 salesCol         PIC X(13).
           02 newCustomersCol  PIC X(19).
           02 attendanceCol    PIC X(15).


       01  badBranchSummary.
         02 badBranchCaption             PIC X(21).
         02 badBranchTotal     PIC ZZ9.

      *    Using edited pictures to align file output
      *    Using the report writer would be better suited for this task,
      *    however, I don't have a compiler that supports it
       01  badEmpRecord.
           02 badEmpName           PIC X(15).
           02 badSaleAmount        PIC $$$$$,$$$.99.
           02 FILLER               PIC X VALUE " ".
           02 badNumOfCustomers    PIC Z(17)9.
           02 FILLER               PIC X VALUE " ".
           02 badAttendance        PIC Z(11)9.99.




       FD goodEmps.
       01   goodStateHeader             PIC XX.

       01   goodStateSummary.
           02 goodStateCaption  PIC X(28).
           02 goodStateTotal   PIC ZZ9.

       01  goodBranchHeader            PIC X(15).

       01  goodColumnHeaders.
           02 nameCol          PIC X(15).
           02 salesCol         PIC X(13).
           02 newCustomersCol  PIC X(19).
           02 attendanceCol    PIC X(15).

       01  goodBranchSummary.
         02 goodBranchCaption             PIC X(22).
         02 goodBranchTotal               PIC ZZ9.



      *Added extra to zero suppressing characters to align columns
       01  goodEmpRecord.
           02 goodEmpName           PIC X(15).
           02 goodSaleAmount        PIC $$$$$,$$$.99.
           02 FILLER                PIC X VALUE " ".
           02 goodNumOfCustomers    PIC Z(17)9.
           02 FILLER                PIC X VALUE " ".
           02 goodAttendance        PIC Z(11)9.99.

       WORKING-STORAGE SECTION.


       01  columnHeaders.
           02 nameCol          PIC X(15) VALUE "Name       ".
           02 salesCol         PIC X(12) VALUE "Sales amount".
           02 newCustomersCol  PIC X(20) VALUE " # of New Customers ".
           02 attendanceCol    PIC X(15) VALUE "Attendance rate".



       01 goodBranchEmpCount             PIC 999 VALUE ZEROES.
       01 badBranchEmpCount              PIC 999 VALUE ZEROES.

       01 goodStateEmpCount             PIC 999 VALUE ZEROES.
       01 badStateEmpCount              PIC 999 VALUE ZEROES.


       01 goodBranchCaptionNf PIC X(22) VALUE "Total good employees: ".
       01 badBranchCaptionNf PIC X(21) VALUE "Total bad employees: ".
       01 badStateCaptionNf PIC X(28) VALUE"Total bad state employees:".
       01 goodStCaptionNf PIC X(28) VALUE "Total good state employees:".



       PROCEDURE DIVISION.
       MAIN-PROCEDURE.


            OPEN INPUT inputFile
            OPEN OUTPUT badEmps
            OPEN OUTPUT goodEmps

            READ inputFile
               AT END SET endOfFile TO TRUE
            END-READ


            PERFORM PROCESS-STATE-RECORD UNTIL endOfFile

           CLOSE inputFile
           CLOSE badEmps
           CLOSE goodEmps

           STOP RUN.


       PROCESS-STATE-RECORD.

           MOVE stateName TO badStateHeader
           MOVE stateName TO goodStateHeader

           WRITE goodStateHeader BEFORE ADVANCING 2 LINES
           WRITE badStateHeader BEFORE ADVANCING 2 LINES

           READ inputFile
               AT END SET endOfFile TO TRUE
           END-READ

           PERFORM PROCESS-BRANCH-RECORD UNTIL endOfFile
           OR isStateRecord.


       PROCESS-BRANCH-RECORD.


           MOVE branchName TO badBranchHeader, goodBranchHeader

           WRITE badBranchHeader BEFORE ADVANCING 2 LINES
           WRITE goodBranchHeader BEFORE ADVANCING 2 LINES

           READ inputFile
               AT END SET endOfFile TO TRUE
           END-READ

           MOVE columnHeaders TO goodColumnHeaders, badColumnHeaders


           WRITE goodColumnHeaders
           WRITE badColumnHeaders



           PERFORM PROCESS-EMPLOYEE UNTIL endOfFile
           OR NOT isEmployeeRecord.


       PROCESS-EMPLOYEE.

           EVALUATE TRUE ALSO TRUE ALSO TRUE
           WHEN phenomenalSales ALSO ANY ALSO ANY
               MOVE employeeName TO goodEmpName
               MOVE totalSalesAmount TO goodSaleAmount
               MOVE numOfNewCustomers TO goodNumOfCustomers
               MOVE attendanceRatio TO goodAttendance
               ADD 1 TO goodBranchEmpCount
               WRITE goodEmpRecord


           WHEN goodSales ALSO lotsOfCustomers ALSO NOT poorAttendance
               MOVE employeeName TO goodEmpName
               MOVE totalSalesAmount TO goodSaleAmount
               MOVE numOfNewCustomers TO goodNumOfCustomers
               MOVE attendanceRatio TO goodAttendance
               ADD 1 TO goodBranchEmpCount
               WRITE goodEmpRecord


           WHEN goodSales ALSO NOT noCustomers ALSO perfectAttendance
               MOVE employeeName TO goodEmpName
               MOVE totalSalesAmount TO goodSaleAmount
               MOVE numOfNewCustomers TO goodNumOfCustomers
               MOVE attendanceRatio TO goodAttendance
               ADD 1 TO goodBranchEmpCount
               WRITE goodEmpRecord

           WHEN ANY ALSO noCustomers ALSO ANY
               MOVE employeeName TO badEmpName
               MOVE totalSalesAmount TO badSaleAmount
               MOVE numOfNewCustomers TO badNumOfCustomers
               MOVE attendanceRatio TO badAttendance
               ADD 1 TO badBranchEmpCount
               WRITE badEmpRecord

           WHEN poorSales ALSO ANY ALSO poorAttendance
               MOVE employeeName TO badEmpName
               MOVE totalSalesAmount TO badSaleAmount
               MOVE numOfNewCustomers TO badNumOfCustomers
               MOVE attendanceRatio TO badAttendance
               ADD 1 TO badBranchEmpCount
               WRITE badEmpRecord


           END-EVALUATE



           READ inputFile
               AT END SET endOfFile TO TRUE
           END-READ

           IF isBranchRecord THEN
               MOVE goodBranchCaptionNf TO goodBranchCaption
               MOVE goodBranchEmpCount TO goodBranchTotal
               ADD goodBranchEmpCount TO goodStateEmpCount

               MOVE badBranchCaptionNf TO badBranchCaption
               MOVE badBranchEmpCount TO badBranchTotal
               ADD badBranchEmpCount TO badStateEmpCount

               MOVE ZEROES TO goodBranchEmpCount
               MOVE ZEROES TO badBranchEmpCount


               WRITE goodBranchSummary BEFORE ADVANCING 2 LINES
               WRITE badBranchSummary BEFORE ADVANCING 2 LINES
           END-IF

           IF isStateRecord OR endOfFile THEN
               MOVE goodBranchCaptionNf TO goodBranchCaption
               MOVE goodBranchEmpCount TO goodBranchTotal
               ADD goodBranchEmpCount TO goodStateEmpCount

               MOVE badBranchCaptionNf TO badBranchCaption
               MOVE badBranchEmpCount TO badBranchTotal
               ADD badBranchEmpCount TO badStateEmpCount

               MOVE ZEROES TO goodBranchEmpCount
               MOVE ZEROES TO badBranchEmpCount

               WRITE goodBranchSummary BEFORE ADVANCING 2 LINES
               WRITE badBranchSummary BEFORE ADVANCING 2 LINES

               MOVE goodStCaptionNf TO goodStateCaption
               MOVE goodStateEmpCount TO goodStateTotal

               MOVE badStateCaptionNf TO badStateCaption
               MOVE badStateEmpCount TO badStateTotal

               MOVE ZEROES TO goodStateEmpCount
               MOVE ZEROES TO badStateEmpCount

               WRITE goodStateSummary BEFORE ADVANCING 2 LINES
               WRITE badSTateSummary BEFORE ADVANCING 2 LINES


               END-IF.
