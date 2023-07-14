      ******************************************************************
      * Author:    Ayelén Rivero
      * Date:      14/07/2023
      * Purpose:   Practice
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERSONAL-INFO.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REPORTE      ASSIGN TO
       "C:\Users\ayele\OneDrive\Escritorio\bin\Personal-report.TXT"
                       ORGANIZATION IS SEQUENTIAL
                       ACCESS MODE IS SEQUENTIAL
                       FILE STATUS IS FS-REPORTE.

       DATA DIVISION.
      *--------------
       FILE SECTION.
      *-------------
       FD  REPORTE
           RECORD CONTAINS 91 CHARACTERS
           BLOCK CONTAINS 0 RECORDS.
       01  REG-REPORTE            PIC X(91).


       01  REPORTE-RECORD.
           05  NOMBRE           PIC X(15).
           05  APELLIDO         PIC X(15).
           05  EDAD             PIC 99.
           05  FECHA-NACIMIENTO.
               07 FECHA-DD      PIC 9(02).
               07 FECHA-MM      PIC 9(02).
               07 FECHA-AAAA    PIC 9(04).
           05  DIRECCION        PIC X(30).
           05  TELEFONO         PIC X(12).


       WORKING-STORAGE SECTION.
      *------------------------

       77  FS-REPORTE               PIC  X(02)   VALUE ' '.
           88 88-FS-REPORTE-YES                  VALUE '00'.
           88 88-FS-REPORTE-EOF                  VALUE '10'.

       77  WS-OPEN-REPORTE          PIC  X(02)   VALUE 'YO'.
           88 88-OPEN-REPORTE-YES                VALUE 'YS'.
           88 88-OPEN-REPORTE-NO                 VALUE 'NO'.


       01  WS-AREAS-A-USAR.
           05 WS-REG-DATOS.
               10  WS-NOMBRE         PIC X(15).
               10  WS-APELLIDO       PIC X(15).
               10  WS-EDAD           PIC 9(02).
               10  WS-FECHA-NACIMIENTO.
                   15 FECHA-DD       PIC 9(02).
                   15 FILLER         PIC X(01)   VALUE '/'.
                   15 FECHA-MM       PIC 9(02).
                   15 FILLER         PIC X(01)   VALUE '/'.
                   15 FECHA-AAAA     PIC 9(04).
               10  WS-DIRECCION      PIC X(15).
               10  WS-TELEFONO       PIC X(10).


           05 SW-FIN                 PIC X(03)    VALUE ' '.

           05  WS-RESPUESTA          PIC X(01).
           05  WS-IMPRESOS           PIC 9(05)    VALUE 0.

       01  WS-CURRENT-DATE.
           03 WS-ACTUAL-DATE.
              05 WS-DATE-AAAA        PIC 9(04).
              05 WS-DATE-MM          PIC 9(02).
              05 WS-DATE-DD          PIC 9(02).

      *----------------------------------------------------------------*
      *TITULOS.
      *----------------------------------------------------------------*
       01  WS-TITULO-1.
           03 FILLER                 PIC X(36)    VALUE ' '.
           03 WS-TIT-1               PIC X(21)
                                     VALUE "REGISTRO DE EMPLEADOS".
           03 FILLER                 PIC X(34)    VALUE ' '.

       01  WS-TITULO-2.
           03 FILLER                 PIC X(08)    VALUE " FECHA: ".
           03 WS-TIT2-FECHA.
               05 TIT-2-DD           PIC 9(02).
               05 FILLER             PIC X(01)    VALUE "/".
               05 TIT-2-MM           PIC 9(02).
               05 FILLER             PIC X(01)    VALUE "/".
               05 TIT-2-AAAA         PIC 9(04).

           03 WS-TIT-2.
               05 FILLER             PIC X(58)    VALUE ' '.
               05 FILLER             PIC X(08)    VALUE "PAGINA: ".
               05 TIT-2-PAGINA       PIC ZZ9.
               05 FILLER             PIC X(04)    VALUE ' '.

       01  WS-GUIONES.
           05 FILLER              PIC X(01).
           05 FILLER              PIC X(89)    VALUE ALL "-".
           05 FILLER              PIC X(01)    VALUE ' '.

       01  WS-SUB-TITULO-1.

           05 FILLER              PIC X(02)    VALUE ' '.
           05 FILLER              PIC X(06)    VALUE "NOMBRE".
           05 FILLER              PIC X(10)    VALUE ' '.
           05 FILLER              PIC X(08)    VALUE "APELLIDO".
           05 FILLER              PIC X(08)    VALUE ' '.
           05 FILLER              PIC X(04)    VALUE 'EDAD'.
           05 FILLER              PIC X(03)    VALUE ' '.
           05 FILLER              PIC X(12)    VALUE 'FECHA DE NAC'.
           05 FILLER              PIC X(04)    VALUE ' '.
           05 FILLER              PIC X(09)    VALUE 'DIRECCION'.
           05 FILLER              PIC X(09)    VALUE ' '.
           05 FILLER              PIC X(08)    VALUE 'TELEFONO'.
           05 FILLER              PIC X(08)    VALUE ' '.

       01  WS-DETALLE.
           05 FILLER              PIC X(01)    VALUE ' '.
           05 WS-DET-NOMBRE       PIC X(15).
           05 FILLER              PIC X(02)    VALUE ' '.
           05 WS-DET-APELLIDO     PIC X(15).
           05 FILLER              PIC X(01)    VALUE ' '.
           05 WS-DET-EDAD         PIC 9(02).
           05 FILLER              PIC X(06)    VALUE ' '.
           05 WS-DET-FECH-NAC.
               10 NAC-DD          PIC 9(02).
               10 FILLER          PIC X VALUE '/'.
               10 NAC-MM          PIC 9(02).
               10 FILLER          PIC X VALUE '/'.
               10 NAC-AAAA        PIC 9(04).
           05 FILLER              PIC X(05)    VALUE ' '.
           05 WS-DET-DIRECCION    PIC X(15).
           05 FILLER              PIC X(02)    VALUE ' '.
           05 WS-DET-TELEFONO     PIC 9(10).
           05 FILLER              PIC X(07)    VALUE ' '.
      *----------------------------------------------------------------*
      *DEFINICION DE FECHA ACTUAL.
      *----------------------------------------------------------------*

       01  CURRENT-DATE.
           05 DATE-DD             PIC 9(02).
           05 FILLER              PIC X     VALUE '/'.
           05 ATE-MM              PIC 9(02).
           05 FILLER              PIC X     VALUE '/'.
           05 DATE-AAA            PIC 9(04).



       PROCEDURE DIVISION.
      *-------------------
       010-INICIO.

           PERFORM 050-ABRIR-ARCHIVO.
           PERFORM 075-INGRESAR-EMPLEADO.
           PERFORM 200-IMPRIME.
           PERFORM 300-WRITE.
           PERFORM 100-PREGUNTAR-OTRO-EMPLEADO UNTIL WS-RESPUESTA = 'N'
                   OR 'n'.

           PERFORM 400-CERRAR-ARCHIVO.

           STOP RUN.

       050-ABRIR-ARCHIVO.
           OPEN OUTPUT REPORTE.

       075-INGRESAR-EMPLEADO.
           DISPLAY "Por favor, ingrese su nombre: ".
           ACCEPT WS-NOMBRE.
           DISPLAY "Ingrese su apellido: ".
           ACCEPT WS-APELLIDO.
           DISPLAY "Ingrese su edad: ".
           ACCEPT WS-EDAD.
           DISPLAY "Ingrese su fecha de nacimiento: ".
           ACCEPT WS-FECHA-NACIMIENTO.
           DISPLAY "Ingrese su direccion: ".
           ACCEPT WS-DIRECCION.
           DISPLAY "Ingrese su numero de telefono: ".
           ACCEPT WS-TELEFONO.


       200-IMPRIME.

           WRITE REG-REPORTE FROM WS-TITULO-1 AFTER ADVANCING PAGE
           MOVE FUNCTION CURRENT-DATE    TO WS-CURRENT-DATE

           MOVE WS-DATE-DD                    TO TIT-2-DD
           MOVE WS-DATE-MM                    TO TIT-2-MM
           MOVE WS-DATE-AAAA                  TO TIT-2-AAAA
           MOVE 1                             TO TIT-2-PAGINA
           WRITE REG-REPORTE FROM WS-TITULO-2 AFTER ADVANCING 1
           WRITE REG-REPORTE FROM WS-GUIONES  AFTER ADVANCING 1
           WRITE REG-REPORTE FROM WS-SUB-TITULO-1 AFTER ADVANCING 1
           WRITE REG-REPORTE FROM WS-GUIONES AFTER ADVANCING 1

           MOVE SPACES TO SW-FIN.

       300-WRITE.

           MOVE WS-NOMBRE TO WS-DET-NOMBRE
           MOVE WS-APELLIDO TO WS-DET-APELLIDO
           MOVE WS-EDAD TO WS-DET-EDAD
           MOVE WS-FECHA-NACIMIENTO TO WS-DET-FECH-NAC
           MOVE WS-DIRECCION TO WS-DET-DIRECCION
           MOVE WS-TELEFONO TO WS-DET-TELEFONO
           WRITE REG-REPORTE FROM WS-DETALLE AFTER ADVANCING 1

           ADD 1 TO WS-IMPRESOS.

       100-PREGUNTAR-OTRO-EMPLEADO.
           DISPLAY "¿Quiere ingresar otro empleado? (S/N): ".
           ACCEPT WS-RESPUESTA.

           IF WS-RESPUESTA = 'S' OR WS-RESPUESTA = 's' THEN

               PERFORM 050-ABRIR-ARCHIVO
               PERFORM 075-INGRESAR-EMPLEADO
               PERFORM 300-WRITE
               PERFORM 100-PREGUNTAR-OTRO-EMPLEADO

      *         WRITE REG-REPORTE

           ELSE
               PERFORM 400-CERRAR-ARCHIVO

           END-IF.

       400-CERRAR-ARCHIVO.

           CLOSE REPORTE

           STOP RUN.
