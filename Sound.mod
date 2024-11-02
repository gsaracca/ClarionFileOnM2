IMPLEMENTATION MODULE Sound;

IMPORT MATHLIB, Lib;
FROM Machine IMPORT IfSound;

PROCEDURE PlayScale( mayor :BOOLEAN; key :SHORTINT );
(*
   Toca una escala en "mayor" o "menor" en clave : "key".
   Usa reales por lo tanto genera arriba de 10Kb de codigo mas de lo normal.
*)

   CONST MiddleC = 131.0;
         Const   = 0.3010299957 / 12.0;

   TYPE  NoteType  = SHORTINT;
         Rango     = [ 1..8 ];
         ScaleType = ARRAY Rango OF NoteType;

   CONST major = ScaleType(0,2,4,5,7,9,11,12);
         minor = ScaleType(0,2,3,5,7,8,11,12);

   VAR  i     :Rango;
        mode  :ScaleType;
        note  :NoteType;
        frec  :LONGREAL;
BEGIN
    IF IfSound() THEN
       IF mayor
          THEN mode := major;
          ELSE mode := minor;
       END;
       FOR i := 1 TO 8 DO
           note := key + mode[ i ];
           frec := MiddleC * MATHLIB.Exp( LONGREAL( note ) * Const );
           Lib.Sound( CARDINAL( frec ) );
           Lib.Delay( 200 );
       END;
       Lib.NoSound();
    END; (* if *)
END PlayScale;

PROCEDURE Alarm();
(*
   Suena la alarma.
*)
    VAR a :CARDINAL;
BEGIN
    IF IfSound() THEN
       FOR a := 0 TO 2 DO
           Lib.Speaker( 600, 50 ); Lib.Delay(  50 );
           Lib.Speaker( 600, 50 ); Lib.Delay( 150 );
        END; (* for *)
    END; (* if *)
END Alarm;

PROCEDURE Ring();
(*
   Hace un pitido.
*)
BEGIN
    IF IfSound() THEN
       Lib.Speaker( 400, 7 );
    END; (* if *)
END Ring;

PROCEDURE Sirena();
(*
   Suena una sirena.
*)
   VAR Note :CARDINAL;
BEGIN
    IF IfSound() THEN
       FOR Note := 440 TO 350 BY -10 DO
           Lib.Speaker( Note, 20 );
       END;
    END; (* if *)
END Sirena;

PROCEDURE Beep( nota, dur :CARDINAL );
(*
   Ejecuta un sonido de "nota" con duraci¢n : "dur".
*)
BEGIN
    IF IfSound() THEN
       Lib.Speaker( nota, dur );
    END; (* if *)
END Beep;

END Sound.
