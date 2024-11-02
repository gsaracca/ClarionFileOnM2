IMPLEMENTATION MODULE String;

IMPORT Lib;

CONST msgError = 'Internal string error = ';

VAR OldChar  :CHAR;
    UsedChar :BOOLEAN;

PROCEDURE SaveChar();
(*
   Salva el "FillChar".
*)
BEGIN
    IF UsedChar THEN Lib.FatalError( msgError + '"FillChar" yet used.' ); END;
    OldChar  := FillChar;
    UsedChar := TRUE;
END SaveChar;

PROCEDURE RestoreChar();
(*
   Reestablece el "FillChar".
*)
BEGIN
    FillChar := OldChar;
    UsedChar := FALSE;
END RestoreChar;

VAR OldErrChar  :CHAR;
    OldErrWrite :BOOLEAN;
    OldErrFill  :BOOLEAN;
    UsedErr     :BOOLEAN;

PROCEDURE SaveErr();
(*
   Salva el "ErrChar" y la "ErrWrite".
*)
BEGIN
    IF UsedErr THEN Lib.FatalError( msgError + '"ErrorChar" yet used.' ); END;
    OldErrChar  := ErrChar;
    OldErrWrite := ErrWrite;
    OldErrFill  := ErrFill;
    UsedErr     := TRUE;
END SaveErr;

PROCEDURE RestoreErr();
(*
   Reestablece el "ErrChar" y la "ErrWrite".
*)
BEGIN
    ErrChar  := OldErrChar;
    ErrWrite := OldErrWrite;
    ErrFill  := OldErrFill;
    UsedErr  := FALSE;
END RestoreErr;

PROCEDURE CharRep( VAR str :ARRAY OF CHAR; ch :CHAR; count: CARDINAL );
(*
   Genera el string "str" con en el caracter : "ch" tantas veces como "count".
*)
    VAR aux :ARRAY [ 0..255 ] OF CHAR;
        i, j :CARDINAL;
BEGIN
    Str.Copy( str, '' );
    WHILE count > 0 DO
        i := SIZE( aux ) - 2;
        IF i > count THEN i := count END;
        DEC( count, i );
        j := 0;
        WHILE ( j < i ) DO aux[ j ] := ch; INC( j ) END;
        aux[ j ] := CHR(0);
        Str.Copy( str, aux );
    END;
END CharRep;

PROCEDURE Card2Str( VAR s :ARRAY OF CHAR; n :CARDINAL; l :INTEGER );
(*
   Convierte un cardinal a un string ajustado en : "l".
*)
    VAR ok     :BOOLEAN;
        s1, s2 :ARRAY [ 0..255 ] OF CHAR;
BEGIN
    Str.CardToStr( VAL( LONGCARD, n ), s1, 10, ok );
    IF ok THEN StrAdj( s2, s1, l ); Str.Copy( s, s2 ); END;
END Card2Str;

PROCEDURE LongCard2Str( VAR s :ARRAY OF CHAR; n :LONGCARD; l :INTEGER );
(*
   Convierte un long cardinal a un string ajustado en : "l".
*)
    VAR ok     :BOOLEAN;
        s1, s2 :ARRAY [ 0..255 ] OF CHAR;
BEGIN
    Str.CardToStr( n, s1, 10, ok );
    IF ok THEN StrAdj( s2, s1, l ); Str.Copy( s, s2 ); END;
END LongCard2Str;

PROCEDURE LongInt2Str( VAR s :ARRAY OF CHAR; n :LONGINT; l :INTEGER );
    VAR ok     :BOOLEAN;
        s1, s2 :ARRAY [ 0..255 ] OF CHAR;
BEGIN
    Str.IntToStr( n, s1, 10, ok );
    IF ok THEN StrAdj( s2, s1, l ); Str.Copy( s, s2 ); END;
END LongInt2Str;

PROCEDURE StrAdj( VAR str1 :ARRAY OF CHAR; str :ARRAY OF CHAR; len :INTEGER );
(*
   Genera el string "str1" a partir del string : "str" con longitud "len".
*)
    VAR aux :ARRAY [ 0..255 ] OF CHAR;
        l   :CARDINAL;
        a   :INTEGER;
BEGIN
    Str.Copy( str1, str );
    l := Str.Length( str1 );
    a := ABS( len ) - INTEGER( l );
    IF (a < 0) & ErrWrite THEN
       l := CARDINAL( ABS( len ) );
       IF l <= HIGH( str1 ) THEN str1[ l ] := CHR(0); END;
       IF ErrFill THEN
          WHILE ( l > 0 ) DO DEC( l ); str1[ l ] := ErrChar; END;
       END; (* if *)
       a := 0;
    END; (* if *)
    aux := '';
    IF ( len > 0 ) & ( a > 0 ) THEN CharRep( aux, FillChar, a ); END;
    Str.Concat( str1, aux, str1 );
    aux := '';
    IF ( len < 0 ) & ( a > 0 ) THEN CharRep( aux, FillChar, a ); END;
    Str.Append( str1, aux );
END StrAdj;

PROCEDURE ClearLastSpaces( VAR s :ARRAY OF CHAR );
(*
   Limpia los £ltimos espacios [ CHR(32) ] del string "s".
*)
BEGIN
    ClearLastChar( s, ' ' );
END ClearLastSpaces;

PROCEDURE ClearLastChar( VAR s :ARRAY OF CHAR; ch :CHAR );
(*
   Limpia los £ltimos caracteres [ ch ] del string "s".
   Idem al anterior pero para cualquier "ch".
*)
    VAR i :INTEGER;
BEGIN
    i := INTEGER( Str.Length( s ) ) - 1;
    WHILE (i >= 0) & (s[ i ] = ch) DO
        s[ i ] := CHR(0);
        DEC( i );
    END; (* while *)
END ClearLastChar;

PROCEDURE ClearFirstSpaces( VAR s :ARRAY OF CHAR );
(*
   Limpia los primeros espacios [ CHR(32) ] del string "s".
*)
BEGIN
    ClearFirstChar( s, CHR(32) );
END ClearFirstSpaces;

PROCEDURE ClearFirstChar( VAR s :ARRAY OF CHAR; ch :CHAR );
(*
   idem anterior pero comenzando de la izquierda.
*)
    VAR i, j :CARDINAL;
BEGIN
    i := 0;
    WHILE (s[ i ] = ch) & (s[ i ] # 0C) & (i <= HIGH( s )) DO
        INC( i );
    END; (* while *)
    IF (i = HIGH( s )) THEN
       s[ 0 ] := 0C;
     ELSIF (i # 0) THEN
       FOR j := i TO HIGH( s ) - 1 DO
           s[ i ] := s[ i + 1 ];
       END; (* for *)
       s[ HIGH( s ) ] := 0C;
    END; (* if *)
END ClearFirstChar;

PROCEDURE BinaryToString( VAR a :ARRAY OF CHAR; l :CARDINAL );
(*
   Convierte un array of byte en un array of char de m xima longitud "l".
*)
BEGIN
    Str.Delete( a, l, 255 );
END BinaryToString;

PROCEDURE ReplaceChar( VAR s :ARRAY OF CHAR; bc :CHAR; nc :CHAR );
(*
   Busca en el string "s" el caracter "bc" y en la primera ocurrencia lo
   reemplaza por "nc".
*)
    VAR len :CARDINAL;
        i   :CARDINAL;
BEGIN
    len := Str.Length( s );
    IF len > 0 THEN
       FOR i := 0 TO len-1 DO
           IF s[ i ] = bc THEN
              s[ i ] := nc;
              RETURN;
           END; (* if *)
       END; (* for *)
    END; (* if *)
END ReplaceChar;

PROCEDURE RReplaceChar( VAR s :ARRAY OF CHAR; bc :CHAR; nc :CHAR );
(*
   Idem a ReplaceChar pero comienza la b£squeda por la derecha.
*)
    VAR len :CARDINAL;
        i   :CARDINAL;
BEGIN
    len := Str.Length( s );
    IF len > 0 THEN
       FOR i := len-1 TO 0 BY -1 DO
           IF s[ i ] = bc THEN
              s[ i ] := nc;
              RETURN;
           END; (* if *)
       END; (* for *)
    END; (* if *)
END RReplaceChar;

PROCEDURE ReplaceAllChar( VAR s :ARRAY OF CHAR; bc :CHAR; nc :CHAR );
(*
   Busca en el string "s" el caracter "bc" y en todas las ocurrencias lo
   reemplaza por el caracter : "nc".
*)
    VAR len :CARDINAL;
        i   :CARDINAL;
BEGIN
    len := Str.Length( s );
    IF len > 0 THEN
       FOR i := 0 TO len-1 DO
           IF s[ i ] = bc THEN
              s[ i ] := nc;
           END; (* if *)
       END; (* for *)
    END; (* if *)
END ReplaceAllChar;

PROCEDURE DeleteChar( VAR s :ARRAY OF CHAR; ch :CHAR );
(*
   Elimina el caracter "ch" en todas sus ocurrencias en el string: "s".
*)
    VAR pos :CARDINAL;
BEGIN
    REPEAT
         pos := Str.CharPos( s, ch );
         IF pos # MAX( CARDINAL ) THEN
            Subst( s, ch, '' );
         END; (* if *)
    UNTIL pos = MAX( CARDINAL );
END DeleteChar;

PROCEDURE FillStr( VAR s :ARRAY OF CHAR; ch :CHAR );
(*
   Llena un string "s" con el caracter : "ch".
   desde 0 hasta HIGH( s ).
*)
BEGIN
    Lib.Fill( ADR( s ), HIGH( s ) + 1, ch );
END FillStr;

PROCEDURE Espanol( n :LONGCARD; VAR frase :ARRAY OF CHAR );
(*
   Dado un n£mero "n" como valor el procedimiento devuelve una "frase" con
   ese valor en espa¤ol.
   Ej : para "n" = 12  ===> "frase" = "doce".
        soporta desde 0 hasta 999,999,999,999.
*)
    TYPE fullStr = ARRAY [ 0..255 ] OF CHAR;

    VAR flagMil, flagMillon :BOOLEAN;

    PROCEDURE GetDigito( n :LONGCARD ) :fullStr;
        TYPE dStr  = ARRAY [ 0..5 ] OF CHAR;
             dType = ARRAY [ 2..9 ] OF dStr;
        CONST d = dType( 'dos',  'tres',  'cuatro', 'cinco',
                         'seis', 'siete', 'ocho',   'nueve' );
        VAR aux :fullStr;
    BEGIN
        IF n = 1 THEN
           IF flagMil OR flagMillon
              THEN RETURN 'un';
              ELSE RETURN 'uno';
           END;
         ELSIF n # 0 THEN
           Str.Copy( aux, d[ CARDINAL( n ) ] );
           RETURN aux;
         ELSE
           RETURN '';
        END;
    END GetDigito;

    PROCEDURE GetDecena( n :LONGCARD ) :fullStr;
        TYPE dStr  = ARRAY [ 0..9 ] OF CHAR;
             dType = ARRAY [ 1..9 ] OF dStr;
        CONST d = dType( 'once',       'doce',      'trece',
                         'catorce',    'quince',    'dieciseis',
                         'diecisiete', 'dieciocho', 'diecinueve' );

        TYPE cStr  = ARRAY [ 0..8 ] OF CHAR;
             cType = ARRAY [ 1..9 ] OF cStr;
        CONST c = cType( 'diez',      'veinte',  'treinta', 'cuarenta',
                         'cincuenta', 'sesenta', 'setenta', 'ochenta',
                         'noventa' );
        VAR aux   :fullStr;
            div   :CARDINAL;
            resto :LONGCARD;
    BEGIN
        IF (n < 10) THEN
           RETURN GetDigito( n );
         ELSE
           div   := CARDINAL( n DIV 10 );
           resto := n MOD 10;
           CASE CARDINAL(n) OF
         11..19 : Str.Copy( aux, d[ CARDINAL( resto ) ] ); |
         21..29 : Str.Concat( aux, 'veinti', GetDigito( resto ) );
             ELSE
                IF resto = 0 THEN
                   Str.Copy( aux, c[ div ] );
                 ELSE
                   Str.Concat( aux, c[ div ], ' y ' );
                   Str.Append( aux, GetDigito( resto ) );
                END;
           END;
           RETURN aux;
        END;
    END GetDecena;

    PROCEDURE GetCentena( n :LONGCARD ) :fullStr;
        TYPE cStr = ARRAY [ 0..12 ] OF CHAR;
             cType = ARRAY [ 1..9 ] OF cStr;

        CONST c = cType( 'ciento',        'doscientos',  'trescientos',
                         'cuatrocientos', 'quinientos',  'seiscientos',
                         'setecientos',   'ochocientos', 'novecientos' );
        VAR aux   :fullStr;
            div   :CARDINAL;
            resto :LONGCARD;
    BEGIN
        IF n < 100 THEN
           RETURN GetDecena( n );
         ELSE
            IF n = 100 THEN
               RETURN 'cien';
             ELSE
               div   := CARDINAL( n DIV 100 );
               resto := n MOD 100;
               IF resto = 0 THEN
                  Str.Copy( aux, c[ div ] );
                ELSE
                  Str.Concat( aux, c[ div ], ' ' );
                  Str.Append( aux, GetDecena( resto ) );
               END;
               RETURN aux;
            END;
         END;
    END GetCentena;

    PROCEDURE GetMiles( n :LONGCARD ) :fullStr;
        VAR aux   :fullStr;
            resto :LONGCARD;
    BEGIN
        resto := n MOD 1000;
        flagMil := FALSE;
        IF n < 1000 THEN
           RETURN GetCentena( n );
         ELSIF n = 1000 THEN
           RETURN 'mil';
         ELSIF (n > 1000) & (n < 2000) THEN
           Str.Concat( aux, 'mil ', GetCentena( resto ) );
           RETURN aux;
         ELSE
           (*
              Entre 2,000 .. 999,999.
           *)
           flagMil := TRUE;
           Str.Concat( aux, GetCentena( n DIV 1000 ), ' mil ' );
           flagMil := FALSE;
           Str.Append( aux, GetCentena( resto ) );
           RETURN aux;
        END;
    END GetMiles;

    PROCEDURE Millones( n :LONGCARD ) :fullStr;
        VAR aux   :fullStr;
            resto :LONGCARD;
    BEGIN
        resto := n MOD 1000000;
        flagMillon := FALSE;
        IF n < 1000000 THEN
           RETURN GetMiles( n );
         ELSIF n = 1000000 THEN
           RETURN 'un millon';
         ELSIF (n > 1000000) & (n < 2000000) THEN
           Str.Concat( aux, 'un millon ', GetMiles( resto ) );
           RETURN aux;
         ELSE
           (*
              Entre : 2,000,000 .. 999,999,999,999.
           *)
           flagMillon := TRUE;
           Str.Concat( aux, GetMiles( n DIV 1000000 ), ' millones ' );
           flagMillon := FALSE;
           Str.Append( aux, GetMiles( resto ) );
           RETURN aux;
        END;
    END Millones;

BEGIN
    IF n = 0
       THEN Str.Copy( frase, 'cero' );
       ELSE Str.Copy( frase, Millones( n ) );
    END;
END Espanol;

PROCEDURE Subst( VAR s1     :ARRAY OF CHAR;
                     Target :ARRAY OF CHAR;
                     New    :ARRAY OF CHAR );
(*
   Idem al Subst de la librer¡a std. del TopSpeed M2 ver 3.01.
   Pero soluciona el bug que posee cuando el string est  lleno.
*)
    VAR pos :CARDINAL;
BEGIN
    pos := Str.Pos( s1, Target );
    IF pos # MAX( CARDINAL ) THEN
       Str.Delete( s1, pos, Str.Length( Target ) );
       Str.Insert( s1, New, pos );
    END; (* if *)
END Subst;

PROCEDURE LOW( ch :CHAR ) :CHAR;
BEGIN
    IF ch IN CharSet{ 'A'..'Z' } THEN
       RETURN CHR( ORD( ch ) + 32 );
     ELSE
       RETURN ch;
    END; (* if *)
END LOW;

PROCEDURE Low( ch :CHAR ) :CHAR;
BEGIN
    CASE ch OF
       '¥' : RETURN '¤'; |
       'š' : RETURN ''; |
     ELSE
       RETURN LOW( ch );
    END; (* if *)
END Low;

PROCEDURE Lows( VAR str :ARRAY OF CHAR );
(*
   Idem al Caps de la libreria std. del TopSpeed M2 ver 3.01.
   Pero soluciona el problema de pasar a may£sculas los nombres en
   Castellano con asento o "¤".
*)
    VAR i :CARDINAL;
BEGIN
    IF str[0] = 0C THEN RETURN; END;
    FOR i := 0 TO Str.Length( str )-1 DO
        str[ i ] := Low( str[ i ] );
    END; (* for *)
END Lows;

PROCEDURE Cap( ch :CHAR ) :CHAR;
BEGIN
    CASE ch OF
       ' ' : RETURN 'A'; |
       '‚' : RETURN 'E'; |
       '¡' : RETURN 'I'; |
       '¢' : RETURN 'O'; |
       '£' : RETURN 'U'; |
       '' : RETURN 'š'; |
       '¤' : RETURN '¥'; |
     ELSE
       RETURN CAP( ch );
    END; (* if *)
END Cap;

PROCEDURE Caps( VAR str :ARRAY OF CHAR );
(*
   Idem al Caps de la libreria std. del TopSpeed M2 ver 3.01.
   Pero soluciona el problema de pasar a may£sculas los nombres en
   Castellano con asento o "¤".
*)
    VAR i :CARDINAL;
BEGIN
    IF str[0] = 0C THEN RETURN; END;
    FOR i := 0 TO Str.Length( str )-1 DO
        str[ i ] := Cap( str[ i ] );
    END; (* for *)
END Caps;

PROCEDURE Capitals( VAR str :ARRAY OF CHAR );
(*
   Dado un string, lo pasa a este con cada palabra comenzando con
   may£scula.
*)
    VAR i :CARDINAL;
BEGIN
    IF str[0] = 0C THEN RETURN; END;
    FOR i := 0 TO Str.Length( str ) - 1 DO
        IF i = 0 THEN
           str[ 0 ] := Cap( str[ 0 ] );
         ELSIF ~ (str[ i - 1 ] IN CharSet{ 'A'..'Z', 'a'..'z' }) THEN
           str[ i ] := Cap( str[ i ] );
         ELSE
           str[ i ] := Low( str[ i ] );
        END; (* if *)
    END; (* for *)
END Capitals;

PROCEDURE PasToStr( PasStr :ARRAY OF CHAR; VAR ModStr :ARRAY OF CHAR );
    VAR i, len :CARDINAL;
BEGIN
    len := ORD( PasStr[ 0 ] );
    FOR i := 1 TO HIGH( PasStr ) DO
        ModStr[ i - 1 ] := PasStr[ i ];
    END; (* for *)
    IF len <= HIGH( ModStr ) THEN
       ModStr[ len ] := CHR(0);
    END; (* if *)
END PasToStr;

PROCEDURE StrToCard( str :ARRAY OF CHAR ) :CARDINAL;
    VAR ok :BOOLEAN;
BEGIN
    RETURN VAL( CARDINAL, Str.StrToCard( str, 10, ok ) );
END StrToCard;

PROCEDURE ShtCardToStr( card :SHORTCARD; VAR str :ARRAY OF CHAR );
    VAR ok :BOOLEAN;
BEGIN
    Str.CardToStr( VAL( LONGCARD, card ), str, 10, ok );
END ShtCardToStr;

PROCEDURE CardToStr( card :CARDINAL; VAR str :ARRAY OF CHAR );
    VAR ok :BOOLEAN;
BEGIN
    Str.CardToStr( VAL( LONGCARD, card ), str, 10, ok );
END CardToStr;

PROCEDURE LngCardToStr( card :LONGCARD; VAR str :ARRAY OF CHAR );
    VAR ok :BOOLEAN;
BEGIN
    Str.CardToStr( card, str, 10, ok );
END LngCardToStr;

PROCEDURE IsUpper( VAR str :ARRAY OF CHAR ) :BOOLEAN;
BEGIN
    RETURN IsString( str, FullSet - CharSet{ 'a'..'z' });
END IsUpper;

PROCEDURE IsLower( VAR str :ARRAY OF CHAR ) :BOOLEAN;
BEGIN
    RETURN IsString( str, FullSet - CharSet{ 'A'..'Z' });
END IsLower;

PROCEDURE IsAlpha( VAR str :ARRAY OF CHAR ) :BOOLEAN;
BEGIN
    RETURN IsString( str, FullSet - CharSet{ '0'..'9' } );
END IsAlpha;

PROCEDURE IsString( VAR str:ARRAY OF CHAR; conj :CharSet ) :BOOLEAN;
    VAR i :CARDINAL;
BEGIN
    IF str[0] = 0C THEN RETURN TRUE; END;
    FOR i := 0 TO Str.Length( str )-1 DO
        IF ~ (str[ i ] IN conj) THEN
           RETURN FALSE;
        END; (* if *)
    END; (* for *)
    RETURN TRUE;
END IsString;

PROCEDURE Move( VAR s1 :ARRAY OF CHAR; s2 :ARRAY OF CHAR; pos :CARDINAL );
(*
   Dada una posici¢n en el string1 "s1" se sobreescriben los caracteres del
   string2 "s2" a partir de esa posici¢n.
*)
    VAR i, max1 :CARDINAL;
        j, max2 :CARDINAL;
        len     :CARDINAL;
BEGIN
    len  := Str.Length( s1 );
    max1 := HIGH( s1 );
    max2 := HIGH( s2 );
    j := 0;
    i := pos;
    WHILE (j <= max2) & (s2[ j ] # 0C) & (i <= max1) DO;
        s1[ i ] := s2[ j ];
        INC( i );
        INC( j );
    END; (* while *)
    IF (i <= max1) & (i >= len) THEN
       s1[ i ] := 0C;
    END; (* if *)
END Move;

PROCEDURE FlexRealToStr( r :LONGREAL; prec :CARDINAL;
                         VAR str :ARRAY OF CHAR; VAR ok :BOOLEAN );
(*
   Convierte un longreal: "r" en un string: "str" eliminando los
   ceros al final.
   los parametros son iguales a los del Str.FixRealToStr.
*)
BEGIN
    Str.FixRealToStr( r, prec, str, ok );
    ClearLastChar( str, '0' );
    ClearLastChar( str, '.' );
END FlexRealToStr;

PROCEDURE GetOnly( VAR str1 :ARRAY OF CHAR; str :ARRAY OF CHAR; set :CharSet );
(*
   Solo toma los caracteres que indique el set.
*)
    VAR i :CARDINAL;
BEGIN
    Str.Copy( str1, '' );
    IF str[0] = 0C THEN RETURN; END; (* if *)
    FOR i := 0 TO Str.Length( str ) - 1 DO
        IF str[ i ] IN set THEN
           Str.Append( str1, str[ i ] );
        END; (* if *)
    END; (* next : i *)
END GetOnly;

PROCEDURE SplitStr( str :ARRAY OF CHAR; long :CARDINAL;
                    VAR str1, str2 :ARRAY OF CHAR );
(*
   Divide el string: "str" en dos sub-strings:
   * long               ===> "str1".
   * HIGH( str ) - long ===> "str2".
*)
BEGIN
    Str.Copy( str1, '' );
    Str.Copy( str2, '' );
    Str.Slice( str1, str, 0,      long+1 );
    Str.Slice( str2, str, long+1, 255    );
END SplitStr;

PROCEDURE Left( str :ARRAY OF CHAR; VAR res :ARRAY OF CHAR );
    VAR i :CARDINAL;
BEGIN
    FillStr( res, FillChar );
    Str.Insert( res, str, 0 );
END Left;

PROCEDURE Center( str :ARRAY OF CHAR; VAR res :ARRAY OF CHAR; v :CARDINAL );
    VAR r, l, x, pos :CARDINAL;
BEGIN
    FillStr( res, FillChar );
    l := HIGH(res);
    IF v <= l THEN
       l := v;
    END; (* if *)
    IF l >= Str.Length(str) THEN
       r := (l - Str.Length(str));
       IF ODD(r) THEN
          x := 1;
        ELSE
          x := 0;
       END; (* if *)
       pos := (r DIV 2) + x;
     ELSE
       pos := 0;
    END; (* if *)
    Str.Insert( res, str, pos );
    Str.Delete( res, v, 255 );
END Center;

PROCEDURE Right( str :ARRAY OF CHAR; VAR res :ARRAY OF CHAR );
BEGIN
    StrAdj( res, str, HIGH(res));
END Right;

PROCEDURE AddHotKey( VAR str :ARRAY OF CHAR; VAR hots :ARRAY OF CHAR ) :CARDINAL;
    VAR pos   :CARDINAL;
        key   :ARRAY [ 0..0 ] OF CHAR;
        found :BOOLEAN;
        first :BOOLEAN;
        i     :CARDINAL;
        index :CARDINAL;
BEGIN
    found := FALSE;
    first := FALSE;
    key   := '@';
    i     := 0;
    index := 0;
    WHILE (i < HIGH(str)) & (str[i] # 0C) DO
        IF ~ found THEN
           IF str[i] = '~' THEN
              Str.Copy( key, str[i+1] );
              Str.Delete( str, i, 1 );
              index := i;
              found := TRUE;
            ELSIF ~ first & (str[i] IN Str.CHARSET{ 'A'..'Z', 'a'..'z', '0'..'9' }) THEN
              Str.Copy( key, str[i] );
              index := i;
              first := TRUE;
           END; (* if *)
        END; (* if *)
        INC(i);
    END; (* while *)
    Str.Append( hots, key );
    RETURN index;
END AddHotKey;

BEGIN
    FillChar := ' ';   (* Char de relleno "WrStrAdj". Defecto ==> ' '       *)
    ErrChar  := '*';   (* Char de error   "WrStrAdj". Defecto ==> '*'       *)
    ErrWrite := TRUE;
    ErrFill  := FALSE;
    UsedChar := FALSE;
    UsedErr  := FALSE;
END String.
