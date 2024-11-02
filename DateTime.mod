IMPLEMENTATION MODULE DateTime;

IMPORT String;
IMPORT Str, IO;

(* ------------------------------------------------------------------------ *)
(* Procedimientos sobre fecha.                                              *)
(* ------------------------------------------------------------------------ *)

TYPE rangoMeses = [ 1..12 ];
     MesesType  = ARRAY rangoMeses OF CARDINAL;

CONST mes = MesesType( 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 );

CONST DaysYear = 366;

TYPE DateStr = ARRAY [ 0..12 ] OF CHAR;
     TimeStr = ARRAY [ 0..12 ] OF CHAR;

VAR format      :FormatType;
    country     :CountryType;
    sort        :SortType;
    sep_char    :CHAR;
    is_sep      :BOOLEAN;
    century     :BOOLEAN;
    month_size  :CARDINAL;
    active_day  :BOOLEAN;
    espanol     :BOOLEAN;
    PrStr       :PrStrType;

VAR null_date :DateStr;
    dia_pos   :CARDINAL;
    mes_pos   :CARDINAL;
    ano_pos   :CARDINAL;

(* ------------------------------------------------------------------------ *)
(* Configuration's over date format.                                        *)
(* ------------------------------------------------------------------------ *)

PROCEDURE SetFormat( fmt :FormatType );

    PROCEDURE BoolComp( f, val :FormatType ) :BOOLEAN;
        VAR ok :BOOLEAN;
    BEGIN
        ok := (f & val) = val;
        RETURN ok;
    END BoolComp;

BEGIN
    SetSort( _dd_mm_yy_sort );
    SetSeparatorChar( '/' );
    SetEspanol( TRUE );       (* Bug: 16/04/94 *)
    SetSeparator( TRUE );           (* if on  ---> 16/08/67             *)
    SetCentury( FALSE );            (* if on  ---> 19xx                 *)
    SetMonthSize( 2 );              (* SetMonthSize( 2 )                *)
    SetActiveDay( TRUE );           (* if on  ---> 16/08/67             *)

    format := fmt;

    IF BoolComp( format, _mm_dd_yy    ) THEN SetSort( _mm_dd_yy_sort ); END;
    IF BoolComp( format, _yy_mm_dd    ) THEN SetSort( _yy_mm_dd_sort ); END;
    IF BoolComp( format, _sep_off     ) THEN SetSeparator( FALSE );     END;
    IF BoolComp( format, _century_on  ) THEN SetCentury( TRUE );        END;
    IF BoolComp( format, _day_off     ) THEN SetActiveDay( FALSE );     END;
    IF BoolComp( format, _espanol_off ) THEN SetEspanol( FALSE );       END;
    IF BoolComp( format, _month_3     ) THEN SetMonthSize( 3 );         END;
    IF BoolComp( format, _sep_minus   ) THEN SetSeparatorChar( '-' );   END;
    IF BoolComp( format, _sep_point   ) THEN SetSeparatorChar( '.' );   END;
END SetFormat;

PROCEDURE GetFormat() :FormatType;
BEGIN
    RETURN format;
END GetFormat;

PROCEDURE SetCountry( cty :CountryType );
BEGIN
    CASE cty OF
       _argentina,
       _british,
       _french,
       _german,
       _italian   : SetSort( _dd_mm_yy_sort ); |
       _USA       : SetSort( _mm_dd_yy_sort ); |
       _ANSI      : SetSort( _yy_mm_dd_sort ); |
    END; (* case *)
    CASE cty OF
       _argentina,
       _british,
       _french,
       _USA,
       _ANSI      : SetSeparatorChar( '/' ); |
       _german    : SetSeparatorChar( '.' ); |
       _italian   : SetSeparatorChar( '-' ); |
    END; (* cty *)
    CASE cty OF
       _british,
       _french,
       _german,
       _italian,
       _USA,
       _ANSI      : SetEspanol( FALSE ); |
       _argentina : SetEspanol( TRUE );  |
    END; (* case *)
END SetCountry;

PROCEDURE GetCountry() :CountryType;
BEGIN
    RETURN country;
END GetCountry;

PROCEDURE SetSort( srt :SortType );
BEGIN
    sort := srt;
    CASE sort OF
       _dd_mm_yy_sort : dia_pos := 0; mes_pos := 1; ano_pos := 2; |
       _mm_dd_yy_sort : dia_pos := 1; mes_pos := 0; ano_pos := 2; |
       _yy_mm_dd_sort : dia_pos := 2; mes_pos := 1; ano_pos := 0; |
    END; (* case *)
END SetSort;

PROCEDURE GetSort() :SortType;
BEGIN
    RETURN sort;
END GetSort;

PROCEDURE SetSeparatorChar( chr :CHAR );
    CONST null_1 = ' -  -  -';
          null_2 = '        ';
BEGIN
    sep_char := chr;
    CASE chr OF
       '/',
       '.' : null_date := null_1; |
       ELSE  null_date := null_2;
    END; (* case *)
    null_date[2] := chr;
    null_date[5] := chr;
END SetSeparatorChar;

PROCEDURE GetSeparatorChar() :CHAR;
BEGIN
    RETURN sep_char;
END GetSeparatorChar;

PROCEDURE SetSeparator( sep :BOOLEAN );
BEGIN
    is_sep := sep;
END SetSeparator;

PROCEDURE IsSeparator() :BOOLEAN;
BEGIN
    RETURN is_sep;
END IsSeparator;

PROCEDURE SetCentury( on :BOOLEAN );
BEGIN
    century := on;
END SetCentury;

PROCEDURE IsCentury() :BOOLEAN;
BEGIN
    RETURN century;
END IsCentury;

PROCEDURE SetMonthSize( size :CARDINAL );
BEGIN
    month_size := size;
END SetMonthSize;

PROCEDURE GetMonthSize() :CARDINAL;
BEGIN
    RETURN month_size;
END GetMonthSize;

PROCEDURE SetActiveDay( act :BOOLEAN );
BEGIN
    active_day := act;
END SetActiveDay;

PROCEDURE IsActiveDay() :BOOLEAN;
BEGIN
    RETURN active_day;
END IsActiveDay;

PROCEDURE SetEspanol( on :BOOLEAN );
BEGIN
    espanol := on;
END SetEspanol;

PROCEDURE GetEspanol() :BOOLEAN;
BEGIN
    RETURN espanol;
END GetEspanol;

(* ------------------------------------------------------------------------ *)
(* procedure & functions over date & time.                                  *)
(* ------------------------------------------------------------------------ *)

PROCEDURE DaysInMonth( m :CARDINAL ) :CARDINAL;
(*
   Dado un mes devuelve la cantidad de d¡as de ese mes.
   (* Ene = 1..Dic = 12 *)
*)
BEGIN
    RETURN mes[ m ];
END DaysInMonth;

PROCEDURE MaxDaysInMonth( mes, ano :CARDINAL ) :CARDINAL;
(*
   Idem al anterior pero teniendo en cuenta los a¤os bisiestos.
   (* Ene = 1..Dic = 12 *)
*)
    VAR max_days :CARDINAL;
BEGIN
    max_days := CARDINAL(DaysInMonth(mes)) +
                CARDINAL((ano MOD 4 = 0) & (mes = 2));
    IF mes = 2 THEN DEC(max_days); END; (* if *)
    RETURN max_days;
END MaxDaysInMonth;

PROCEDURE DayOfWeek( date :DateType ) :DayType;
   VAR century, yr, dw  :INTEGER;
       day_week         :DayType;
       Day, Month, Year :INTEGER;
       d, m, a          :CARDINAL;
BEGIN
    DateUnCompress( date, d, m, a );
    Day   := d;
    Month := m;
    Year  := a + BaseYear;
    IF Month < 3 THEN
       INC( Month, 10 );
       DEC( Year );
     ELSE
       DEC( Month, 2 );
    END; (* if *)
    century := Year DIV 100;
    yr := Year MOD 100;
    dw := (((((26 * Month - 2) DIV 10) + Day + yr + (yr DIV 4) +
            (century DIV 4) - (2 * century)) MOD 7) + 1) MOD 7;
    IF dw < 0 THEN
       INC( dw, 7 );
    END; (* if *)
    IF dw = 0 THEN dw := 6; ELSE DEC(dw); END; (* if *)
    day_week := DayType( dw );
    RETURN day_week;
END DayOfWeek;

PROCEDURE CtrlDate( d, m, a :CARDINAL ) :BOOLEAN;
(*
   Control de los valores de la "fecha".
*)
BEGIN
    RETURN (d > 0) & (d <= mes[ m ]) &
           (m > 0) & (m <= 12)       &
           (a > 0) & (a <= MaxYear)
END CtrlDate;

PROCEDURE DateCompress( d, m, a :CARDINAL ) :DateType;
(*
   Comprime una fecha dada por: d¡a, mes y a¤o a un cardinal.
*)
    VAR i, sum, res :CARDINAL;
BEGIN
    IF (m < 1) OR (m > 12)      THEN m := 1; END;
    IF (d < 1) OR (d > mes[m])  THEN d := 1; END;
    IF (a < 1) OR (a > MaxYear) THEN a := 1; END;
    sum := 0;
    FOR i := 1 TO m - 1 DO
        INC( sum, mes[ i ] )
    END;
    res := sum + (d - 1) + DaysYear * (a - 1);
    RETURN res;
END DateCompress;

PROCEDURE DateUnCompress( fecha :DateType; VAR d, m, a :CARDINAL );
(*
   Descomprime una fecha en: d¡a, mes, a¤o.
*)
    VAR num, i, sum :CARDINAL;
BEGIN
    num := fecha;
    a   := (num DIV DaysYear) + 1;
    num := num - (a - 1) * DaysYear;
    m   := 0;
    sum := 0;
    i   := 1;
    WHILE mes[ i ] <= (num - sum) DO
        INC( m );
        INC( sum, mes[ i ] );
        INC( i )
    END;
    INC( m );
    d := (num - sum) + 1;
END DateUnCompress;

PROCEDURE WrDate( fecha :DateType );
    VAR str :DateStr;
BEGIN
    DateToStr( fecha, str );
    IO.WrStr( str );
END WrDate;

PROCEDURE PrDate( fecha :DateType );
    VAR str :DateStr;
BEGIN
    DateToStr( fecha, str );
    PrStr( str );
END PrDate;

PROCEDURE WrDateFormat( fecha :DateType; format :FormatType );
    VAR old :FormatType;
BEGIN
    old := GetFormat();
    SetFormat( format );
    WrDate( fecha );
    SetFormat( old );
END WrDateFormat;

PROCEDURE PrDateFormat( fecha :DateType; format :FormatType );
    VAR old :FormatType;
BEGIN
    old := GetFormat();
    SetFormat( format );
    PrDate( fecha );
    SetFormat( old );
END PrDateFormat;

PROCEDURE GetDate() :DateType;
(*
   Toma la fecha del sistema y la comprime.
*)
    VAR d, m, a :CARDINAL;
        weekDay :DayType;
        date    :DateType;
BEGIN
    Lib.GetDate( a, m, d, weekDay );
    IF a > BaseYear THEN
       a := a - BaseYear;
    END; (* if *)
    date := DateCompress( d, m, a );
    RETURN date;
END GetDate;

PROCEDURE GetDateCard( VAR d, m, a :CARDINAL );
(*
   Tom la fecha del sistema y la devuelve como d¡a, mes y a¤o.
*)
    VAR weekDay :DayType;
BEGIN
    Lib.GetDate( a, m, d, weekDay );
    IF a > BaseYear THEN
       a := a - BaseYear;
    END; (* if *)
END GetDateCard;

PROCEDURE GetDateStr( VAR d, m, a :ARRAY OF CHAR );
    VAR dc, mc, ac :CARDINAL;
        weekDay    :DayType;
        ok         :BOOLEAN;
BEGIN
    Lib.GetDate( ac, mc, dc, weekDay );
    IF ac > BaseYear THEN
       ac := ac - BaseYear;
    END; (* if *)
    Str.CardToStr( LONGCARD(dc), d, 10, ok ); IF ~ ok THEN Str.Copy(d,''); END;
    Str.CardToStr( LONGCARD(mc), m, 10, ok ); IF ~ ok THEN Str.Copy(m,''); END;
    Str.CardToStr( LONGCARD(ac), a, 10, ok ); IF ~ ok THEN Str.Copy(a,''); END;
END GetDateStr;

PROCEDURE Day( fecha :DateType ) :CARDINAL;
    VAR d, m, a :CARDINAL;
BEGIN
    DateUnCompress( fecha, d, m, a );
    RETURN d;
END Day;

PROCEDURE Month( fecha :DateType ) :CARDINAL;
    VAR d, m, a :CARDINAL;
BEGIN
    DateUnCompress( fecha, d, m, a );
    RETURN m;
END Month;

PROCEDURE Year( fecha :DateType ) :CARDINAL;
    VAR d, m, a :CARDINAL;
BEGIN
    DateUnCompress( fecha, d, m, a );
    RETURN a;
END Year;

PROCEDURE DateToStr( f :DateType; VAR s :ARRAY OF CHAR );
(*
   Convierte la fecha : "f" a un string : "s".
*)

    VAR str       :ARRAY [ 0..31 ] OF CHAR;
        oldch     :CHAR;
        i         :CARDINAL;
        wrt       :BOOLEAN;
        val       :CARDINAL;
        adj       :CARDINAL;
        d, m, a   :CARDINAL;
        max_write :CARDINAL;
BEGIN
    IF f = 0 THEN
       Str.Copy( s, null_date );
     ELSE
       oldch := String.FillChar;
       String.FillChar := '0';
       Str.Copy( s, '' );
       str := '';
       max_write := MAX(range_date);
       DateUnCompress( f, d, m, a );
       FOR i := MIN(range_date) TO MAX(range_date) DO
           wrt := TRUE;
           adj := 2;
           IF i = dia_pos THEN
              wrt := active_day;
              val := d;
              String.Card2Str( str, val, adj );
           ELSIF i = mes_pos THEN
              val := m;
              adj := month_size;
              IF adj > 2 THEN
                 Mes( val, str );
                 str[adj] := 0C;
               ELSE
                 String.Card2Str( str, val, adj );
              END; (* if *)
           ELSE (* i = ano_mes *)
              IF century THEN
                 adj := 4;
                 a   := a + BaseYear;
               ELSE
                 a   := a MOD 100;
              END; (* if *)
              val := a;
              String.Card2Str( str, val, adj );
           END; (* if *)
           IF wrt THEN
              Str.Append( s, str );
              IF is_sep & (i < max_write) THEN
                 Str.Append( s, sep_char );
              END; (* if *)
           END; (* if *)
       END; (* next *)
       String.FillChar := oldch;
    END; (* if *)
END DateToStr;

PROCEDURE DateToFormatStr( f :DateType; format :FormatType; VAR s :ARRAY OF CHAR );
(*
   Convierte la fecha : "f" a un string : "s".
   Con el formato : indicado.
*)
    VAR old :FormatType;
BEGIN
    old := GetFormat();
    SetFormat( format );
    DateToStr( f, s );
    SetFormat( old );
END DateToFormatStr;

PROCEDURE Dia( i :DayType; VAR dStr :ARRAY OF CHAR );
(*
   Toma el d¡a indicado por : "i".
*)
    TYPE dayStr = ARRAY [ 0..8 ] OF CHAR;
         dayType = ARRAY DayType OF dayStr;

    CONST daysARG = dayType( 'domingo', 'lunes',   'martes', 'mi‚rcoles',
                             'jueves',  'viernes', 's bado' );

    CONST daysUSA = dayType( 'sunday',   'monday',  'tuesday', 'wednesday',
                             'thursday', 'friday',  'saturday' );

BEGIN
    IF espanol THEN
       Str.Copy( dStr, daysARG[ i ] );
     ELSE
       Str.Copy( dStr, daysUSA[ i ] );
    END; (* if *)
END Dia;

PROCEDURE Mes( i :CARDINAL; VAR mStr :ARRAY OF CHAR );
(*
   Toma el mes indicado por : "i".
*)
    TYPE mesStr = ARRAY [ 0..8 ] OF CHAR;
         mesType = ARRAY rangoMeses OF mesStr;

    CONST mesesARG = mesType( 'enero',   'febrero',   'marzo',
                              'abril',   'mayo',      'junio',
                              'julio',   'agosto',    'setiembre',
                              'octubre', 'noviembre', 'diciembre' );

    CONST mesesUSA = mesType( 'january', 'february',  'march',
                              'april',   'may',       'june',
                              'july',    'august',    'september',
                              'october', 'november',  'december' );

BEGIN
    IF espanol THEN
       Str.Copy( mStr, mesesARG[ i ] );
     ELSE
       Str.Copy( mStr, mesesUSA[ i ] );
    END; (* if *)
END Mes;

PROCEDURE GetDia( VAR dStr :ARRAY OF CHAR );
(*
   Tomo el dia actual y lo devuelve en un string.
*)
    VAR d, m, a :CARDINAL;
        weekDay :DayType;
BEGIN
    Lib.GetDate( a, m, d, weekDay );
    Dia( weekDay, dStr );
END GetDia;

PROCEDURE GetMes( VAR mStr :ARRAY OF CHAR );
(*
   Toma el mes actual y lo devuelve en un string.
*)
    VAR d, m, a :CARDINAL;
        weekDay :DayType;
BEGIN
    Lib.GetDate( a, m, d, weekDay );
    Mes( m, mStr );
END GetMes;

(* ------------------------------------------------------------------------ *)
(* Procedimientos sobre hora                                                *)
(* ------------------------------------------------------------------------ *)

PROCEDURE CtrlTime( h, m, s :CARDINAL ) :BOOLEAN;
(*
   Control de los valores de la "hora".
*)
BEGIN
    RETURN (h < 24) & (m < 60) & (s < 60);
END CtrlTime;

VAR HMS :HMSType;
    PM  :BOOLEAN;

PROCEDURE SetHMS( hms :HMSType );
BEGIN
    HMS := hms;
END SetHMS;

PROCEDURE SetPM( on :BOOLEAN );
BEGIN
    PM := on;
END SetPM;

PROCEDURE WrTime( time :CARDINAL );
    VAR str :TimeStr;
BEGIN
    TimeToStr( time, str );
    IO.WrStr( str );
END WrTime;

PROCEDURE PrTime( time :CARDINAL );
    VAR str :TimeStr;
BEGIN
    TimeToStr( time, str );
    PrStr( str );
END PrTime;

PROCEDURE GetTime() :TimeType;
(*
   Toma la hora del sistema y la comprime.
*)
    VAR h, m, s, hs :CARDINAL;
        hora        :TimeType;
BEGIN
    Lib.GetTime( h, m, s, hs );
    hora := TimeCompress( h, m, s );
    RETURN hora;
END GetTime;

PROCEDURE GetTimeCard( VAR h, m, s :CARDINAL );
(*
   Tomo la hora del sistema en "cardinales".
*)
    VAR hs :CARDINAL;
BEGIN
    Lib.GetTime( h, m, s, hs );
END GetTimeCard;

PROCEDURE TimeToStr( t :TimeType; VAR s :ARRAY OF CHAR );
(*
   Convierte la hora : "t" a un string : "s".
   Con el orden : seleccionado.
*)
    VAR fc         :CHAR;
        v1, v2, v3 :CARDINAL;
        str        :ARRAY [ 0..20 ] OF CHAR;
        pm         :BOOLEAN;
BEGIN
    fc := String.FillChar;
    TimeUnCompress( t, v1, v2, v3 );

    IF PM THEN
       pm := v1 > 11;
       IF pm THEN v1 := v1 - 12; END; (* if *)
       IF v1 = 0 THEN v1 := 12; END; (* if *)
    END; (* if *)

    String.FillChar := ' ';
    String.Card2Str( str, v1, 2 ); Str.Copy( s, str );
    IF ~ (HMS = _H) THEN
       Str.Append( s, ':' );
       String.FillChar := '0';
       String.Card2Str( str, v2, 2 ); Str.Append( s, str );
    END; (* if *)

    IF HMS = _HMS THEN
       Str.Append( s, ':' );
       String.FillChar := '0';
       String.Card2Str( str, v3, 2 ); Str.Append( s, str );
    END; (* if *)

    IF PM THEN
       IF pm
          THEN Str.Append( s, 'pm' );
          ELSE Str.Append( s, 'am' );
       END; (* if *)
    END; (* if *)
    String.FillChar := fc;
END TimeToStr;

(* ------------------------------------------------------------------------ *)
(* Funciones sobre Fecha y Hora en el Formato del Sistema Operativo.        *)
(* ------------------------------------------------------------------------ *)

(*
   Comprime y descomprime una fecha en formato del DOS a "d¡a, mes y a¤o".
*)

PROCEDURE DateDosCompress( d, m, a :CARDINAL ) :CARDINAL;
    VAR fecha :CARDINAL;
BEGIN
    IF a > BaseYear THEN a := a - BaseYear; END; (* if *)
    IF a < 80       THEN a := 80;           END; (* if *)
    fecha := ( a - 80 ) * 512 + m * 32 + d;
    RETURN fecha;
END DateDosCompress;

PROCEDURE DateDosUnCompress( fecha :CARDINAL; VAR d, m, a :CARDINAL );
BEGIN
    a     := (fecha DIV 512) + 80;
    fecha :=  fecha MOD 512;
    m     :=  fecha DIV  32;
    d     :=  fecha MOD  32;
END DateDosUnCompress;

PROCEDURE GetDateDos() :CARDINAL;
    VAR a, m, d :CARDINAL;
        day     :DayType;
BEGIN
    Lib.GetDate( a, m, d, day );
    RETURN DateDosCompress( d, m, a );
END GetDateDos;

(*
   Comprime y descomprime una hora en formato del DOS a "hora, min y seg".
*)

PROCEDURE TimeDosCompress( h, m, s :CARDINAL ) :CARDINAL;
    VAR time :CARDINAL;
BEGIN
    time := h * 2048 + m * 32 + s DIV 2;
    RETURN time;
END TimeDosCompress;

PROCEDURE TimeDosUnCompress( hora :CARDINAL; VAR h, m, s :CARDINAL );
    CONST horBits = BITSET{ 11..15 };
          minBits = BITSET{  5..10 };
          secBits = BITSET{  0.. 4 };
BEGIN
    h := CARDINAL(BITSET( hora ) * horBits) >> 11;
    m := CARDINAL(BITSET( hora ) * minBits) >>  5;
    s := CARDINAL(BITSET( hora ) * secBits);
END TimeDosUnCompress;

PROCEDURE GetTimeDos() :CARDINAL;
    VAR h, m , s, c :CARDINAL;
BEGIN
    Lib.GetTime( h, m, s, c );
    RETURN TimeDosCompress( h, m, s );
END GetTimeDos;

(* ------------------------------------------------------------------------ *)
(* Definici¢n del procedimiento de Imprimir.                                *)
(* ------------------------------------------------------------------------ *)

PROCEDURE NullPrStr( str :ARRAY OF CHAR );
END NullPrStr;

PROCEDURE SetPrStr( _proc :PrStrType );
BEGIN
    PrStr := _proc;
END SetPrStr;

BEGIN
    (*
       Inicializaci¢n de "Date".
    *)
    SetFormat( _std_day );           (* setea: el formato std. *)
    (*
       Inicializaci¢n de "Time".
    *)
    SetHMS( _HM );
    SetPM( TRUE );
    SetPrStr( NullPrStr );
END DateTime.
