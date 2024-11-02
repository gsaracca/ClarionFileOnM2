IMPLEMENTATION MODULE dBase;

IMPORT String;
IMPORT Str;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

CONST CharSet ::= Str.CHARSET;

CONST dbase_2        =  02H;
      dbase_3        =  03H;
      dbase_3_w_memo = 083H;
      max_dec        =   15;
      max_memo_size  =  512;
      max_head_size  = 4129;
      max_fields     =  128;        (* en realidad son 128 campos       *)
      max_rec_size   = 4000;        (* en realidad es 4000 bytes        *)
      eoh_mark       = CHAR(00DH);  (* End Of Head: solo dBase III/III+ *)
      eof_mark       = CHAR(01AH);  (* End Of File                      *)

CONST del_rec_char   = '*';
      undel_rec_char = ' ';
      blank_char     = ' ';

CONST CharOk = CharSet{ CHR(32)..CHR(254) };
      NumeOk = CharSet{ CHR(32), '0'..'9', '-', '.' };
      BoolOk = CharSet{ 'T', 't', 'Y', 'y', 'F', 'f', 'N', 'n', '?' };
      DateOk = CharSet{ CHR(32), '0'..'9' };
      MemoOk = CharSet{ CHR(32), '0'..'9' };

(* ------------------------------------------------------------------------ *)
(* Tipos de datos f¡sicos de la base de datos.                              *)
(* ------------------------------------------------------------------------ *)

TYPE UpDateType       = ARRAY [ 0.. 2 ] OF SHORTCARD;
     (*
         undate[ 0 ] :===> last update -> year (-1900).
         update[ 1 ] :===> last update -> month.
         update[ 2 ] :===> last update -> day.
     *)
     ReservHeadType   = ARRAY [ 0..19 ] OF SHORTCARD;
     ReservFieldsType = ARRAY [ 0..13 ] OF SHORTCARD;

CONST NullReservHead   = ReservHeadType( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0 );
      NullReservFields = ReservFieldsType( 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                           0, 0, 0, 0 );

TYPE PhHeadType = RECORD
         ver       :SHORTCARD;         (* N£mero de versi¢n *)
         update    :UpDateType;        (* last update. *)
         rec_count :LONGCARD;          (* n£mero de registros. *)
         head_size :CARDINAL;          (* long de la cabecera. *)
         rec_size  :CARDINAL;          (* long del registro.   *)
         reserved  :ReservHeadType;    (* bytes reservados versi¢n 1.00 *)
     END; (* PhFileHead *)

TYPE PhFieldDefType = RECORD
         name   :NameType;          (* debe ser terminado en cero *)
         type   :CHAR;              (* C, N, L, D, M *)
         addr   :LONGCARD;          (* direcci¢n del campo en memoria. *)
         long   :SHORTCARD;         (* longitud del campo. *)
         dec    :SHORTCARD;         (* campos decimales. *)
         reserv :ReservFieldsType;  (* reservados versi¢n 1.00 *)
     END; (* PhFieldType *)

CONST ph_head_size      = SIZE( PhHeadType );
      ph_field_def_size = SIZE( PhFieldDefType );

(* ------------------------------------------------------------------------ *)
(* Tipos de datos l¢gicos de la base de datos (en memoria)                  *)
(* ------------------------------------------------------------------------ *)

TYPE HeadType = RECORD
         ver       :SHORTCARD;           (* N£mero de versi¢n *)
         update    :UpDateType;          (* last update. *)
         rec_count :FM.IndexType;            (* n£mero de registros. *)
         head_size :CARDINAL;            (* long de la cabecera. *)
         rec_size  :CARDINAL;            (* long del registro.   *)
     END; (* FileHead *)

TYPE FieldDefType = RECORD
         name   :NameType;         (* debe ser terminado en cero *)
         type   :CHAR;             (* C, N, L, D, M *)
         long   :CARDINAL;         (* longitud del campo. *)
         dec    :CARDINAL;         (* campos decimales. *)
         offset :CARDINAL;         (* offset del campo en el registro *)
     END; (* FieldType *)

CONST head_size      = SIZE( HeadType );
      field_def_size = SIZE( FieldDefType );

TYPE ptrFields = POINTER TO ARRAY [ 0..max_fields ] OF FieldDefType;

TYPE BaseType = POINTER TO BaseRec;
     BaseRec = RECORD
          file        :FM.File;       (* descriptor del archivo. *)
          name        :FM.PathStr;    (* nombre del archivo.     *)
          field_count :CARDINAL;       (* cantidad de campos.     *)
          fields      :ptrFields;      (* descripci¢n de los registros. *)
          head        :HeadType;       (* cabecera del archivo. *)
     END; (* FileRec *)

(* ------------------------------------------------------------------------ *)
(*                  Comienzo de la implementaci¢n                           *)
(* ------------------------------------------------------------------------ *)

VAR OpResult :CARDINAL;

PROCEDURE MakeName( dir, name :ARRAY OF CHAR; VAR full :ARRAY OF CHAR );
BEGIN
    FM.MakeName( dir, name, FM.DB_DB3_FEXT, full );
END MakeName;

PROCEDURE WriteHead( b :BaseType );
    VAR ph_head  :PhHeadType;
        ph_field :PhFieldDefType;
        pos      :FM.IndexType;
        i        :CARDINAL;
BEGIN
    WITH b^ DO
       ph_head.ver       := head.ver;        (* N£mero de versi¢n *)
       ph_head.update    := head.update;     (* last update. *)
       ph_head.rec_count := head.rec_count;  (* n£mero de registros. *)
       ph_head.head_size := head.head_size;  (* long de la cabecera. *)
       ph_head.rec_size  := head.rec_size;   (* long del registro.   *)
       ph_head.reserved  := NullReservHead;  (* los lleno con nada.  *)
       pos := FM.GetPos( file );
       FM.Seek( file, 0 );
       FM.WrBlock( file, ph_head, ph_head_size );
       (*
          descripci¢n de campos de la base de datos.
       *)
       FOR i := 0 TO field_count - 1 DO
           WITH fields^[ i ] DO
              ph_field.name   := name;              (* nombre del registro. *)
              ph_field.type   := type;              (* C, N, L, D, M *)
              ph_field.addr   := 0;                 (* direcci¢n del campo en memoria. *)
              ph_field.long   := SHORTCARD( long ); (* longitud del campo. *)
              ph_field.dec    := SHORTCARD( dec  ); (* campos decimales. *)
              ph_field.reserv := NullReservFields;
           END; (* with : fields^[ i ] *)
           FM.WrBlock( file, ph_field, ph_field_def_size );
       END; (* for : i *)
       FM.WrBlock( file, eoh_mark, SIZE(eof_mark) );
       FM.Seek( file, pos );
       FM.Flush( file );
    END; (* with *)
END WriteHead;

PROCEDURE LegalType( t :CHAR ) :BOOLEAN;
BEGIN
    RETURN t IN CharSet{ 'C', 'N', 'D', 'L', 'M' };
END LegalType;

PROCEDURE Check( base :BaseType );

    PROCEDURE ExistsField( j :CARDINAL; name :NameType ) :BOOLEAN;
        VAR i :CARDINAL;
    BEGIN
        FOR i := 0 TO base^.field_count - 1 DO
            IF (j # i) &
               (Str.Compare( name, base^.fields^[ i ].name ) = 0)
             THEN
               RETURN FALSE;
            END; (* if *)
        END; (* for *)
        RETURN TRUE;
    END ExistsField;

    VAR i :CARDINAL;

BEGIN
    WITH base^ DO
       IF field_count > max_fields THEN
          OpResult := too_many_fields; RETURN;
       END; (* if *)
       (*
          cheque de los campos.
       *)
       FOR i := 0 TO field_count - 1 DO
           WITH fields^[ i ] DO
              IF Str.Length( name ) = HIGH( name ) THEN
                 OpResult := field_name_error; RETURN;
              END; (* if *)
              IF ExistsField( i, name ) THEN
                 OpResult := field_name_exists; RETURN;
              END; (* if *)
              IF ~ LegalType( type ) THEN
                 OpResult := field_invalid_type; RETURN;
              END; (* if *)
              CASE type OF
                 'C' : IF (long < 1) OR (long > 254) THEN
                          OpResult := field_invalid_long; RETURN;
                       END; (* if *); |
                 'N' : IF (long < 1) OR (long > 19) THEN
                          OpResult := field_invalid_long; RETURN;
                       END; (* if *) |
                 'D' : IF long # SIZE( DateType ) THEN
                          OpResult := field_invalid_long; RETURN;
                       END; (* if *)      |
                 'L' : IF long # SIZE( BoolType ) THEN
                          OpResult := field_invalid_long; RETURN;
                       END; (* if *)   |
                 'M' : IF long # SIZE( MemoType ) THEN
                          OpResult := field_invalid_long; RETURN;
                       END; (* if *) |
              END; (* case *)
              IF (type = 'N') & ((dec > max_dec) OR (dec > (long - 2))) THEN
                 OpResult := field_invalid_dec; RETURN;
              END; (* if *)
           END; (* with : fields^[ i ] *)
       END; (* for *)
       (*
          chequeo de la cabecera.
       *)
       WITH head DO
          IF (ver # dbase_3) OR (ver # dbase_3_w_memo) THEN
             OpResult := no_dbase_file; RETURN;
          END; (* if *)
          IF head_size > max_head_size THEN
             OpResult := head_too_big; RETURN;
          END; (* if *)
          IF rec_size > max_rec_size THEN
             OpResult := rec_too_big; RETURN;
          END; (* if *)
       END; (* with : head *)
    END; (* with : base *)
    OpResult := no_error;
END Check;

PROCEDURE Create( n :ARRAY OF CHAR; fieldsDef :ARRAY OF FieldDef ) :BaseType;
    VAR base        :BaseType;
        i           :CARDINAL;
        field       :FieldDefType;
        a, m, d     :CARDINAL;
        day         :DateTime.DayType;
        record_size :CARDINAL;
        memo        :BOOLEAN;
BEGIN
    NEW( base );
    WITH base^ DO
       MakeName( '', n, name );                (* nombre del archivo.    *)
       file        := FM.Create( name );       (* descriptor del archivo *)
       field_count := HIGH( fieldsDef ) + 1;   (* cantidad de registros. *)
       (*
          campos del registro
       *)
       ALLOCATE( fields, field_def_size * field_count );
       memo := FALSE;
       record_size := 0;
       FOR i := 0 TO field_count - 1 DO
           WITH field DO
              String.FillStr( name, 0C );
              Str.Copy( name, fieldsDef[ i ].name ); (* debe ser terminado en cero *)
              CASE fieldsDef[ i ].type OF
                 Character : type := 'C'; |
                 Numeric   : type := 'N'; |
                 Logical   : type := 'L'; fieldsDef[ i ].long :=  1; |
                 Date      : type := 'D'; fieldsDef[ i ].long :=  8; |
                 Memo      : type := 'M'; fieldsDef[ i ].long := 10;
                             memo := TRUE; |
              END; (* case *)
              IF (type # 'N') THEN
                 fieldsDef[ i ].dec := 0;
              END; (* if *)
              long        := fieldsDef[ i ].long;  (* longitud del campo. *)
              dec         := fieldsDef[ i ].dec;   (* campos decimales. *)
              offset      := record_size;          (* offset del registro. *)
              record_size := record_size + long;   (* longitud del reg. *)
           END; (* with *)
           fields^[ i ] := field;
       END; (* for *)
       (*
          cabecera del archivo.
       *)
       WITH head DO
          IF memo                            (* versi¢n de la base de datos *)
             THEN ver := dbase_3_w_memo;
             ELSE ver := dbase_3;
          END; (* if *)
          update[ 2 ] := 0;
          update[ 1 ] := 0;
          update[ 0 ] := 0;
          rec_count   := 0;                  (* n£mero de registros. *)
          (*
            long de la cabecera.
          *)
          head_size := (field_count * ph_field_def_size) + ph_head_size + 1;
          rec_size  := record_size + 1;
       END; (* with *)
    END; (* base *)
    Check( base );      (* Control de errores. *)
    WriteHead( base );  (* Escritura de la cabecera de la base de datos. *)
    Seek( base, 0 );    (* Coloco el puntero al primer lugar. *)
    RETURN base;
END Create;

PROCEDURE CheckHead( f :FM.File; ph_head :PhHeadType ) :BOOLEAN;
    VAR file_size    :FM.IndexType;
        regs_fisicos :FM.IndexType;
BEGIN
    WITH ph_head DO
        IF (ver # dbase_3) & (ver # dbase_3_w_memo) THEN
           OpResult := no_dbase_file; RETURN FALSE;
        END; (* if *)
        IF head_size > max_head_size THEN
           OpResult := head_size_too_big; RETURN FALSE;
        END; (* if *)
        IF ((head_size-1) MOD 32) # 0 THEN
           OpResult := head_size_corrupted; RETURN FALSE;
        END; (* if *)
        IF rec_size > max_rec_size THEN
           OpResult := rec_size_too_big; RETURN FALSE;
        END; (* if *)
        file_size := FM.Size( f );
        regs_fisicos := file_size - FM.IndexType( head_size+1 );
        IF (regs_fisicos MOD FM.IndexType( rec_size )) # 0 THEN
           OpResult := corrupted_file; RETURN FALSE;
        END; (* if *)
        IF FM.IndexType(((rec_count * FM.IndexType(rec_size)) + FM.IndexType(head_size+1))) #
           FM.Size( f ) THEN
           OpResult := diferent_record_count; RETURN FALSE;
        END; (* if *)
    END; (* with : ph_head *)
    RETURN TRUE;
END CheckHead;

PROCEDURE Open( name :ARRAY OF CHAR ) :BaseType;
    VAR base        :BaseType;
        status      :BOOLEAN;
        nameBase    :FM.PathStr;
        f           :FM.File;
        result      :CARDINAL;
        ph_head     :PhHeadType;
        ph_field    :PhFieldDefType;
        record_size :CARDINAL;
        i           :CARDINAL;
BEGIN
    MakeName( '', name, nameBase );
    f := FM.Open( nameBase );
    IF result # 0 THEN
       OpResult := io_error;
       RETURN NIL;
    END; (* if *)
    (*
       creo la base de datos.
    *)
    NEW( base );
    WITH base^ DO
        file := f;
        name := nameBase;
        FM.RdBlock( f, ph_head, ph_head_size );
        IF CheckHead( f, ph_head ) THEN
           field_count := (ph_head.head_size - ph_head_size) DIV ph_field_def_size;
           WITH head DO
              ver       := ph_head.ver;          (* N£mero de versi¢n *)
              update    := ph_head.update;       (* last update. *)
              rec_count := ph_head.rec_count;    (* n£mero de registros. *)
              head_size := ph_head.head_size;    (* long de la cabecera. *)
              rec_size  := ph_head.rec_size;     (* long del registro.   *)
           END; (* with : head *)
           ALLOCATE( fields, field_def_size * (field_count - 1) );
           FOR i := 0 TO field_count - 1 DO
               record_size := 0;
               WITH fields^[ i ] DO
                    FM.RdBlock( f, ph_field, ph_field_def_size );
                    name   := ph_field.name;             (* debe ser terminado en cero *)
                    type   := ph_field.type;             (* C, N, L, D, M *)
                    long   := CARDINAL( ph_field.long ); (* longitud del campo. *)
                    dec    := CARDINAL( ph_field.dec  ); (* campos decimales. *)
                    offset := record_size;               (* offset del campo en el reg *)
                    record_size := record_size + long;   (* longitud del reg. *)
               END; (* with : fields^[ i ] *)
           END; (* for *)
        END; (* if *)
    END; (* with : base *)
    RETURN base;
END Open;

PROCEDURE Close( VAR b :BaseType );
    VAR d, m, a :CARDINAL;
BEGIN
    DateTime.DateCard( d, m, a );
    b^.head.update[0] := SHORTCARD(a);
    b^.head.update[1] := SHORTCARD(m);
    b^.head.update[2] := SHORTCARD(d);
    WriteHead( b );
    Seek( b, b^.head.rec_count );
    FM.WrBlock( b^.file, eof_mark, SIZE(eof_mark) );
    FM.Close( b^.file );
    DEALLOCATE( b^.fields, b^.field_count * field_def_size );
    DEALLOCATE( b, SIZE( b^ ) );
END Close;

PROCEDURE Exists( name :ARRAY OF CHAR ) :BOOLEAN;
    VAR full :FM.PathStr;
        ok   :BOOLEAN;
BEGIN
    MakeName( '', name, full );
    ok := FM.Exists( full );
    RETURN ok;
END Exists;

PROCEDURE Lock( b :BaseType );
BEGIN
END Lock;

PROCEDURE UnLock( b :BaseType );
BEGIN
END UnLock;

PROCEDURE Sync( b :BaseType );
BEGIN
END Sync;

PROCEDURE Init() :BaseType;
BEGIN
    RETURN NIL;
END Init;

PROCEDURE BlankRec( VAR b :BaseType; VAR rec :ARRAY OF BYTE );
    VAR i, j   :CARDINAL;
        long   :CARDINAL;
        offset :CARDINAL;
        dec    :CARDINAL;
BEGIN
    FOR i := 0 TO b^.field_count - 1 DO
        offset := b^.fields^[ i ].offset;
        long   := b^.fields^[ i ].long;
        CASE b^.fields^[ i ].type OF
           'C' : FOR j := 0 TO long - 1 DO
                     rec[  offset + j ] := 32; (* espacio " "*)
                 END; (* for *) |
           'N' : FOR j := 0 TO long - 1 DO
                     rec[  offset + j ] := 32; (* espacio " " *)
                 END; (* for *)
                 dec := b^.fields^[ i ].dec;
                 IF dec # 0 THEN
                    rec[ offset + long - dec - 1 ] := 46; (* punto "." *)
                 END; (* if *) |
           'L' : rec[ offset ] := 63; (* pregunta "?" *) |
           'D' : FOR j := 0 TO long - 1 DO
                     rec[ offset + j ] := 32; (* espacio : " " *)
                 END; (* for *); |
           'M' : FOR j := 0 TO long - 1 DO
                     rec[ offset + j ] := 32; (* espacio : " " *)
                 END; (* for *)
        END; (* case *)
    END; (* for *)
END BlankRec;

PROCEDURE AddRec( VAR b :BaseType; VAR rec :ARRAY OF BYTE );
BEGIN
    Seek( b, b^.head.rec_count );
    Write( b, rec );
    INC( b^.head.rec_count );
END AddRec;

PROCEDURE PutRec( VAR b :BaseType; place :FM.IndexType; VAR rec :ARRAY OF BYTE );
BEGIN
    Seek( b, place );
    Write( b, rec );
END PutRec;

PROCEDURE GetRec( VAR b :BaseType; place :FM.IndexType; VAR rec :ARRAY OF BYTE );
BEGIN
    Seek( b, place );
    Read( b, rec );
END GetRec;

PROCEDURE DelRec( VAR b :BaseType; place :FM.IndexType );
BEGIN
    Seek( b, place );
    FM.WrBlock( b^.file, del_rec_char, SIZE(del_rec_char) );
END DelRec;

PROCEDURE UnDelRec( VAR b :BaseType; place :FM.IndexType );
BEGIN
    Seek( b, place );
    FM.WrBlock( b^.file, undel_rec_char, SIZE(undel_rec_char) );
END UnDelRec;

PROCEDURE Seek( VAR b :BaseType; place :FM.IndexType );
BEGIN
    WITH b^ DO
       FM.Seek( file, (FM.IndexType(head.head_size) +
                       FM.IndexType(head.rec_size ) * place) );
    END; (* with *)
END Seek;

PROCEDURE Read( VAR b :BaseType; VAR rec :ARRAY OF BYTE );
BEGIN
    FM.Seek( b^.file, FM.GetPos( b^.file ) + 1 );
    FM.RdBlock( b^.file, rec, b^.head.rec_size-1 );
END Read;

PROCEDURE Write( VAR b :BaseType; VAR rec :ARRAY OF BYTE );
BEGIN
    FM.WrBlock( b^.file, undel_rec_char, SIZE(undel_rec_char) );
    FM.WrBlock( b^.file, rec, b^.head.rec_size-1 );
END Write;

PROCEDURE BlankField( VAR b :BaseType; n :CARDINAL; VAR rec :ARRAY OF BYTE );
    VAR long   :CARDINAL;
        offset :CARDINAL;
        dec    :CARDINAL;
        j      :CARDINAL;
BEGIN
    offset := b^.fields^[ n ].offset;
    long   := b^.fields^[ n ].long;
    CASE b^.fields^[ n ].type OF
       'C' : FOR j := 0 TO long - 1 DO
                 rec[  offset + j ] := 32; (* espacio " "*)
             END; (* for *) |
       'N' : FOR j := 0 TO long - 1 DO
                 rec[  offset + j ] := 32; (* espacio " " *)
             END; (* for *)
             dec := b^.fields^[ n ].dec;
             IF dec # 0 THEN
                rec[ offset + long - dec ] := 46; (* punto "." *)
             END; (* if *) |
       'L' : rec[ offset ] := 63; (* pregunta "?" *) |
       'D' : FOR j := 0 TO long - 1 DO
                 rec[ offset + j ] := 32; (* espacio : " " *)
             END; (* for *); |
       'M' : FOR j := 0 TO long DO
                 rec[ offset + j ] := 32; (* espacio : " " *)
             END; (* for *) |
      ELSE OpResult := field_invalid_type;
    END; (* case *)
END BlankField;

PROCEDURE GetField( VAR b :BaseType; n :CARDINAL; VAR field, rec :ARRAY OF BYTE );
    VAR long   :CARDINAL;
        offset :CARDINAL;
        i      :CARDINAL;
BEGIN
    long   := b^.fields^[ n ].long;
    offset := b^.fields^[ n ].offset;
    FOR i := 0 TO long - 1 DO
        field[ i ] := rec[ offset + i ];
    END; (* for *)
END GetField;

PROCEDURE PutField( VAR b :BaseType; n :CARDINAL; VAR field, rec :ARRAY OF BYTE );
    VAR long   :CARDINAL;
        offset :CARDINAL;
        i      :CARDINAL;
BEGIN
    long   := b^.fields^[ n ].long;
    offset := b^.fields^[ n ].offset;
    FOR i := 0 TO long - 1 DO
        rec[ offset + i ] := field[ i ];
    END; (* for *)
END PutField;

(*
   Nota: todos los "fields" (campos) y los "rec" (registros) son pasados como
   "VAR" no solo por que se los va a modificar en algunos casos sino tambi‚n
   por que as¡ no se sobrecarga a la "pila" con demasiados datos.
*)
(*
   Funciones sobre la base de datos.
*)

PROCEDURE BaseFile( b :BaseType; VAR name :ARRAY OF CHAR );
BEGIN
    Str.Copy( name, b^.name );
END BaseFile;

PROCEDURE IndexFile( b :BaseType; VAR name :ARRAY OF CHAR );
BEGIN
END IndexFile;

PROCEDURE Version( b :BaseType; VAR ver :CARDINAL );
BEGIN
    ver := CARDINAL( b^.head.ver );
END Version;

PROCEDURE LastUpDate( b :BaseType; VAR d, m, a :CARDINAL );
BEGIN
    a := VAL( CARDINAL, b^.head.update[ 2 ] );
    m := VAL( CARDINAL, b^.head.update[ 1 ] );
    d := VAL( CARDINAL, b^.head.update[ 0 ] );
END LastUpDate;

PROCEDURE RecCount( b :BaseType ) :FM.IndexType;
    (* n£mero de registros actuales en la base de datos. *)
BEGIN
    RETURN b^.head.rec_count;
END RecCount;

PROCEDURE RecSize( b :BaseType ) :CARDINAL; (* tama¤o del registro *)
BEGIN
    RETURN b^.head.rec_size;
END RecSize;

PROCEDURE Field_count( b :BaseType ) :CARDINAL;
BEGIN
    RETURN b^.field_count;
END Field_count;

PROCEDURE Field_name( b :BaseType; n :CARDINAL; VAR name :ARRAY OF CHAR );
BEGIN
    Str.Copy( name, b^.fields^[ n ].name );
END Field_name;

PROCEDURE Field_type( b :BaseType; n :CARDINAL; VAR type :Types );
    VAR t :CHAR;
BEGIN
    t := b^.fields^[ n ].type;
    CASE t OF
       'C' : type := Character; |
       'N' : type := Numeric;   |
       'D' : type := Date;      |
       'L' : type := Logical;   |
       'M' : type := Memo;      |
     ELSE
       OpResult := field_invalid_type;
    END; (* case *)
END Field_type;

PROCEDURE Field_size( b :BaseType; n :CARDINAL; VAR size :CARDINAL );
BEGIN
    size := b^.fields^[ n ].long;
END Field_size;

PROCEDURE Field_decs( b :BaseType; n :CARDINAL; VAR decs :CARDINAL );
BEGIN
    decs := b^.fields^[ n ].dec;
END Field_decs;

PROCEDURE BOF( b :BaseType ) :BOOLEAN;    (* Si esta el archio al principio *)
BEGIN
    RETURN Pos( b ) = 0;
END BOF;

PROCEDURE EOF( b :BaseType ) :BOOLEAN;    (* Si esta el archivo en el final *)
BEGIN
    RETURN Pos( b ) > b^.head.rec_count;
END EOF;

PROCEDURE Pos( b :BaseType ) :FM.IndexType;   (* Posici¢n en la base de datos *)
BEGIN
    RETURN (FM.GetPos( b^.file ) - FM.IndexType( b^.head.head_size )) DIV
                                   FM.IndexType( b^.head.rec_size );
END Pos;

PROCEDURE Deleted( b :BaseType ) :BOOLEAN;
(*
   si el registro actual esta borrado.
*)
    VAR chdel :CHAR;
BEGIN
    FM.RdBlock( b^.file, chdel, 1 );
    RETURN chdel = del_rec_char;
END Deleted;

PROCEDURE DOresult() :CARDINAL; (* ver codigos de error. *)
    VAR op :CARDINAL;
BEGIN
    op := OpResult;
    OpResult := no_error;
    RETURN op;
END DOresult;

(* ------------------------------------------------------------------------ *)
(* Procedimientos de conversi¢n de datos de Modula-2 --> dBase III+         *)
(* ------------------------------------------------------------------------ *)

PROCEDURE CompleteStr( VAR str :ARRAY OF CHAR );
    VAR BlankLine :ARRAY [ 0..255 ] OF CHAR;
BEGIN
    String.FillStr( BlankLine, blank_char );
    Str.Append( str, BlankLine );
END CompleteStr;

PROCEDURE AssignStr( VAR str :ARRAY OF CHAR; str2 :ARRAY OF CHAR );
BEGIN
    Str.Copy( str, str2 );
    CompleteStr( str );
END AssignStr;

PROCEDURE AssignCard( VAR str :ARRAY OF CHAR; card :CARDINAL );
    VAR ok :BOOLEAN;
        aux :ARRAY [ 0..15 ] OF CHAR;
BEGIN
    Str.CardToStr( LONGCARD( card ), aux, 10, ok );
    IF ok THEN
       Str.Copy( str, aux ); (* Numeric. *)
     ELSE
       Str.Copy( str, '' );
    END; (* if *)
    CompleteStr( str );
END AssignCard;

PROCEDURE AssignLCard( VAR str :ARRAY OF CHAR; lngcard :LONGCARD );
    VAR ok  :BOOLEAN;
        aux :ARRAY [ 0..15 ] OF CHAR;
BEGIN
    Str.CardToStr( lngcard, aux, 10, ok );
    IF ok THEN
       Str.Copy( str, aux ); (* Numeric. *)
     ELSE
       Str.Copy( str, '' );
    END; (* if *)
    CompleteStr( str );
END AssignLCard;

PROCEDURE AssignDate( VAR str :ARRAY OF CHAR; date :DateTime.DateType );
    VAR s       :ARRAY [ 0..12 ] OF CHAR;
        d, m, a :CARDINAL;
        ok      :BOOLEAN;
BEGIN
    DateTime.DateUnCompress( date, d, m, a );
    a := a + 1900;
    Str.CardToStr( LONGCARD(a), s, 10, ok ); Str.Copy  ( str, s );
    Str.CardToStr( LONGCARD(m), s, 10, ok ); Str.Append( str, s );
    Str.CardToStr( LONGCARD(d), s, 10, ok ); Str.Append( str, s );
    CompleteStr( str );
END AssignDate;

END dBase.
