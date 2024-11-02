IMPLEMENTATION MODULE db_db3;

(*# define( multi => on ) *)

IMPORT DateTime, String, GDialog, Sound, Win;
IMPORT Str, Window, Lib, Storage;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

VAR error       :ErrorType;
    last_errors :CARDINAL;

PROCEDURE SetError( err :ErrorType );
BEGIN
    error := err;
    IF error # no_error THEN
       INC( last_errors );
     ELSE
       last_errors := 0;
    END; (* if *)
END SetError;

CLASS IMPLEMENTATION dB3File;

    PROCEDURE LockFile();
    BEGIN
        dBase.Lock( f_DAT );
    END LockFile;

    PROCEDURE UnLockFile();
    BEGIN
        dBase.UnLock( f_DAT );
    END UnLockFile;

    PROCEDURE SeekData( i :FM.IndexType );
    BEGIN
        dBase.Seek( f_DAT, i );
    END SeekData;

    PROCEDURE WrData( VAR d :ARRAY OF BYTE );
    BEGIN
        dBase.Write( f_DAT, d );
    END WrData;

    PROCEDURE RdData( VAR d :ARRAY OF BYTE ) :BOOLEAN;
    BEGIN
        dBase.Read( f_DAT, d );
        RETURN TRUE;
    END RdData;

    PROCEDURE GetData( i :FM.IndexType; VAR data :ARRAY OF BYTE ) :BOOLEAN;
        VAR ok :BOOLEAN;
    BEGIN
        ok := i < FileSize();
        IF ok THEN
           SeekData( i );
           ok := RdData( data );
        END; (* if *)
        RETURN ok;
    END GetData;

    PROCEDURE PutData( i :FM.IndexType; VAR data :ARRAY OF BYTE ) :BOOLEAN;
        VAR ok :BOOLEAN;
    BEGIN
        ok := i < FileSize();
        IF ok THEN
           SeekData( i ); WrData( data );
        END; (* if *)
        RETURN ok;
    END PutData;

    VIRTUAL PROCEDURE InitData( VAR d :ARRAY OF BYTE );
    END InitData;

    VIRTUAL PROCEDURE Comp( d1, d2 :FM.IndexType ) :BOOLEAN;
    BEGIN
        (* Implementada por los usuarios de esta clase. *)
        RETURN FALSE;
    END Comp;

    VIRTUAL PROCEDURE Compare( VAR d1, d2 :ARRAY OF BYTE ) :INTEGER;
    (*
       Utilizado por el algoritmo de Busqueda Binaria.
    *)
    BEGIN
        (* Implementada por los usuarios de esta clase. *)
        RETURN 0;
    END Compare;

    VIRTUAL PROCEDURE GetKeyStr( i :FM.IndexType; VAR s :ARRAY OF CHAR );
        (*
           Implementado por los usuarios de esta clase.
        *)
    END GetKeyStr;

    PROCEDURE SetPath( path :ARRAY OF CHAR ); (* Agregado el 25-03-94 *)
    BEGIN
        Str.Copy( Dir, path );
    END SetPath;

    PROCEDURE Open( name :ARRAY OF CHAR; fields :ARRAY OF dBase.FieldDef; new :BOOLEAN ) :BOOLEAN;
        VAR ok :BOOLEAN;
    BEGIN
        IF Exists( name ) THEN
           f_DAT := dBase.Open( DataName );
         ELSIF new THEN
           Create( name, fields );
           ok := TRUE;
         ELSE
           ok := FALSE;
        END; (* if *)
        RETURN ok;
    END Open;

    PROCEDURE Create( name :ARRAY OF CHAR; fields :ARRAY OF dBase.FieldDef );
    BEGIN
        f_DAT := dBase.Create( name, fields );
    END Create;

    PROCEDURE OpenExists( name :ARRAY OF CHAR ) :BOOLEAN;
    BEGIN
        IF ~ dBase.Exists( name ) THEN
           RETURN FALSE;
         ELSE
           f_DAT := dBase.Open( DataName  );
        END;
        RETURN TRUE;
    END OpenExists;

    PROCEDURE Exists( name :ARRAY OF CHAR ) :BOOLEAN;
        VAR exists :BOOLEAN;
    BEGIN
        exists := dBase.Exists( name );
        RETURN exists;
    END Exists;

    PROCEDURE Sync();
    BEGIN
        dBase.Sync( f_DAT );
    END Sync;

    PROCEDURE FileSize() :FM.IndexType;
        VAR max :FM.IndexType;
    BEGIN
        max := dBase.RecCount( f_DAT );
        RETURN max;
    END FileSize;

    PROCEDURE CloseFile();
    BEGIN
        dBase.Close( f_DAT );
    END CloseFile;

    PROCEDURE New( VAR d :ARRAY OF BYTE );
    BEGIN
        dBase.AddRec( f_DAT, d );
    END New;

    PROCEDURE Put( i :FM.IndexType; VAR d :ARRAY OF BYTE );
    BEGIN
        dBase.PutRec( f_DAT, i, d );
    END Put;

    PROCEDURE Get( i :FM.IndexType; VAR d :ARRAY OF BYTE ) :BOOLEAN;
        VAR ok :BOOLEAN;
    BEGIN
        dBase.GetRec( f_DAT, i, d );
        RETURN TRUE;
    END Get;

    PROCEDURE Del( i :FM.IndexType );
    BEGIN
        dBase.DelRec( f_DAT, i );
    END Del;

    PROCEDURE SearchFile(     search :ARRAY OF BYTE;
                          VAR index  :FM.IndexType;
                          VAR data   :ARRAY OF BYTE ) :BOOLEAN;

        PROCEDURE BSearch(     Low,
                               High   :FM.IndexType;
                           VAR Middle :FM.IndexType ) :BOOLEAN;
            VAR Resp  :INTEGER;
                First :FM.IndexType;
                Last  :FM.IndexType;
        BEGIN
            First := Low;
            Last  := High;
            Middle := (Low + High) DIV 2;
            Get( Middle, data );
            Resp := Compare( search, data );
            WHILE (Low <= High) & (Resp # 0) DO
                IF Resp < 0 THEN
                   IF First = Middle THEN RETURN FALSE; END;
                   High := Middle - 1;
                 ELSIF Resp > 0 THEN
                   IF Middle = Last THEN RETURN FALSE; END;
                   Low := Middle + 1;
                END; (* if *)
                Middle := (Low + High) DIV 2;
                Get( Middle, data );
                Resp := Compare( search, data );
            END; (* while *)
            RETURN Resp = 0;
        END BSearch;

        VAR pointer :FM.IndexType;
            ok      :BOOLEAN;

    BEGIN
        SortFile();
        IF FileSize() > 0 THEN
           ok := BSearch( 0, FileSize() - 1, index );
           IF ok THEN
 (*           SeekIndex( index ); RdIndex( pointer );
              index := pointer;*)
            ELSE
              index := MAX( FM.IndexType );
           END; (* if *)
         ELSE
           ok := FALSE;
        END; (* if *)
        RETURN ok;
    END SearchFile;

    PROCEDURE SortFile() :BOOLEAN;
    BEGIN
        RETURN FALSE;
    END SortFile;

    PROCEDURE Rebuild();
    BEGIN
    END Rebuild;

    PROCEDURE UnSortFile() :BOOLEAN;
        VAR abort :BOOLEAN;
    BEGIN
        abort := GDialog.Wait_yes( '­Inicializaci¢n del archivo! ¨Aborto?' );
        IF ~ abort THEN Rebuild(); END; (* if *)
        RETURN ~ abort;
    END UnSortFile;

    PROCEDURE Copy( VAR _file :dB3File; name :ARRAY OF CHAR );
        VAR size  :FM.IndexType;
            i     :FM.IndexType;
            pdata :FM.ptrDT;
            w     :Win.window;
            tit   :ARRAY [ 0..80 ] OF CHAR;
    BEGIN
        tit := '¯ "';
        Str.Append( tit, name );
        Str.Append( tit, '" existe, ¨ Sigo ? ®' );
        IF ~ Exists( name ) OR
           GDialog.Wait_yes( tit )
         THEN
           w.Init( 15, 9, 45, 2, Win.Inverse, TRUE,
                     '', Win.Center, Win.Inverse,
                     Win.SingleDoubleFrame, Win.Inverse );
           Window.GotoXY( 3, 1 ); Win.WrStr( 'Copiando archivo : ' );
           Win.WrStr( name );
           NEW( pdata );
           size := FileSize();
           IF size > 0 THEN
              DEC( size );
              (* _file.Create( name, FileHead.recSize ); *)
              FOR i := 0 TO size DO
                  Get( i, pdata^ );
                  _file.New( pdata^ );
                  Window.GotoXY( 31, 1 ); Win.WrLngCard( i+1, 6 );
              END; (* next : i *)
              _file.CloseFile();
            ELSE
              GDialog.Wait_key( '¯ archivo vacio, listo ®' );
           END; (* if *)
           DISPOSE( pdata );
           w.Close();
        END; (* if *)
    END Copy;


BEGIN
    f_DAT     := dBase.Init();
    Dir       := '';
    DataName  := '';
END dB3File;

PROCEDURE Erase( name :ARRAY OF CHAR );
    VAR name_dat :FM.PathStr;
BEGIN
    dBase.MakeName( '', name, name_dat );
    IF FM.Exists( name_dat ) THEN FM.Erase( name_dat ); END; (* if *)
END Erase;

PROCEDURE Rename( old_name, new_name :ARRAY OF CHAR );
    VAR n_name_dat :FM.PathStr;
        o_name_dat :FM.PathStr;
BEGIN
    dBase.MakeName( '', old_name, o_name_dat );
    dBase.MakeName( '', new_name, n_name_dat );
    FM.Rename( o_name_dat, n_name_dat );
END Rename;

PROCEDURE Rebuild( name :ARRAY OF CHAR );
BEGIN
END Rebuild;

PROCEDURE Error() :ErrorType;
    VAR ret :ErrorType;
BEGIN
    ret := error;
    SetError( no_error );
    RETURN ret;
END Error;

BEGIN
    SetError( no_error );
END db_db3.
