IMPLEMENTATION MODULE db_btrv;

IMPORT Str, GDialog, Lib, IO;

PROCEDURE MakeName(     dir      :ARRAY OF CHAR;
                        name     :ARRAY OF CHAR;
                    VAR dataName :ARRAY OF CHAR );
BEGIN
    FM.MakeName( dir, name, FM.DB_BTRV_FEXT, dataName );
END MakeName;

CLASS IMPLEMENTATION BtrvFile;

    PROCEDURE LockFile();       (* Begin Transaction *)
    BEGIN
        BTRV.BeginTransaction();
    END LockFile;

    PROCEDURE UnLockFile();     (* End   Transaction *)
    BEGIN
        BTRV.EndTransaction();
    END UnLockFile;

    PROCEDURE SetPath( path :ARRAY OF CHAR );
    BEGIN
        Str.Copy( Dir, path );
    END SetPath;

    PROCEDURE SetIDX( index :BTRV.KeyRange );
    BEGIN
        BTRV.SetIDX( f_DAT, index );
    END SetIDX;

    PROCEDURE Open( name :ARRAY OF CHAR; spec :BTRV.FileSpec; new :BOOLEAN ) :BOOLEAN;
        VAR ok     :BOOLEAN;
    BEGIN
        ok := TRUE;
        MakeName( Dir, name, DataName );
        IF ~ Exists( name ) THEN
           Create( name, spec );
        END; (* if *)
        f_DAT := BTRV.BOpen( DataName, BTRV.Normal );
        RETURN ok;
    END Open;

    PROCEDURE Create( name :ARRAY OF CHAR; spec :BTRV.FileSpec );
    BEGIN
        MakeName( Dir, name, DataName );
        f_DAT := BTRV.BCreate( DataName, spec );
    END Create;

    PROCEDURE OpenExists( name :ARRAY OF CHAR ) :BOOLEAN;
    BEGIN
        MakeName( Dir, name, DataName );
        IF ~ Exists( name ) THEN
           RETURN FALSE;
         ELSE
           f_DAT := BTRV.BOpen( DataName, BTRV.Normal );
           RETURN TRUE;
        END;
    END OpenExists;

    PROCEDURE Exists( name :ARRAY OF CHAR ) :BOOLEAN;
        VAR _data_file :BTRV.FileName;
            exists     :BOOLEAN;
    BEGIN
        MakeName( Dir, name, _data_file );
        exists := FM.Exists( _data_file );
        RETURN exists;
    END Exists;

    PROCEDURE FileSize() :FM.IndexType;
    BEGIN
        RETURN BTRV.BRecords( f_DAT );
    END FileSize;

    PROCEDURE CloseFile();
    BEGIN
        BTRV.BClose( f_DAT );
    END CloseFile;

    PROCEDURE New( VAR d :ARRAY OF BYTE ) :FM.IndexType;
    BEGIN
        RETURN BTRV.BAdd( f_DAT, d );
    END New;

    PROCEDURE Get( i :FM.IndexType; VAR d :ARRAY OF BYTE );
    BEGIN
        BTRV.BGet( f_DAT, i, d );
    END Get;

    PROCEDURE Put( i :FM.IndexType; VAR d :ARRAY OF BYTE );
    BEGIN
        BTRV.BPut( f_DAT, i, d );
    END Put;

    PROCEDURE Del( i :FM.IndexType );
    BEGIN
        BTRV.BDel( f_DAT, i );
    END Del;

    PROCEDURE SearchFile(     search :ARRAY OF BYTE;
                          VAR pos    :FM.IndexType;
                          VAR data   :ARRAY OF BYTE ) :BOOLEAN;
    BEGIN
        RETURN BTRV.BSrch( f_DAT, search, pos, data );
    END SearchFile;

    PROCEDURE GetFirst( VAR pos :FM.IndexType; VAR d :ARRAY OF BYTE );
    BEGIN
        BTRV.BGetFirst( f_DAT, pos, d );
    END GetFirst;

    PROCEDURE GetNext( VAR pos :FM.IndexType; VAR d :ARRAY OF BYTE );
    BEGIN
        BTRV.BGetNext( f_DAT, pos, d );
    END GetNext;

    PROCEDURE SUpdate();
    BEGIN
        BTRV.SUpdate( f_DAT );
    END SUpdate;

    PROCEDURE GetSpec() :BTRV.FileSpec;
    BEGIN
        RETURN BTRV.GetSpec( f_DAT );
    END GetSpec;

    PROCEDURE Copy( VAR _file :BtrvFile; name :ARRAY OF CHAR );
    END Copy;

BEGIN

END BtrvFile;

PROCEDURE Erase( name :ARRAY OF CHAR );
    VAR name_dat :FM.PathStr;
BEGIN
    MakeName( '', name, name_dat );
    IF FM.Exists( name_dat ) THEN FM.Erase( name_dat ); END; (* if *)
END Erase;

PROCEDURE Rename( old_name, new_name :ARRAY OF CHAR );
    VAR n_name_dat :FM.PathStr;
        o_name_dat :FM.PathStr;
BEGIN
    MakeName( '', old_name, o_name_dat );
    MakeName( '', new_name, n_name_dat );
    FM.Rename( o_name_dat, n_name_dat );
END Rename;


PROCEDURE Error() :BTRV.ErrorType;
(*
   Toma los errores de los archivos de "BTRV".
*)
BEGIN
    RETURN BTRV.GetError();
END Error;

END db_btrv.
