IMPLEMENTATION MODULE Clarion;

IMPORT FM;

CONST FieldNameSize = 16;
      PreSize       =  3;

TYPE RecNameType  = ARRAY [ 1..FieldNameSize-4 ] OF CHAR;
     MemoNameType = ARRAY [ 1..FieldNameSize-4 ] OF CHAR;
     PrefixType   = ARRAY [ 1..PreSize         ] OF CHAR;

     FileHead = RECORD
        FileSig   :CARDINAL;     (* signature                    *)
        FileAttr  :CARDINAL;     (* file attributes              *)
        NumKeys   :SHORTCARD;    (* number of keys in this file  *)
        NumRecs   :LONGCARD;     (* number of records in file    *)
        NumDels   :LONGCARD;     (* number of deleted records    *)
        NumFields :CARDINAL;     (* number of fields             *)
        NumPics   :CARDINAL;     (* number of pictures           *)
        NumArrays :CARDINAL;     (* number of arrays             *)
        RecLen    :CARDINAL;     (* record length                *)
        OffSet    :LONGCARD;     (* start of data area           *)
        LogEOF    :LONGCARD;     (* logical end of file          *)
        LogBOF    :LONGCARD;     (* logical beginning of file    *)
        FreeRec   :LONGCARD;     (* first usable deleted record  *)
        RecName   :RecNameType;  (* record name without pre      *)
        MemoName  :MemoNameType; (* memo name without pre        *)
        FilePre   :PrefixType;   (* file name prefix             *)
        RecPre    :PrefixType;   (* record name prefix           *)
        MemoLen   :CARDINAL;     (* size of memo                 *)
        MemoWidht :CARDINAL;     (* column length of memo        *)
        LockCount :LONGCARD;     (* file locked counter          *)
        ChgTime   :LONGCARD;     (* time of last change          *)
        ChgDate   :LONGCARD;     (* date of last change          *)
        Checksum  :CARDINAL;     (* checksum used for encrypt    *)
     END; (* FILEHEAD *)


CLASS IMPLEMENTATION ClarionFile;

    PROCEDURE Init( name    :ARRAY OF CHAR;
                    recdef  :ARRAY OF FieldDef;
                    keydef  :ARRAY OF KeyDef;
                    create  :BOOLEAN;
                    reclaim :BOOLEAN );
    BEGIN
    END Init;

    PROCEDURE SetField( field :FieldID; val :ARRAY OF BYTE );
    BEGIN
    END SetField;

    PROCEDURE GetField( field :FieldID; val :ARRAY OF BYTE );
    BEGIN
    END GetField;

    PROCEDURE ClrField( field :FieldID; mod :INTEGER );
    BEGIN
    END ClrField;

    PROCEDURE ClrRecord( mode :INTEGER );
    BEGIN
    END ClrRecord;

    PROCEDURE Clear();
    BEGIN
    END Clear;

    PROCEDURE Create();
    BEGIN
    END Create;

    PROCEDURE Open( access_mode :AccessMode );
    BEGIN
    END Open;

    PROCEDURE Close();
    BEGIN
    END Close;

    PROCEDURE Copy( new_file :ClarionFile );
    BEGIN
    END Copy;

    PROCEDURE Empty();
    BEGIN
    END Empty;

    PROCEDURE Flush();
    BEGIN
    END Flush;

    PROCEDURE Lock();
    BEGIN
    END Lock;

    PROCEDURE Pack();
    BEGIN
    END Pack;

    PROCEDURE Remove();
    BEGIN
    END Remove;

    PROCEDURE Rename();
    BEGIN
    END Rename;

    PROCEDURE Share( access_mode :AccessMode );
    BEGIN
    END Share;

    PROCEDURE Stream();
    BEGIN
    END Stream;

    PROCEDURE UnLock();
    BEGIN
    END UnLock;

    PROCEDURE Add();
    BEGIN
    END Add;

    PROCEDURE Append();
    BEGIN
    END Append;

    PROCEDURE Delete();
    BEGIN
    END Delete;

    PROCEDURE Get();
    BEGIN
    END Get;

    PROCEDURE Hold();
    BEGIN
    END Hold;

    PROCEDURE Next();
    BEGIN
    END Next;

    PROCEDURE Previous();
    BEGIN
    END Previous;

    PROCEDURE Put();
    BEGIN
    END Put;

    PROCEDURE Release();
    BEGIN
    END Release;

    PROCEDURE Reset();
    BEGIN
    END Reset;

    PROCEDURE Set();
    BEGIN
    END Set;

    PROCEDURE Skip();
    BEGIN
    END Skip;

    PROCEDURE BOF() :BOOLEAN;
    BEGIN
        RETURN FALSE;
    END BOF;

    PROCEDURE Bytes() :LONGCARD;
    BEGIN
        RETURN 0;
    END Bytes;

    PROCEDURE Duplicate() :BOOLEAN;
    BEGIN
        RETURN FALSE;
    END Duplicate;

    PROCEDURE EOF() :BOOLEAN;
    BEGIN
        RETURN FALSE;
    END EOF;

    PROCEDURE Pointer() :LONGCARD;
    BEGIN
        RETURN 0;
    END Pointer;

    PROCEDURE Position() :LONGCARD;
    BEGIN
        RETURN 0;
    END Position;

    PROCEDURE Records()   :LONGCARD;
    BEGIN
        RETURN 0;
    END Records;

BEGIN
END ClarionFile;

END Clarion.
