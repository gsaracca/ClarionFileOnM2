IMPLEMENTATION MODULE db_cla;

IMPORT FM, DateTime, String;
IMPORT IO, FIO, FIOR, Lib, Str;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

(* ------------------------------------------------------------------------ *)
(* Constantes.                                                              *)
(* ------------------------------------------------------------------------ *)

CONST FileSignature = 03343H;   (* file signature for data file        *)
      MemoSignature = 0334DH;   (* file signature for memo file        *)
      FieldNameSize =  16;      (* max size of a field name            *)
      PreSize       =   3;      (* max size field name prefix          *)
      MaxFields     = 256;      (* max number of fields per record     *)
      DefEXT        = 'DAT';    (* default file extension for sfiles   *)
      KeyEXT        = 'K00';    (* default file extension for indices  *)
      IndexEXT      = 'I00';    (* default file extension for indices  *)
      TempEXT       = 'T00';    (* default file extension for temps    *)
      MemoEXT       = 'MEM';    (* default file extension for temps    *)
      MemTMP        = 'M00';    (* default file extension for temps    *)
      LogEXT        = 'LOG';    (* default file extension for logout   *)
      BufMax        =  20;      (* maximum number of requested buffers *)
      NodeSize      = 512;      (* size of nodes in key files          *)
      MemoSize      = 256;      (* size of memo text block             *)
      MaxKeySize    = 245;      (* maximum length of key               *)
      MaxKeys       = 255;      (* maximum number of keys              *)
      MaxPicLen     = 256;      (* maximum length of picture           *)
      DtoDEF        = 0FFFFH;   (* default dto is -1                   *)

CONST HeadOffSet  = 5;          (* 5 : bytes + OffSet to Header        *)
      FieldOffSet = 1;          (* 1 : bytes + OffSet to Fields        *)

(* key type status *)
CONST KeyIndex       = 01H; (* Set the key like index   : FALSE *)
      KeyAllowDup    = 10H; (* Allow duplicates records : FALSE *)
      KeyNoCaseSense = 20H; (* No case sensitive        : FALSE *)
      KeyAllowNulls  = 40H; (* Allow null records       : FALSE *)

(* record status types *)
CONST RecordNew       = 001H;   (* New Record    *)
      RecordOld       = 002H;   (* Old Record    *)
      RecordRevised   = 004H;   (* Revised       *)
      RecordBacked    = 008H;   (* Backed Up     *)
      RecordDeleted   = 010H;   (* Deleted       *)
      RecordEmpty     = 020H;   (* Empty         *)
      RecordReadLock  = 040H;   (* Record Locked *)
      RecordWriteLock = 080H;   (* ???           *)

(* sf file status defines for sfatr *)
CONST FileLock     = 001H;      (* file is locked          *)
      FileOwner    = 002H;      (* file is owned           *)
      FileEncrypt  = 004H;      (* records are encrypted   *)
      FileMemo     = 008H;      (* memo file exists        *)
      FileCompress = 010H;      (* file is compressed      *)
      FileReclaim  = 020H;      (* reclaim deleted records *)
      FileReadOnly = 040H;      (* file is read only       *)
      FileCreate   = 080H;      (* file may be created     *)

TYPE ErrorType = CARDINAL;

CONST NoError      = 000H;      (* No Error Found                *)
      EXTerror     = 001H;      (* Entension Error, EXT # 'DAT'  *)
      IOerror      = 002H;      (* Error in I/O                  *)
      EOFerror     = 003H;      (* Read before EOF               *)
      FILEerror    = 004H;      (* File isn't a Clarion's File   *)
      RECSIZEerror = 005H;      (* Record Size Error             *)
      NotFound     = 006H;      (* File Not Found                *)

TYPE RecNameType   = ARRAY [ 1..FieldNameSize-4 ] OF CHAR;
     MemoNameType  = ARRAY [ 1..FieldNameSize-4 ] OF CHAR;
     PrefixType    = ARRAY [ 1..PreSize         ] OF CHAR;
     FieldNameType = ARRAY [ 1..FieldNameSize   ] OF CHAR;

TYPE FileHead = RECORD
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

CONST LongType    ::= field_long_type;      LongSize    = 4;
      RealType    ::= field_real_type;      RealSize    = 8;
      StringType  ::= field_string_type;    StringSize  = 0;
      PicType     ::= field_pic_type;       PicSize     = 0;
      ByteType    ::= field_byte_type;      ByteSize    = 1;
      ShortType   ::= field_short_type;     ShortSize   = 2;
      GroupType   ::= field_group_type;     GroupSize   = 0;
      DecimalType ::= field_decimal_type;   DecimalSize = 0;

TYPE FieldHead = RECORD
        FieldType   :SHORTCARD;     (* type of field             *)
        FieldName   :FieldNameType; (* name of field             *)
        FOffSet     :CARDINAL;      (* offset into record        *)
        Length      :CARDINAL;      (* length of field           *)
        DecSignif   :SHORTCARD;     (* significance for decimals *)
        DecDec      :SHORTCARD;     (* number of decimal places  *)
        ArrayNumber :CARDINAL;      (* array number              *)
        PictureNum  :CARDINAL;      (* picture number            *)
     END; (* FieldHead *)

TYPE KeyHead = RECORD
        NumComponent :SHORTCARD;     (* number of components for key *)
        KeyName      :FieldNameType; (* name of this key             *)
        Type         :SHORTCARD;     (* type of composite            *)
        Length       :SHORTCARD;     (* length of composite          *)
     END; (* KeyHead *)

TYPE KeyComponent = RECORD
        FieldType   :SHORTCARD;      (* field type                    *)
        FieldNumber :CARDINAL;       (* field number                  *)
        RecOffSet   :CARDINAL;       (* record offset of this element *)
        Length      :SHORTCARD;      (* length of element             *)
    END; (* KeyComponent *)

TYPE PictureType = RECORD
        Length :CARDINAL;
        PicStr :ARRAY [ 1..MaxPicLen ] OF CHAR;
     END; (* PictureType *)

TYPE ArrayType = RECORD
        NumDIM   :CARDINAL;
        TotalDIM :CARDINAL;
        ElemSize :CARDINAL;
     END; (* ArrayType *)

TYPE ArrayComponent = RECORD
        MaxDIM :CARDINAL;
        Length :CARDINAL;
     END; (* ArrayComponent *)

TYPE RecHead = RECORD
         RecHead    :SHORTCARD;
         RecPointer :LONGCARD;
     END; (* RecHead *)

PROCEDURE CHK( var, const :SHORTCARD ) :BOOLEAN;
BEGIN
    RETURN (var & const) = const;
END CHK;

PROCEDURE WrDouble();
BEGIN
    IO.WrCharRep( 'Í', 60 ); IO.WrLn();
END WrDouble;

PROCEDURE WrSingle();
BEGIN
    IO.WrCharRep( 'Ä', 60 ); IO.WrLn();
END WrSingle;

PROCEDURE WrError( error :ErrorType; file :ARRAY OF CHAR );
    VAR msg   :ARRAY [ 0..80 ] OF CHAR;
        break :BOOLEAN;
BEGIN
    break := FALSE;
    CASE error OF
       NoError      : msg := 'No Error Found';     |
       EXTerror     : msg := 'Extension Error';    |
       IOerror      : msg := 'I/O Error';         break := TRUE; |
       EOFerror     : msg := 'EOF Error';         break := TRUE; |
       FILEerror    : msg := 'Unknow file type';   |
       RECSIZEerror : msg := 'Record Size Error'; break := TRUE; |
       NotFound     : msg := 'File Not Found';     |
       ELSE           msg := 'Unknow Error';
    END; (* case *)
    IO.WrLn();
    IO.WrStr( msg ); IO.WrStr( ' ' ); IO.WrStr( file ); IO.WrStr( '.' );
    IO.WrLn();
    WrDouble();
    IF break THEN
       HALT;
    END; (* if *)
END WrError;

PROCEDURE WrHelp();
BEGIN
    IO.WrLn();
    IO.WrStr( 'Usage: CLAVIEW <file_name> | <*.dat>.' ); IO.WrLn();
END WrHelp;

PROCEDURE ViewHead( file_name :ARRAY OF CHAR);

    PROCEDURE WrHead( f :FIO.File );
        VAR head       :FileHead;
            field_def  :FieldHead;
            key_def    :KeyHead;
            key_comp   :KeyComponent;
            pic_def    :PictureType;
            array_def  :ArrayType;
            array_comp :ArrayComponent;
            rec_head   :RecHead;
            record     :POINTER TO ARRAY [ 0..65000 ] OF BYTE;
            i, j       :CARDINAL;
            li         :LONGCARD;
            owner      :BOOLEAN;
            encrypt    :BOOLEAN;
            compress   :BOOLEAN;

        PROCEDURE ReadFile( f :FIO.File; VAR buff :ARRAY OF BYTE; size :CARDINAL );
            VAR b :CARDINAL;
        BEGIN
            b := FIO.RdBin( f, buff, size );
            IF (FIO.IOresult() # 0) THEN
               WrError( IOerror, file_name );
             ELSIF FIO.EOF THEN
               WrError( EOFerror, file_name );
            END; (* if *)
        END ReadFile;

        PROCEDURE WrName( fieldtype :SHORTCARD );
        BEGIN
            IO.WrStr( ' ===> ' );
            CASE fieldtype OF
               1 : IO.WrStr( 'LONG'    ); |
               2 : IO.WrStr( 'REAL'    ); |
               3 : IO.WrStr( 'STRING'  ); |
               4 : IO.WrStr( 'PICTURE' ); |
               5 : IO.WrStr( 'BYTE'    ); |
               6 : IO.WrStr( 'SHORT'   ); |
               7 : IO.WrStr( 'GROUP'   ); |
               8 : IO.WrStr( 'DECIMAL' ); |
            END; (* case *)
        END WrName;

        PROCEDURE WrSep();
        BEGIN
            IO.WrStr(' | ');
        END WrSep;

        PROCEDURE WrFileAttr( file_attr :CARDINAL );
        BEGIN
            IO.WrStr( ' ==> ' );
            IF CHK( SHORTCARD(file_attr), FileLock  ) THEN
               IO.WrStr( 'Lock' );  WrSep();
            END; (* if *)
            IF CHK( SHORTCARD(file_attr), FileOwner ) THEN
               IO.WrStr( 'Owner' ); WrSep();
               owner := TRUE;
             ELSE
               owner := FALSE;
            END; (* if *)
            IF CHK( SHORTCARD(file_attr), FileEncrypt ) THEN
               IO.WrStr( 'Encrypt' ); WrSep();
               encrypt := TRUE;
             ELSE
               encrypt := FALSE;
            END; (* if *)
            IF CHK( SHORTCARD(file_attr), FileMemo ) THEN
               IO.WrStr( 'Memo' ); WrSep();
            END; (* if *)
            IF CHK( SHORTCARD(file_attr), FileCompress ) THEN
               IO.WrStr( 'Compress' ); WrSep();
               compress := TRUE;
             ELSE
               compress := FALSE;
            END; (* if *)
            IF CHK( SHORTCARD(file_attr), FileReclaim ) THEN
               IO.WrStr( 'Reclaim' ); WrSep();
            END; (* if *)
            IF CHK( SHORTCARD(file_attr), FileReadOnly ) THEN
               IO.WrStr( 'Read Only' ); WrSep();
            END; (* if *)
            IF CHK( SHORTCARD(file_attr), FileCreate ) THEN
               IO.WrStr( 'Create' ); WrSep();
            END; (* if *)
        END WrFileAttr;

        PROCEDURE WrRecHead( rec_head :SHORTCARD );
        BEGIN
            IO.WrStr( ' ==> ' );
            IF CHK( rec_head, RecordNew      ) THEN IO.WrStr( 'New'     ); WrSep(); END;
            IF CHK( rec_head, RecordOld      ) THEN IO.WrStr( 'Old'     ); WrSep(); END;
            IF CHK( rec_head, RecordRevised  ) THEN IO.WrStr( 'Revised' ); WrSep(); END;
            IF CHK( rec_head, RecordBacked   ) THEN IO.WrStr( 'Backed'  ); WrSep(); END;
            IF CHK( rec_head, RecordDeleted  ) THEN IO.WrStr( 'Deleted' ); WrSep(); END;
            IF CHK( rec_head, RecordEmpty    ) THEN IO.WrStr( 'Empty'   ); WrSep(); END;
            IF CHK( rec_head, RecordReadLock ) THEN IO.WrStr( 'RD Lock' ); WrSep(); END;
            IF CHK( rec_head, RecordWriteLock) THEN IO.WrStr( 'WR Lock' ); WrSep(); END;
        END WrRecHead;

        PROCEDURE WrHexa( val :CARDINAL );
        BEGIN
            IO.WrCard( val, 10 );
            IO.WrStr( ' ==> ' );
            IO.WrHex( val, 4 );
            IO.WrStr( 'H' );
        END WrHexa;

        PROCEDURE WrNL();
        BEGIN
            IO.WrStr( '.' );
            IO.WrLn();
        END WrNL;

        PROCEDURE WrStr( val :ARRAY OF CHAR );
        BEGIN
            IO.WrStr( '"' );
            IO.WrStr( val );
            IO.WrStr( '"' );
        END WrStr;

        PROCEDURE WrMsg( msg :ARRAY OF CHAR );
            VAR old_char :CHAR;
        BEGIN
            old_char := IO.SuffixChar;
            IO.SuffixChar := '.';
            IO.WrStrAdj( msg, -14 );
            IO.WrStr( ':' );
            IO.SuffixChar := old_char;
        END WrMsg;

        PROCEDURE WrKeyType( key_type :SHORTCARD );
        BEGIN
            IO.WrStr( ' ==> ' );
            IF CHK( key_type, KeyIndex ) THEN
               IO.WrStr( 'Index' );
             ELSE
               IO.WrStr( 'Key'   );
            END; (* if *)
            WrSep();
            IF CHK( key_type, KeyAllowDup ) THEN
               IO.WrStr( 'Duplicates' );
             ELSE
               IO.WrStr( 'Unique' );
            END; (* if *)
            WrSep();
            IF CHK( key_type, KeyNoCaseSense ) THEN
               IO.WrStr( 'No Case Sense' );
             ELSE
               IO.WrStr( 'Case Sense' );
            END; (* if *)
            WrSep();
            IF CHK( key_type, KeyAllowNulls ) THEN
               IO.WrStr( 'Allow Nulls' );
             ELSE
               IO.WrStr( 'Exclude Nulls' );
            END; (* if *)
            WrSep();
        END WrKeyType;


    BEGIN
        WrDouble();
        IO.WrStr( ' View File: ' ); IO.WrStr( file_name ); WrNL();
        WrDouble();
        IO.WrLn();
        ReadFile( f, head, SIZE(head) );
        head.OffSet := head.OffSet + HeadOffSet;
        WrMsg( 'Signature' ); WrHexa( head.FileSig );
        IO.WrStr( ' => ' );
        CASE head.FileSig OF
           FileSignature : IO.WrStr( 'Clarion file' );         |
           MemoSignature : IO.WrStr( 'Clarion memo file.' ); |
           ELSE            WrError( FILEerror, file_name );
                           RETURN;
        END; (* if *)
        WrNL();
        WrMsg( 'Attributes'    ); IO.WrCard   ( head.FileAttr,  10 );
        WrFileAttr( head.FileAttr );
        WrNL();
        WrMsg( "Key's #"       ); IO.WrShtCard( head.NumKeys,   10 ); WrNL();
        WrMsg( "Record's #"    ); IO.WrLngCard( head.NumRecs,   10 ); WrNL();
        WrMsg( "Deleted's #"   ); IO.WrLngCard( head.NumDels,   10 ); WrNL();
        WrMsg( "Field's #"     ); IO.WrCard   ( head.NumFields, 10 ); WrNL();
        WrMsg( "Picture's #"   ); IO.WrCard   ( head.NumPics,   10 ); WrNL();
        WrMsg( "Array's #"     ); IO.WrCard   ( head.NumArrays, 10 ); WrNL();
        WrMsg( 'Record Size'   ); IO.WrCard   ( head.RecLen,    10 ); WrNL();
        WrMsg( 'OffSet Data'   ); IO.WrLngCard( head.OffSet,    10 ); WrNL();
        WrMsg( 'Logical EOF'   ); IO.WrLngCard( head.LogEOF,    10 ); WrNL();
        WrMsg( 'Logical BOF'   ); IO.WrLngCard( head.LogBOF,    10 ); WrNL();
        WrMsg( 'First Free'    ); IO.WrLngCard( head.FreeRec,   10 ); WrNL();
        WrMsg( 'Record Name'   ); WrStr       ( head.RecName       ); WrNL();
        WrMsg( 'Memo Name'     ); WrStr       ( head.MemoName      ); WrNL();
        WrMsg( 'File Prefix'   ); WrStr       ( head.FilePre       ); WrNL();
        WrMsg( 'Record Prefix' ); WrStr       ( head.RecPre        ); WrNL();
        WrMsg( 'Memo Length'   ); IO.WrCard   ( head.MemoLen,   10 ); WrNL();
        WrMsg( 'Memo Width'    ); IO.WrCard   ( head.MemoWidht, 10 ); WrNL();
        WrMsg( 'Lock Counter'  ); IO.WrLngCard( head.LockCount, 10 ); WrNL();
        WrMsg( 'Change Time'   ); IO.WrLngCard( head.ChgTime,   10 ); WrNL();
        WrMsg( 'Change Date'   ); IO.WrLngCard( head.ChgDate,   10 ); WrNL();
        WrMsg( 'Checksum'      ); IO.WrCard   ( head.Checksum,  10 ); WrNL();
        WrDouble();
        WrMsg( 'Head Size' ); IO.WrCard( SIZE(head), 6 ); IO.WrLn();
        IF owner OR encrypt OR compress THEN
           IO.WrStr( ' File Not Supported.' ); IO.WrLn();
           WrDouble();
           RETURN;
        END; (* if *)
        WrDouble();
        FOR i := 0 TO head.NumFields-1 DO
            ReadFile( f, field_def, SIZE(field_def) );
            field_def.FOffSet := field_def.FOffSet + FieldOffSet;
            WITH field_def DO
            IF i # 0 THEN WrSingle(); END; (* if *)
            WrMsg( 'Type'       ); IO.WrShtCard( FieldType   ,10 ); WrName( FieldType );                                           IO.WrLn(); (* TYPE OF field             *)
            WrMsg( 'Name'       ); WrStr       ( FieldName       ); IO.WrLn();
            WrMsg( 'OffSet'     ); IO.WrCard   ( FOffSet     ,10 ); IO.WrLn();
            WrMsg( 'Length'     ); IO.WrCard   ( Length      ,10 ); IO.WrLn();
            WrMsg( 'Signific'   ); IO.WrShtCard( DecSignif   ,10 ); IO.WrLn();
            WrMsg( 'Dec Places' ); IO.WrShtCard( DecDec      ,10 ); IO.WrLn();
            WrMsg( 'Array #'    ); IO.WrCard   ( ArrayNumber ,10 ); IO.WrLn();
            WrMsg( 'Picture #'  ); IO.WrCard   ( PictureNum  ,10 ); IO.WrLn();
            END; (* with *)
        END; (* next *)
        WrDouble();
        IF head.NumKeys > 0 THEN
           FOR i := 0 TO CARDINAL(head.NumKeys)-1 DO
               ReadFile( f, key_def, SIZE(key_def) );
               WITH key_def DO
               IF i # 0 THEN WrSingle(); END; (* if *)
               WrMsg( "Key's Componenet" ); IO.WrShtCard( NumComponent, 10 ); IO.WrLn();
               WrMsg( "Key Name"         ); WrStr       ( KeyName          ); IO.WrLn();
               WrMsg( "Key Type"         ); IO.WrShtCard( Type,         10 );
               WrKeyType( Type ); IO.WrLn();
               WrMsg( "Length"           ); IO.WrShtCard( Length,       10 ); IO.WrLn();
               END; (* with *)
               FOR j := 0 TO CARDINAL(key_def.NumComponent)-1 DO
                   ReadFile( f, key_comp, SIZE(key_comp) );
                   WrSingle();
                   WITH key_comp DO
                   WrMsg( 'Field Type'    ); IO.WrShtCard( FieldType,   10 ); WrName( FieldType ); IO.WrLn();
                   WrMsg( 'Field #'       ); IO.WrCard   ( FieldNumber, 10 ); IO.WrLn();
                   WrMsg( 'Record OffSet' ); IO.WrCard   ( RecOffSet,   10 ); IO.WrLn();
                   WrMsg( 'Length'        ); IO.WrShtCard( Length,      10 ); IO.WrLn();
                   END; (* with *)
               END; (* next *)
           END; (* next *)
         ELSE
           IO.WrStr( " File without key's." ); IO.WrLn();
        END; (* if *)
        WrDouble();
        IF head.NumPics > 0 THEN
           FOR i := 0 TO head.NumPics-1 DO
               ReadFile( f, pic_def.Length, SIZE(pic_def.Length) );
               ReadFile( f, pic_def.PicStr, pic_def.Length );
               IF i # 0 THEN WrSingle(); END; (* if *)
               WrMsg( 'Picture Length' ); IO.WrCard( pic_def.Length, 10 ); IO.WrLn();
               WrMsg( 'Picture String' ); WrStr    ( pic_def.PicStr     ); IO.WrLn();
           END; (* next *)
         ELSE
           IO.WrStr( " File without pictures's." ); IO.WrLn();
        END; (* if *)
        WrDouble();
        IF head.NumArrays > 0 THEN
           FOR i := 0 TO head.NumArrays-1 DO
               ReadFile( f, array_def, SIZE(array_def) );
               IF i # 0 THEN WrDouble(); END; (* if *)
               WITH array_def DO
               WrMsg( "Dimension's #"     ); IO.WrCard( NumDIM,   10 ); IO.WrLn();
               WrMsg( "Dimension's Total" ); IO.WrCard( TotalDIM, 10 ); IO.WrLn();
               WrMsg( 'Element Size'      ); IO.WrCard( ElemSize, 10 ); IO.WrLn();
               WrDouble();
               IF TotalDIM > 0 THEN
                  FOR j := 0 TO TotalDIM-1 DO
                      IF j # 0 THEN WrSingle(); END; (* if *)
                      ReadFile( f, array_comp, SIZE(array_comp) );
                      WrMsg( 'High Dimension'   ); IO.WrCard( array_comp.MaxDIM, 10 ); IO.WrLn();
                      WrMsg( 'Lenght Dimension' ); IO.WrCard( array_comp.Length, 10 ); IO.WrLn();
                  END; (* next *)
                ELSE
                  IO.WrStr( " File without dimension's." ); IO.WrLn();
               END; (* if *)
               END; (* with *)
           END; (* next *)
         ELSE
           IO.WrStr( " File without array's." ); IO.WrLn();
        END; (* if *)
        WrDouble();
        IF head.NumRecs > 0 THEN
           ALLOCATE( record, head.RecLen );
           FOR li := 0 TO head.NumRecs - 1 DO
               ReadFile( f, rec_head, SIZE(rec_head) );
               IF head.RecLen < SIZE(rec_head) THEN
                  WrError( RECSIZEerror, file_name );
               END; (* if *)
               ReadFile( f, record^,  head.RecLen - SIZE(rec_head) );
               IF li # 0 THEN WrSingle(); END; (* if *)
               WrMsg( 'Record Head' );    IO.WrShtCard( rec_head.RecHead,    10 );
               WrRecHead( rec_head.RecHead ); IO.WrLn();
               WrMsg( 'Record Pointer' ); IO.WrLngCard( rec_head.RecPointer, 10 ); IO.WrLn();
           END; (* next *)
           DEALLOCATE( record, head.RecLen );
         ELSE
           IO.WrStr( " File without record's." ); IO.WrLn();
        END; (* if *)
        WrDouble();
        IO.WrStr( '<EOF>' ); IO.WrLn();
        IO.WrLn();
    END WrHead;

    VAR is :BOOLEAN;
        f  :FIO.File;
BEGIN
    is := FIOR.IsExtension( file_name, 'DAT' );
    IF ~ is THEN
       WrError( EXTerror, file_name );
     ELSE
       IF FIO.Exists( file_name ) THEN
          f := FIO.Open( file_name );
          FIO.Seek( f, 0 );
          WrHead( f );
          FIO.Close( f );
        ELSE
          WrError( NotFound, file_name );
       END; (* if *)
    END; (* IF *)
END ViewHead;

PROCEDURE ReadCLI();
    VAR file_name :FIO.PathStr;
        count     :CARDINAL;
        ok        :BOOLEAN;
        entry     :FIO.DirEntry;
BEGIN
    FIO.IOcheck := FALSE;
    count := Lib.ParamCount();
    CASE count OF
       0 : WrHelp();   |
       1 : Lib.ParamStr( file_name, 1 );
           IF (Str.CharPos( file_name, '*' ) = MAX(CARDINAL)) &
              (Str.CharPos( file_name, '?' ) = MAX(CARDINAL))
            THEN
              ViewHead( file_name );
            ELSE
              ok := FIO.ReadFirstEntry( file_name, FIO.FileAttr{}, entry );
              WHILE ok DO
                 ViewHead( entry.Name );
                 ok := FIO.ReadNextEntry( entry );
              END; (* while *)
           END; (* if *) |
      ELSE WrHelp();
    END; (* CASE *)
END ReadCLI;

PROCEDURE ClaView;
BEGIN
    IO.WrStr( 'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿' ); IO.WrLn();
    IO.WrStr( '³ Clarion File Viewer. HGS-Software. (c) 1995. v1.00.  ³' ); IO.WrLn();
    IO.WrStr( 'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ' ); IO.WrLn();
    IO.WrLn();
    ReadCLI();
    IO.WrLn();
END ClaView;

TYPE File = POINTER TO ClarionType;

TYPE KeyType = RECORD
        head   :KeyHead;
        fields :ARRAY [ 0..15 ] OF KeyComponent;
     END; (* KeyType *)
     ClarionType = RECORD
        file   :FM.File;
        head   :FileHead;
        fields :ARRAY [ 0..MaxFields ] OF FieldHead;
        keys   :ARRAY [ 0..MaxKeys   ] OF KeyType;
     END; (* ClarionType *)

PROCEDURE FillStr( VAR str :ARRAY OF CHAR );
    VAR i :CARDINAL;
BEGIN
    FOR i := 0 TO HIGH(str) DO
        str[i] := ' ';
    END; (* next *)
END FillStr;

PROCEDURE InitFile( VAR cl :ClarionType; pre :ARRAY OF CHAR );
    VAR i, j :CARDINAL;
BEGIN
    cl.head.FileSig   := FileSignature;
    cl.head.FileAttr  := FileCreate;
    cl.head.NumKeys   := 0;
    cl.head.NumRecs   := 0;
    cl.head.NumDels   := 0;
    cl.head.NumFields := 0;
    cl.head.NumPics   := 0;
    cl.head.NumArrays := 0;
    cl.head.RecLen    := 0;
    cl.head.OffSet    := 0;
    cl.head.LogEOF    := 0;
    cl.head.LogBOF    := 0;
    cl.head.FreeRec   := 0;
    FillStr( cl.head.RecName  );
    FillStr( cl.head.MemoName );
    Str.Caps( pre );
    Str.Copy( cl.head.FilePre, pre );
    Str.Copy( cl.head.RecPre,  pre );
    cl.head.MemoLen   := 0;
    cl.head.MemoWidht := 0;
    cl.head.LockCount := 0;
    cl.head.ChgTime   := 0;
    cl.head.ChgDate   := 0;
    cl.head.Checksum  := 0;
    FOR i := 0 TO HIGH( cl.fields ) DO
        cl.fields[i].FieldType   := 0;
        cl.fields[i].FieldName   := '';
        cl.fields[i].FOffSet     := 0;
        cl.fields[i].Length      := 0;
        cl.fields[i].DecSignif   := 0;
        cl.fields[i].DecDec      := 0;
        cl.fields[i].ArrayNumber := 0;
        cl.fields[i].PictureNum  := 0;
    END; (* next *)
    FOR i := 0 TO HIGH( cl.keys ) DO
        cl.keys[i].head.NumComponent := 0;
        cl.keys[i].head.KeyName      := '';
        cl.keys[i].head.Type         := 0;
        cl.keys[i].head.Length       := 0;
        FOR j := 0 TO HIGH( cl.keys[i].fields ) DO
            cl.keys[i].fields[j].FieldType   := 0;
            cl.keys[i].fields[j].FieldNumber := 0;
            cl.keys[i].fields[j].RecOffSet   := 0;
            cl.keys[i].fields[j].Length      := 0;
        END; (* next *)
    END; (* next *)
END InitFile;

PROCEDURE Init( VAR file :File; pre :ARRAY OF CHAR );
BEGIN
    ALLOCATE( file, SIZE(file^) );
    InitFile( file^, pre );
END Init;

PROCEDURE AssignField( VAR field :ARRAY OF CHAR; pre, name :ARRAY OF CHAR );
    VAR aux :ARRAY [ 0..127 ] OF CHAR;
BEGIN
    FillStr( field );
    Str.Copy( aux, pre );
    Str.Append( aux, ':' );
    Str.Append( aux, name );
    Str.Caps( aux );
    Str.Insert( field, aux, 0 );
END AssignField;

PROCEDURE AddField( VAR file :File; name :ARRAY OF CHAR; tp, sz :CARDINAL );
    VAR field :CARDINAL;
BEGIN
    file^.head.NumFields := file^.head.NumFields + 1;
    field := file^.head.NumFields-1;
    file^.fields[ field ].FieldType := SHORTCARD(tp);
    AssignField( file^.fields[ field ].FieldName, file^.head.RecPre, name );
    file^.fields[ field ].FOffSet   := file^.head.RecLen;
    file^.fields[ field ].Length    := sz;
    file^.head.RecLen := file^.head.RecLen + file^.fields[ field ].Length;
END AddField;

PROCEDURE AddFieldLong( VAR file :File; name :ARRAY OF CHAR );
BEGIN
    AddField( file, name, LongType, LongSize );
END AddFieldLong;

PROCEDURE AddFieldReal( VAR file :File; name :ARRAY OF CHAR );
BEGIN
    AddField( file, name, RealType, RealSize );
END AddFieldReal;

PROCEDURE AddFieldStr( VAR file :File; name :ARRAY OF CHAR; sz :CARDINAL );
BEGIN
    AddField( file, name, StringType, sz );
END AddFieldStr;

PROCEDURE AddFieldPic( VAR file :File; name :ARRAY OF CHAR );
BEGIN
    AddField( file, name, PicType, PicSize );
END AddFieldPic;

PROCEDURE AddFieldByte( VAR file :File; name :ARRAY OF CHAR );
BEGIN
    AddField( file, name, ByteType, ByteSize );
END AddFieldByte;

PROCEDURE AddFieldShort( VAR file :File; name :ARRAY OF CHAR );
BEGIN
    AddField( file, name, ShortType, ShortSize );
END AddFieldShort;

PROCEDURE AddFieldGroup( VAR file :File; name :ARRAY OF CHAR );
BEGIN
    AddField( file, name, GroupType, GroupSize );
END AddFieldGroup;

PROCEDURE AddFieldDec( VAR file :File; name :ARRAY OF CHAR );
BEGIN
    AddField( file, name, DecimalType, DecimalSize );
END AddFieldDec;

PROCEDURE AddKey( VAR file :File; name :ARRAY OF CHAR; attr :SHORTCARD );
    VAR key :CARDINAL;
        x   :SHORTCARD;
BEGIN
    attr := attr + key_case;
    x    := 0;
    IF ~CHK( attr, key_unique ) THEN x := x + KeyAllowDup;    END; (* if *)
    IF  CHK( attr, key_case   ) THEN x := x + KeyNoCaseSense; END; (* if *)
    IF  CHK( attr, key_null   ) THEN x := x + KeyAllowNulls;  END; (* if *)
    IF  CHK( attr, key_index  ) THEN x := x + KeyIndex;       END; (* if *)

    file^.head.NumKeys := file^.head.NumKeys + 1;
    key := CARDINAL( file^.head.NumKeys-1 );
    file^.keys[ key ].head.Type := x;
    AssignField( file^.keys[ key ].head.KeyName, file^.head.RecPre, name );
END AddKey;

PROCEDURE AddKeyField( VAR file :File; fnum :CARDINAL );
    VAR key   :CARDINAL;
        field :CARDINAL;
        fdef  :FieldHead;
BEGIN
    key   := CARDINAL( file^.head.NumKeys-1 );
    field := CARDINAL( file^.keys[ key ].head.NumComponent );

    fdef  := file^.fields[ fnum ];
    file^.keys[ key ].fields[ field ].FieldType   := fdef.FieldType;
    file^.keys[ key ].fields[ field ].FieldNumber := fnum + 1;
    file^.keys[ key ].fields[ field ].RecOffSet   := fdef.FOffSet;
    file^.keys[ key ].fields[ field ].Length      := SHORTCARD(fdef.Length);

    file^.keys[ key ].head.NumComponent := file^.keys[ key ].head.NumComponent + 1;
    file^.keys[ key ].head.Length       := file^.keys[ key ].head.Length + SHORTCARD(fdef.Length);
END AddKeyField;

PROCEDURE SetKeyAttr( VAR file :File; attr :CARDINAL );
    VAR key :CARDINAL;
BEGIN
    key := CARDINAL( file^.head.NumKeys-1 );
    file^.keys[key].head.Type := file^.keys[key].head.Type + SHORTCARD(attr);
END SetKeyAttr;

PROCEDURE UpDateHead( VAR file :File );
    VAR old_pos :LONGCARD;
BEGIN
    old_pos := FM.GetPos( file^.file );
    FM.Seek( file^.file, 0 );
    FM.WrBlock( file^.file, file^.head, SIZE(file^.head) );
    FM.Seek( file^.file, old_pos );
END UpDateHead;

PROCEDURE CopyStruct( VAR target :File; source :File );
BEGIN
    target^ := source^;
END CopyStruct;

PROCEDURE MakeKeyName( name :ARRAY OF CHAR; VAR aux :ARRAY OF CHAR; i :CARDINAL );
    VAR extension :ARRAY [ 0..4 ] OF CHAR;
        number    :ARRAY [ 0..4 ] OF CHAR;
        old_char  :CHAR;
BEGIN
    Str.Copy( aux, name );
    old_char := String.FillChar;
    String.FillChar := '0';
    String.Card2Str( number, i, 2 );
    String.FillChar := old_char;
    Str.Concat( extension, 'k', number );
    FM.ChangeExt( aux, extension );
END MakeKeyName;

PROCEDURE Open( VAR file :File; name :ARRAY OF CHAR );
    VAR i, j :CARDINAL;
        aux  :FM.PathStr;
BEGIN
    Str.Copy( aux, name );
    FM.ChangeExt( aux, 'dat' );
    file^.file := FM.Open( aux );
    FM.RdBlock( file^.file, file^.head, SIZE(file^.head) );
    FOR i := 0 TO file^.head.NumFields-1 DO
        FM.RdBlock( file^.file, file^.fields[i], SIZE(file^.fields[i]) );
    END; (* next *)
    IF file^.head.NumKeys > 0 THEN
       FOR i := 0 TO CARDINAL(file^.head.NumKeys)-1 DO
           FM.RdBlock( file^.file, file^.keys[i].head, SIZE(file^.keys[i].head) );
           IF file^.keys[i].head.NumComponent > 0 THEN
              FOR j := 0 TO CARDINAL(file^.keys[i].head.NumComponent)-1 DO
                  FM.RdBlock( file^.file, file^.keys[i].fields[j], SIZE(file^.keys[i].fields[j]));
              END; (* next *)
           END; (* if *)
       END; (* next *)
    END; (* if *)
    Seek( file, 0 );
END Open;

PROCEDURE Create( VAR file :File; name :ARRAY OF CHAR );
    VAR i, j :CARDINAL;
        aux  :FM.PathStr;
BEGIN
    Str.Copy( aux, name );
    FM.ChangeExt( aux, 'dat' );
    file^.file := FM.Create( aux );
    FM.WrBlock( file^.file, file^.head, SIZE(file^.head) );
    FOR i := 0 TO file^.head.NumFields-1 DO
        FM.WrBlock( file^.file, file^.fields[i], SIZE(file^.fields[i]) );
    END; (* next *)
    IF file^.head.NumKeys > 0 THEN
       FOR i := 0 TO CARDINAL(file^.head.NumKeys)-1 DO
           MakeKeyName( name, aux, i );
           IF FM.Exists( aux ) THEN
              FM.Erase( aux );
           END; (* if *)
           FM.WrBlock( file^.file, file^.keys[i].head, SIZE(file^.keys[i].head) );
           IF file^.keys[i].head.NumComponent > 0 THEN
              FOR j := 0 TO CARDINAL(file^.keys[i].head.NumComponent)-1 DO
                  FM.WrBlock( file^.file, file^.keys[i].fields[j], SIZE(file^.keys[i].fields[j]));
              END; (* next *)
           END; (* if *)
       END; (* next *)
    END; (* if *)
    file^.head.OffSet := FM.Size( file^.file );
    file^.head.RecLen := file^.head.RecLen + SIZE(RecHead);
    UpDateHead( file );
END Create;

PROCEDURE Seek( VAR file :File; i :LONGCARD );
BEGIN
    FM.Seek( file^.file, i * LONGCARD(file^.head.RecLen) + file^.head.OffSet );
END Seek;

PROCEDURE Read( VAR file :File; VAR rec :ARRAY OF BYTE ) :BOOLEAN;
    VAR rh :RecHead;
        ok :BOOLEAN;
BEGIN
    FM.RdBlock( file^.file, rh, SIZE(rh) );
    FM.RdBlock( file^.file, rec, file^.head.RecLen-SIZE(RecHead) );
    ok := ~ CHK( rh.RecHead, RecordDeleted );
    RETURN ok;
END Read;

PROCEDURE Write( VAR file :File; VAR rec :ARRAY OF BYTE );
    VAR rh :RecHead;
BEGIN
    rh.RecHead    := RecordNew;
    rh.RecPointer := 0;
    FM.WrBlock( file^.file, rh, SIZE(rh) );
    FM.WrBlock( file^.file, rec, file^.head.RecLen-SIZE(RecHead) );
    file^.head.NumRecs := file^.head.NumRecs + 1;
    file^.head.LogEOF  := file^.head.NumRecs;
    file^.head.LogBOF  := 1;
    UpDateHead( file );
END Write;

PROCEDURE Close( VAR file :File );
BEGIN
    FM.Close( file^.file );
END Close;

PROCEDURE Done( VAR file :File );
BEGIN
    DEALLOCATE( file, SIZE(file^) );
END Done;

PROCEDURE Fields( VAR file :File ) :CARDINAL;
BEGIN
    RETURN file^.head.NumFields;
END Fields;

PROCEDURE FieldName( VAR file :File; i :CARDINAL; VAR name :ARRAY OF CHAR );
BEGIN
    Str.Copy( name, file^.fields[i].FieldName );
END FieldName;

PROCEDURE FieldType( VAR file :File; i :CARDINAL ) :CARDINAL;
BEGIN
    RETURN CARDINAL(file^.fields[i].FieldType);
END FieldType;

PROCEDURE FieldSize( VAR file :File; i :CARDINAL ) :CARDINAL;
BEGIN
    RETURN file^.fields[i].Length;
END FieldSize;

PROCEDURE FieldData( VAR file :File; i :CARDINAL; VAR r, d :ARRAY OF BYTE );
    VAR off, size :CARDINAL;
BEGIN
    off  := file^.fields[i].FOffSet;
    size := file^.fields[i].Length;
    IF (SIZE(d) # size) & (file^.fields[i].FieldType # field_string_type) THEN
       IO.WrStr( 'Error in size: ' );
       IO.WrCard( size, 1 ); IO.WrStr( ' = ' ); IO.WrCard( SIZE(d), 1 );
       Lib.FatalError( 'Incompatible fields types' );
    END;
    Lib.Move( ADR(r[off]), ADR(d), size );
END FieldData;

PROCEDURE Records( VAR file :File ) :LONGCARD;
BEGIN
    RETURN file^.head.NumRecs;
END Records;

(* ------------------------------------------------------------------------ *)
(* Procedures for convert dos-date & dos-time into clarion date & time.     *)
(* ------------------------------------------------------------------------ *)

PROCEDURE AssignByte( VAR byte :C_BYTE; _bool :BOOLEAN );
BEGIN
    IF _bool THEN
       byte := 1;
     ELSE
       byte := 0;
    END; (* if *)
END AssignByte;

PROCEDURE AssignShort( VAR short :C_SHORT; _card :CARDINAL );
BEGIN
    short := _card;
END AssignShort;

PROCEDURE AssignLong( VAR long :C_LONG; _long :LONGCARD );
BEGIN
    long := _long;
END AssignLong;

PROCEDURE AssignStr( VAR str :ARRAY OF CHAR; _str  :ARRAY OF CHAR );
    VAR aux :ARRAY [ 0..255 ] OF CHAR;
BEGIN
    FillStr( aux );
    Str.Insert( aux, _str, 0 );
    Str.Copy( str, aux );
END AssignStr;

PROCEDURE AssignReal( VAR real :C_REAL; _real :LONGREAL );
BEGIN
    real := _real;
END AssignReal;

(* ------------------------------------------------------------------------ *)
(* Clarion DATEs are passed as a LONG, the DATE record can be used to       *)
(* resolve the elements of the date from the LONG value.                    *)
(* ------------------------------------------------------------------------ *)

TYPE ConvDATE = RECORD
       CASE :BOOLEAN OF
         TRUE  : l     :C_LONG;  |
         FALSE : Day   :C_BYTE;
                 Month :C_BYTE;
                 Year  :C_SHORT; |
       END; (* case *)
    END; (* ConvDATE *)

(* ------------------------------------------------------------------------ *)
(* Clarion TIMEs are passed as a LONG, the TIME record can be used to       *)
(* resolve the elements of the time from the LONG value.                    *)
(* ------------------------------------------------------------------------ *)

TYPE ConvTIME = RECORD
       CASE :BOOLEAN OF
          TRUE  : l    :C_LONG; |
          FALSE : Hund :C_BYTE;
                  Sec  :C_BYTE;
                  Min  :C_BYTE;
                  Hour :C_BYTE; |
       END; (* case *)
    END; (* ConvTIME *)

PROCEDURE AssignDate( VAR date :C_DATE; _date :CARDINAL );
BEGIN
    Date_To_CLA_Date( _date, date );
END AssignDate;

PROCEDURE AssignTime( VAR time :C_TIME; _time :CARDINAL );
BEGIN
END AssignTime;

(* ------------------------------------------------------------------------ *)
(* Compress & UnCompress Date & Time types for clarion DataBase.            *)
(* ------------------------------------------------------------------------ *)

PROCEDURE YearDays( year :CARDINAL ) :LONGCARD;
BEGIN
    IF (year MOD 4) = 0 THEN
       RETURN 366;
     ELSE
       RETURN 365;
    END; (* if *)
END YearDays;

PROCEDURE MonthDays( month, year :CARDINAL ) :LONGCARD;
BEGIN
    RETURN LONGCARD(DateTime.MaxDaysInMonth( month, year ));
END MonthDays;

PROCEDURE DateCompress( d, m, a :CARDINAL ) :C_DATE;
    VAR i     :CARDINAL;
        count :LONGCARD;
        sum   :LONGCARD;
        date  :C_DATE;
BEGIN
    count := 2;
    IF a > 1 THEN
       FOR i := 1801 TO a - 1 DO
           sum   := YearDays( i );
           count := count + sum;
       END; (* if *)
    END; (* if *)
    IF m > 1 THEN
       FOR i := 1 TO m - 1 DO
           sum   := MonthDays( i, a );
           count := count + sum;
       END; (* next *)
    END; (* if *)
    count := count + LONGCARD(d);
    date  := count;
    RETURN date;
END DateCompress;

PROCEDURE DateUnCompress( date :C_DATE; VAR d, m, a :CARDINAL );
    VAR tdate :LONGCARD;
        day   :CARDINAL;
        month :CARDINAL;
        year  :CARDINAL;
BEGIN
    IF (date < 4) OR (date > 109211) THEN
       a := 1801;
       m :=    1;
       d :=    1;
       RETURN;
    END; (* if *)
    year  := 1801;
    month :=    1;
    day   :=    1;
    tdate := date - 2;
    WHILE (tdate > YearDays( year )) DO
        tdate := tdate - YearDays( year );
        year  := year + 1;
    END; (* while *)
    WHILE (tdate > MonthDays( month, year )) DO
        tdate := tdate - MonthDays( month, year );
        month := month + 1;
    END; (* while *)
    day := day + CARDINAL(tdate) - 1;
    a   := year;
    m   := month;
    d   := day;
END DateUnCompress;

PROCEDURE Date_To_CLA_Date( date :CARDINAL; VAR _date :C_DATE );
    VAR d, m, a :CARDINAL;
BEGIN
    DateTime.DateUnCompress( date, d, m, a );
    _date := DateCompress( d, m, DateTime.BaseYear + a );
END Date_To_CLA_Date;

PROCEDURE CLA_Date_To_Date( date :C_DATE; VAR _date :CARDINAL );
    VAR d, m, a :CARDINAL;
BEGIN
    DateUnCompress( date, d, m, a );
    _date := DateTime.DateCompress( d, m, a - DateTime.BaseYear );
END CLA_Date_To_Date;

PROCEDURE DOS_Date_To_STD_Date( DosDate :CARDINAL) :C_DATE;
    VAR CDate :ConvDATE;
BEGIN
    CDate.Year  := C_USHORT( BITSET( DosDate >> 9H) * BITSET(7FH)) + 1980;
    CDate.Month := C_BYTE  ( BITSET( DosDate >> 5H) * BITSET(0FH));
    CDate.Day   := C_BYTE  ( BITSET( DosDate >> 0H) * BITSET(1FH));
    RETURN CDate.l;
END DOS_Date_To_STD_Date;

PROCEDURE DOS_Time_To_STD_Time( DosTime :CARDINAL) :C_TIME;
    VAR CTime :ConvTIME;
BEGIN
    CTime.Hour := C_BYTE( BITSET( DosTime >> 11H) * BITSET(1FH));
    CTime.Min  := C_BYTE( BITSET( DosTime >> 05H) * BITSET(3FH));
    CTime.Sec  := C_BYTE( CARDINAL(BITSET(DosTime) * BITSET(1FH)) << 1);
    CTime.Hund := 0;
    RETURN CTime.l;
END DOS_Time_To_STD_Time;

END db_cla.
