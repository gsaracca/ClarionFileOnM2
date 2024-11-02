IMPLEMENTATION MODULE db_lzg;

IMPORT Str;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

CONST IdDef      = IdType( 'L','Z','G','W' );
      FileVerDef = 004H;
      Extension  = 'LZG';
      NullHead   = HeadType(
         IdDef,         (* file-ID                            *)
         FileVerDef,    (* file-version of LZG compressor     *)
         LZNO,          (* algoritmo de compression utilizado *)
         FTUNK,         (* tipo de archivo detectado          *)
         '',            (* nombre del archivo original        *)
         0,             (* attributos del archivo original    *)
         FM.NullDate,   (* fecha del archivo original         *)
         0,             (* Compress size                      *)
         0,             (* Original size                      *)
         0,             (* CRC de 32 bits                     *)
         0,             (* Cantidad de block's in the file    *)
         MaxBuff,       (* Longitud m xima del block de comp  *)
         FM.EOH );      (* Fin de la cabecera                 *)


PROCEDURE XOR( a, b :LONGINT ) :LONGINT;
    TYPE BIT32 = SET OF [ 0..31 ];
BEGIN
    RETURN LONGINT(BIT32(a) / BIT32(b));
END XOR;

CLASS IMPLEMENTATION LZG_FILE;

    PROCEDURE RdHead();
    BEGIN
        FM.Seek( file, 0 );
        FM.RdBlock( file, head, SIZE(head) );
    END RdHead;

    PROCEDURE WrHead();
    BEGIN
        FM.Seek( file, 0 );
        FM.WrBlock( file, head, SIZE(head) );
    END WrHead;

    PROCEDURE SetPath( _path :ARRAY OF CHAR );
    BEGIN
        Str.Copy( path, _path );
    END SetPath;

    PROCEDURE SetOver( on :BOOLEAN );
    BEGIN
        over := on;
    END SetOver;

    PROCEDURE SetAlgoritm( algo :AlgoritmType );
    BEGIN
        CASE algo OF
           LZNO : Compress   := LZDEF.NO_Compression;
                  UnCompress := LZDEF.NO_UnCompression; |
           LZKH : Compress   := LZDEF.KH_Compression;
                  UnCompress := LZDEF.KH_UnCompression; |
           LZM2 : Compress   := LZDEF.M2_Compression;
                  UnCompress := LZDEF.M2_UnCompression; |
           LZSS : Compress   := LZDEF.SS_Compression;
                  UnCompress := LZDEF.SS_UnCompression; |
        END; (* case *)
    END SetAlgoritm;

    PROCEDURE Open( _name :ARRAY OF CHAR );
    BEGIN
        IF Exists( _name ) THEN
           file := FM.Open( name );
           RdHead();
           IF (head.id      # IdDef)      THEN ErrorID := _NON_LZG_FILE;         END;
           IF (head.version # FileVerDef) THEN ErrorID := _BAD_LZG_VERSION_FILE; END;
           SetAlgoritm( head.algoritm );
         ELSE
           ErrorID := _FILE_NOT_EXISTS;
        END; (* if *)
        create_mode := FALSE;
    END Open;

    PROCEDURE Create( _name,
                      source_name :ARRAY OF CHAR;
                      source_size :LONGCARD;
                      source_date :FM.FileDate;
                      algo        :AlgoritmType;
                      ftype       :FileTypes );
    BEGIN
        file := FM.Create( name );
        WITH head DO
           id        := IdDef;
           version   := FileVerDef;
           algoritm  := algo;
           file_type := ftype;
           Str.Copy( file_name, source_name );
           attr      := FM.GetFileAttr( source_name );
           date      := source_date;
           comp_size := 0;
           orig_size := 0;
           CRC32     := source_size;
           blocks    := 0;
        END; (* with *)
        SetAlgoritm( algo );
        WrHead();
        create_mode := TRUE;
    END Create;

    PROCEDURE Exists( _name :ARRAY OF CHAR ) :BOOLEAN;
    BEGIN
        FM.MakeName( path, _name, Extension, name );
        RETURN FM.Exists( name );
    END Exists;

    PROCEDURE Close();
    BEGIN
        IF create_mode THEN
           head.comp_size := head.comp_size;
           head.CRC32     := XOR( head.CRC32, head.orig_size );
           WrHead();
        END; (* if *)
        FM.Close( file );
    END Close;

    PROCEDURE Add( src_buffer :PBuffer; size :CARDINAL ) :CARDINAL;
        VAR trg_buffer :PBuffer;
            block      :CARDINAL;
    BEGIN
        NEW(trg_buffer);
        block := Compress( ADR(src_buffer^), ADR(trg_buffer^), size );
        head.CRC32 := CRC.CalcCRC32( ADR(src_buffer^), size, head.CRC32 );

        FM.WrBlock( file, block, SIZE(block) );
        head.comp_size := head.comp_size + SIZE(block);

        FM.WrBlock( file, trg_buffer^, block );
        head.orig_size := head.orig_size + LONGCARD( size  );
        head.comp_size := head.comp_size + LONGCARD( block );
        head.blocks    := head.blocks    + 1;
        DISPOSE(trg_buffer);
        RETURN block;
    END Add;

    PROCEDURE Get( VAR trg_buffer :PBuffer; VAR Zsize, Nsize :CARDINAL ) :BOOLEAN;
        VAR src_buffer :PBuffer;
            block      :CARDINAL;
            real_size  :CARDINAL;
            bytes      :CARDINAL;
            ok         :BOOLEAN;
    BEGIN
        NEW(src_buffer);
        bytes := FM.RdBlockN( file, block, SIZE(block) );
        IF bytes > 0 THEN
           FM.RdBlock( file, src_buffer^, block );
           real_size := UnCompress( ADR(src_buffer^), ADR(trg_buffer^), block );
           ok := TRUE;
         ELSE
           ok := FALSE;
        END; (* if *)
        DISPOSE(src_buffer);
        Zsize := block;
        Nsize := real_size;
        RETURN ok;
    END Get;

    PROCEDURE IsError() :BOOLEAN;
    BEGIN
        RETURN ErrorID # _NO_ERROR;
    END IsError;

    PROCEDURE Error() :ErrorType;
        VAR err :ErrorType;
    BEGIN
        err := ErrorID;
        ErrorID := _NO_ERROR;
        RETURN err;
    END Error;

BEGIN
    head        := NullHead;
    file        := FM.NullFile;
    path        := '';
    name        := '';
    over        := TRUE;
    ErrorID     := _NO_ERROR;
    Compress    := LZDEF.NO_Compression;
    UnCompress  := LZDEF.NO_UnCompression;
    create_mode := FALSE;
END LZG_FILE;

END db_lzg.
