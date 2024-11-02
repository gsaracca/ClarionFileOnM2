IMPLEMENTATION MODULE db_dos;

IMPORT DateTime, String, GDialog, Sound, Win;
IMPORT Str, Window, Lib, Storage;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

PROCEDURE PosLogica( pos     :FM.FileIndex;
                     max     :FM.IndexType;
                     recSize :FM.SizeType ) :FM.IndexType;
(*
   Dada una posici¢n f¡sica de un archivo da la posici¢n l¢gica del mismo.
*)
    VAR pos_fisica :FM.FileIndex;
        aux        :FM.FileIndex;
BEGIN
    (*
       Controlo la posici¢n f¡sica entre los rangos :
                            FileHeadSize .. max * recSize.
    *)
    IF pos < FileHeadSize THEN
       pos := FileHeadSize;
    END; (* if *)
    aux := VAL( FM.FileIndex, max ) * VAL( FM.FileIndex, recSize );
    IF pos > aux THEN
       pos := aux;
    END; (* if *)
    pos_fisica := (pos - FileHeadSize);
    (*
       Transformo la posici¢n f¡sica en l¢gica.
    *)
    (*
       IF (pos_fisica MOD VAL( fileIndex, recSize)) # 0 THEN
          Recover( 'posici¢n logica erronea' );
       END; (* if *)
    *)
    RETURN VAL( FM.IndexType, pos_fisica DIV VAL( FM.FileIndex, recSize ));
END PosLogica;

PROCEDURE CtrlCreateFile( FName :ARRAY OF CHAR; RecSize :FM.SizeType );
    VAR str :ARRAY [ 0..80 ] OF CHAR;
BEGIN
    IF RecSize > FM.DataSize THEN
       String.Card2Str( str, RecSize, 6 );
       Str.Prepend( str, 'registro demasiado grande. size:' );
       FM.DoError( FName, str );
    END; (* if *)
END CtrlCreateFile;

PROCEDURE CtrlOpenFile( VAR f :FileHeadType; FName :ARRAY OF CHAR; FSize :FM.FileIndex ) :BOOLEAN;
    VAR regs_fisicos :FM.FileIndex;
        regs_logicos :FM.IndexType;
        recover, ok  :BOOLEAN;
        str, aux     :ARRAY [ 0..80 ] OF CHAR;
BEGIN
    regs_fisicos := FSize - FileHeadSize;
    regs_logicos := regs_fisicos DIV FM.IndexType( f.rec_size );
    recover      := FALSE;
    (*
       Errores recuperables.
    *)
    recover := FM.Recover( FName, 'tipo de archivo invalido', f.filetype # DefArch );
    IF recover THEN
       f.filetype := DefArch;
    END; (* if *)
    recover := FM.Recover( FName, 'versi¢n incorrecta', f.ver # DefVer );
    IF recover THEN
       f.ver := DefVer;
    END; (* if *)
    recover := FM.Recover( FName, 'distinta cantidad de registros', f.max # regs_logicos );
    IF recover THEN
       f.max := regs_logicos;
       Str.CardToStr( LONGCARD( regs_logicos ), aux, 10, ok );
       Str.CardToStr( LONGCARD( f.max ),        str, 10, ok );
       Str.Prepend( aux, 'Cant. in Head: ' );
       Str.Append( aux,  '  Cant. in File: ' );
       Str.Append( aux, str );
       GDialog.Wait_key( aux );
    END; (* if *)
    recover := FM.Recover( FName, 'archivo mal cerrado', f.open & (~ FM.share) );
    IF recover THEN
       f.open := TRUE;
    END; (* if *)
    recover := FM.Recover( FName, 'archivo corrupto', regs_fisicos MOD FM.FileIndex(f.rec_size) # 0 );
    IF recover THEN
       f.max := regs_logicos;
    END; (* if *)
    IF recover THEN
       f.sort := FALSE;
    END; (* if *)
    RETURN recover;
END CtrlOpenFile;

PROCEDURE MakeName( dir, name, fext :ARRAY OF CHAR; VAR dname :ARRAY OF CHAR );
    VAR file_ext :FM.ExtStr;
BEGIN
    IF fext[0] = 0C THEN
       Str.Copy( file_ext, FM.DB_DOS_FEXT );
     ELSE
       Str.Copy( file_ext, fext );
    END; (* if *)
    FM.MakeName( dir, name, file_ext, dname );
END MakeName;

CLASS IMPLEMENTATION DosFile;

    PROCEDURE LockFile();
    BEGIN
        FM.Lock( f_DAT, 0, FileHeadSize );
    END LockFile;

    PROCEDURE UnLockFile();
    BEGIN
        FM.UnLock( f_DAT, 0, FileHeadSize );
    END UnLockFile;

    PROCEDURE WrHead();
        VAR pos :FM.FileIndex;
    BEGIN
        pos := FM.GetPos( f_DAT );
        FM.Seek( f_DAT, 0 );
        FM.Lock   ( f_DAT, 0,        FileHeadSize );
        FM.WrBlock( f_DAT, FileHead, FileHeadSize );
        FM.Flush( f_DAT );
        FM.UnLock ( f_DAT, 0,        FileHeadSize );
        FM.Seek( f_DAT, pos );
    END WrHead;

    PROCEDURE RdHead();
        VAR pos :FM.FileIndex;
    BEGIN
        pos := FM.GetPos( f_DAT );
        FM.Seek( f_DAT, 0 );
        FM.Lock   ( f_DAT, 0,        FileHeadSize );
        FM.RdBlock( f_DAT, FileHead, FileHeadSize );
        FM.UnLock ( f_DAT, 0,        FileHeadSize );
        FM.Seek( f_DAT, pos );
    END RdHead;

    PROCEDURE SeekData( i :FM.IndexType );
    BEGIN
        FM.Seek( f_DAT, i*FM.FileIndex(FileHead.rec_size)+FileHeadSize);
    END SeekData;

    PROCEDURE WrData( VAR d :ARRAY OF BYTE );
        VAR pos :FM.FileIndex;
    BEGIN
        pos := FM.GetPos(f_DAT);
        FM.Lock   ( f_DAT, pos, FM.FileIndex(FileHead.rec_size) );
        FM.WrBlock( f_DAT, d,   FileHead.rec_size );
        FM.UnLock ( f_DAT, pos, FM.FileIndex(FileHead.rec_size) );
    END WrData;

    PROCEDURE RdData( VAR d :ARRAY OF BYTE ) :BOOLEAN;
        VAR ok  :BOOLEAN;
            pos :FM.FileIndex;
    BEGIN
        pos := FM.GetPos( f_DAT );
        ok := FM.Lock( f_DAT, pos, FM.FileIndex(FileHead.rec_size) );
        IF ok THEN
           FM.RdBlock( f_DAT, d,   FileHead.rec_size );
           FM.UnLock ( f_DAT, pos, FM.FileIndex(FileHead.rec_size) );
        END; (* if *)
        RETURN ok;
    END RdData;

    PROCEDURE GetData( i :FM.IndexType; VAR data :ARRAY OF BYTE ) :BOOLEAN;
        VAR ok :BOOLEAN;
    BEGIN
        ok := i < FileHead.max;
        IF ok THEN
           SeekData( i );
           ok := RdData( data );
        END; (* if *)
        RETURN ok;
    END GetData;

    PROCEDURE PutData( i :FM.IndexType; VAR data :ARRAY OF BYTE ) :BOOLEAN;
        VAR ok :BOOLEAN;
    BEGIN
        ok := i < FileHead.max;
        IF ok THEN
           SeekData( i ); WrData( data );
        END; (* if *)
        RETURN ok;
    END PutData;

    VIRTUAL PROCEDURE InitData( VAR d :ARRAY OF BYTE );
    END InitData;

    VIRTUAL PROCEDURE Comp( d1, d2 :ARRAY OF BYTE ) :BOOLEAN;
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

    PROCEDURE SetFExt( fext :ARRAY OF CHAR );
    BEGIN
        Str.Copy( FExt, fext );
    END SetFExt;

    PROCEDURE SetPath( path :ARRAY OF CHAR );
    BEGIN
        Str.Copy( Dir, path );
    END SetPath;

    PROCEDURE Open( name :ARRAY OF CHAR; sizeRec :CARDINAL; new :BOOLEAN ) :BOOLEAN;
        VAR ok :BOOLEAN;
    BEGIN
        IF OpenExists( name ) THEN
           IF FileHead.rec_size # sizeRec THEN
              FM.DoError( name, 'registros de diferente tama¤o' );
           END; (* if *)
           ok := TRUE;
         ELSIF new THEN
           Create( name, sizeRec );
           ok := TRUE;
         ELSE
           ok := FALSE;
        END; (* if *)
        RETURN ok;
    END Open;

    PROCEDURE Create( name :ARRAY OF CHAR; sizeRec :CARDINAL );
    BEGIN
        MakeName( Dir, name, FExt, DataName );
        CtrlCreateFile( name, sizeRec );
        f_DAT := FM.Create( DataName  ); FM.Close( f_DAT );
        FM.SetShare( FM._deny_rw );
        f_DAT := FM.Open( DataName  );
        WITH FileHead DO
           rec_size := sizeRec;       (* el tama¤o de los datos guardados  *)
           open     := TRUE;          (* si el archivo esta abierto        *)
           sort     := TRUE;          (* si el archivo esta ordenado.      *)
           pos      := 0;             (* posici¢n l¢gica actual.           *)
           max      := 0;             (* FileSize of file en forma l¢gica. *)
        END; (* with : FileHead *)
        FM.WrBlock( f_DAT, FileHead, SIZE(FileHead) );
        FM.SetShare( FM._deny_none );
    END Create;

    PROCEDURE OpenExists( name :ARRAY OF CHAR ) :BOOLEAN;
        VAR ok :BOOLEAN;
    BEGIN
        MakeName( Dir, name, FExt, DataName );
        IF Exists( name ) THEN
           f_DAT := FM.Open( DataName );
           RdHead();
           IF CtrlOpenFile( FileHead, name, FM.Size(f_DAT) ) THEN
              SeekData( FileHead.max );
              FM.Truncate( f_DAT );
           END; (* if *)
           FileHead.open := TRUE;
           WrHead();
           ok := TRUE;
         ELSE
           ok := FALSE;
        END; (* if *)
        RETURN ok;
    END OpenExists;

    PROCEDURE Exists( name :ARRAY OF CHAR ) :BOOLEAN;
        VAR full   :FM.PathStr;
            exists :BOOLEAN;
    BEGIN
        MakeName( Dir, name, FExt, full );
        exists := FM.Exists( full );
        RETURN exists;
    END Exists;

    PROCEDURE Sync();
    BEGIN
        WrHead();
        FM.Flush( f_DAT );
    END Sync;

    PROCEDURE FileSize() :FM.IndexType;
        VAR max :FM.IndexType;
    BEGIN
        RdHead();
        max := FileHead.max;
        RETURN max;
    END FileSize;

    PROCEDURE CloseFile();
    BEGIN
        RdHead();
        FileHead.open := FALSE;
        WrHead();
        FM.Close( f_DAT );
    END CloseFile;

    PROCEDURE New( VAR d :ARRAY OF BYTE );
    BEGIN
        RdHead();
        SeekData( FileHead.max ); WrData( d );
        INC( FileHead.max );
        FileHead.sort := FALSE;
        WrHead();
    END New;

    PROCEDURE Put( i :FM.IndexType; VAR d :ARRAY OF BYTE );
        VAR oldd :FM.DataType;
    BEGIN
        RdHead();
        IF FileHead.RdOnPut THEN
           (*
              Lee lo que hab¡a en el archivo.
           *)
           GetData( i, oldd );
        END; (* if *)
        (*
           Comparo si no es lo mismo que voy a escribir.
        *)
        IF FileHead.RdOnPut &
           (Lib.Compare( ADR( d ), ADR( oldd ), FileHead.rec_size ) =
                         FileHead.rec_size ) THEN RETURN END;
        PutData( i, d );
        FileHead.sort := FALSE;
        WrHead();
    END Put;

    PROCEDURE Get( i :FM.IndexType; VAR d :ARRAY OF BYTE ) :BOOLEAN;
        VAR ok :BOOLEAN;
    BEGIN
        ok := i < FileHead.max;
        IF ok THEN
           ok := GetData( i, d );
        END; (* if *)
        RETURN ok;
    END Get;

    PROCEDURE Del( i :FM.IndexType );
        VAR d          :FM.DataType;
            oldRdOnPut :BOOLEAN;
    BEGIN
        IF FileHead.max > 0 THEN
           Get( FileHead.max - 1, d );
           oldRdOnPut := FileHead.RdOnPut;
           FileHead.RdOnPut := FALSE;
           Put( i, d );
           FileHead.RdOnPut := oldRdOnPut;
           DEC( FileHead.max );
           SeekData ( FileHead.max ); FM.Truncate( f_DAT );
           FileHead.sort := FALSE;
           WrHead();
        END; (* if *)
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
        IF FileHead.max > 0 THEN
           ok := BSearch( 0, FileHead.max - 1, index );
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

        TYPE FileName = ARRAY [ 0..19 ] OF CHAR;

        VAR nameG1 :FileName;
            nameG2 :FileName;
            nameF1 :FileName;
            nameF2 :FileName;
            i      :LONGCARD;
            w      :Win.window;
            ok     :BOOLEAN;

        PROCEDURE SwapName( VAR n1, n2 :FileName );
            VAR aux :FileName;
        BEGIN
            aux := n1;
            n1  := n2;
            n2  := aux;
        END SwapName;

        PROCEDURE Split();
            VAR f1, f2    :DosFile;
                WriteFile :DosFile;
                WriteOne  :BOOLEAN;
                d         :FM.ptrDT;
        BEGIN
            FM.Flush( f_DAT );
            SeekData( 0 );
            f1.Create( nameF1, FileHead.rec_size ); SeekData(0);
            f2.Create( nameF2, FileHead.rec_size ); SeekData(0);
            WriteOne := TRUE;
            ALLOCATE( d, FileHead.rec_size );
            FOR i := 0 TO FileHead.max - 1 DO
               FM.RdBlock( f_DAT, d^, FileHead.rec_size );
               IF WriteOne
                  THEN WriteFile := f1;
                  ELSE WriteFile := f2;
               END; (* if *)
               WriteFile.New( d^ );
               WriteOne := ~ WriteOne;
            END; (* loop *)
            DEALLOCATE( d, FileHead.rec_size );
            f1.CloseFile();
            f2.CloseFile();
        END Split;

        PROCEDURE Combina( k :LONGCARD;
                           wrName1, wrName2,
                           rdName1, rdName2 :ARRAY OF CHAR );
            VAR select :BOOLEAN;
                wr     :DosFile;
                wr1    :DosFile;
                wr2    :DosFile;
                rd1    :DosFile;
                rd2    :DosFile;
                count1 :LONGCARD;
                count2 :LONGCARD;
                w1     :BOOLEAN;
                w2     :BOOLEAN;
                a      :CARDINAL;
                a1, a2 :FM.ptrDT;
        BEGIN
            ALLOCATE( a1, FileHead.rec_size );
            ALLOCATE( a2, FileHead.rec_size );
            wr1.Create( wrName1, FileHead.rec_size );       wr1.SeekData(0);
            wr2.Create( wrName2, FileHead.rec_size );       wr2.SeekData(0);
            rd1.Open  ( rdName1, FileHead.rec_size, TRUE ); rd1.SeekData(0);
            rd2.Open  ( rdName2, FileHead.rec_size, TRUE ); rd2.SeekData(0);
            select := TRUE;
            FM.EOF := FALSE;
            WHILE ~ FM.EOF DO
               IF select THEN wr := wr1; ELSE wr := wr2; END;
               (*
                  Comienzo merge.
               *)
               count1 := 0;
               count2 := 0;
               w1 := TRUE;
               w2 := TRUE;
               LOOP
                  IF w1 THEN
                     rd1.RdData( a1^ );
                     IF FM.EOF THEN a := 1; FM.EOF := FALSE; EXIT; END;
                     w1 := FALSE;
                  END;
                  IF w2 THEN
                     rd2.RdData( a2^ );
                     IF FM.EOF THEN a := 2; FM.EOF := FALSE; EXIT; END;
                     w2 := FALSE;
                  END;
                  IF Comp( a1^, a2^ ) THEN
                     wr.New( a1^ ); w1 := TRUE;
                     INC( count1 ); IF count1 = k THEN a := 1; EXIT; END;
                   ELSE
                     wr.New( a2^ ); w2 := TRUE;
                     INC( count2 ); IF count2 = k THEN a := 2; EXIT; END;
                  END;
               END; (* loop *)
               CASE a OF
                1 : IF ~ w2 THEN
                       wr.New( a2^ );
                       INC( count2 );
                    END;
                    WHILE ~ FM.EOF & (count2 < k ) DO
                        rd2.RdData( a2^ );
                        IF ~ FM.EOF THEN
                           INC( count2 );
                           wr.New( a2^ );
                        END;
                    END; |
                2 : IF ~ w1 THEN
                       wr.New( a1^ );
                       INC( count1 );
                    END;
                    WHILE ~ FM.EOF & (count1 < k ) DO
                        rd1.RdData( a1^ );
                        IF ~ FM.EOF THEN
                           INC( count1 );
                           wr.New( a1^ );
                       END;
                    END; (* while *) |
               END;
               (*
                  Final merge.
               *)
               select := ~ select;
            END; (* while FM.EOF *)
            IF FM.EOF THEN
               LOOP
                  rd2.RdData( a2^ );
                  IF FM.EOF THEN EXIT; END;
                  wr.New( a2^ );
               END;
             ELSE
               LOOP
                  rd1.RdData( a1^ );
                  IF FM.EOF THEN EXIT; END;
                  wr.New( a1^ );
               END; (* loop *)
            END; (* if *)
            wr1.CloseFile();
            wr2.CloseFile();
            rd1.CloseFile();
            rd2.CloseFile();
            DEALLOCATE( a1, FileHead.rec_size );
            DEALLOCATE( a2, FileHead.rec_size );
        END Combina;

        PROCEDURE WrMsg( str :ARRAY OF CHAR );
        BEGIN
            Window.GotoXY( 3, 4 ); Window.ClrEol();
            Win.WrStr( str ); Win.WrStr( '...' );
        END WrMsg;

        PROCEDURE CopyNDX();
            VAR g1 :DosFile;
                i  :LONGCARD;
                d  :FM.ptrDT;
        BEGIN
            ALLOCATE( d, FileHead.rec_size );
            SeekData( 0 );
            g1.Open( nameG1, FileHead.rec_size, TRUE ); g1.SeekData(0);
            FOR i := 0 TO FileHead.max-1 DO
                g1.RdData( d^ );
                FM.WrBlock( f_DAT, d^, FileHead.rec_size );
            END; (* next *)
            g1.CloseFile();
            DEALLOCATE( d, FileHead.rec_size );
        END CopyNDX;

        VAR lock_size :LONGCARD;

    BEGIN
        FM.Close(f_DAT);
        f_DAT := FM.Open( DataName );
        RdHead();
        ok := FALSE;
        lock_size := FM.Size( f_DAT );
        IF FM.Lock( f_DAT, 0, lock_size ) THEN
           IF ~ FileHead.sort THEN
              w.Init( 19, 5, 41, 6, Win.Inverse, TRUE,
                        '', Win.Center, Win.Inverse,
                        Win.Half, Win.Inverse );
              Window.GotoXY( 3, 2 ); Win.WrStr( 'Ordenando: ' );
              Win.WrStr( DataName );
              nameG1 := 'g1.$$$';
              nameG2 := 'g2.$$$';
              nameF1 := 'f1.$$$';
              nameF2 := 'f2.$$$';
              WrMsg( 'Spliting' );
              Split();
              WrMsg( 'Sorting'  );
              i := 1;
              IF i <= FileHead.max THEN
                 Window.GotoXY( 16, 4 ); Win.WrLngCard( i, 5 );
                 LOOP
                    Combina( i, nameG1, nameG2, nameF1, nameF2 );
                    i := i * 2;
                    Window.GotoXY( 16, 4 ); Win.WrLngCard( i, 5 );
                    IF i > FileHead.max THEN EXIT; END;
                    SwapName( nameG1, nameF1 );
                    SwapName( nameG2, nameF2 );
                 END; (* loop *)
                 Erase( nameF1 );
                 Erase( nameF2 );
               ELSE
                 nameG1 := nameF1;
                 nameG2 := nameF2;
              END; (* if *)
              WrMsg( 'Coping'    );     CopyNDX();
              WrMsg( 'Erase (1)' );     Erase( nameG1 );
              WrMsg( 'Erase (2)' );     Erase( nameG2 );
              FileHead.sort := TRUE;    WrMsg( 'Ready' );
              w.Close();
              ok := TRUE;
           END; (* if *)
           FM.UnLock( f_DAT, 0, lock_size );
           IF ok THEN
              WrHead();
           END; (* if *)
         ELSE
           GDialog.Wait_key( '¯ no puedo ordenar, mientras otros trabajan ®' );
        END; (* if *)
        RETURN ok;
    END SortFile;

    PROCEDURE Copy( _file :DosFile; name :ARRAY OF CHAR );
        VAR size  :LONGCARD;
            i     :LONGCARD;
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
                     Win.Half, Win.Inverse );
           Window.GotoXY( 3, 1 ); Win.WrStr( 'Copiando archivo : ' );
           Win.WrStr( name );
           NEW( pdata );
           size := FileSize();
           IF size > 0 THEN
              DEC( size );
              MakeName( Dir, name, FExt, _file.DataName );
              _file.Create( name, FileHead.rec_size );
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
    WITH FileHead DO
       id       := FM.HGS_ID;     (* HGS Soft identificador            *)
       filetype := DefArch;       (* tipo de archivo                   *)
       ver      := DefVer;        (* versi¢n de la data_file           *)
       user_ver := FM.DefUserVer; (* versi¢n de los datos del usuario  *)
       rec_size := 0;             (* el tama¤o de los datos guardados  *)
       encript  := '';            (* si el archivo esta encriptado     *)
       RdOnPut  := TRUE;          (* if RdOnPut => when "Put" compare  *)
       open     := FALSE;         (* si el archivo esta abierto        *)
       sort     := FALSE;         (* si el archivo esta ordenado.      *)
       pos      := 0;             (* posici¢n l¢gica actual.           *)
       max      := 0;             (* FileSize of file en forma l¢gica. *)
       eoh      := FM.EOH;        (* Fin de la cabecera.               *)
    END; (* with *)
    f_DAT     := FM.NullFile;
    Dir       := '';
    DataName  := '';
    FExt      := '';
END DosFile;

PROCEDURE Erase( name :ARRAY OF CHAR );
    VAR name_dat :FM.PathStr;
BEGIN
    MakeName( '', name, '', name_dat );
    IF FM.Exists( name_dat ) THEN FM.Erase( name_dat ); END; (* if *)
END Erase;

PROCEDURE Rename( old_name, new_name :ARRAY OF CHAR );
    VAR n_name_dat :FM.PathStr;
        o_name_dat :FM.PathStr;
BEGIN
    MakeName( '', old_name, '', o_name_dat ); FM.Rename( o_name_dat, n_name_dat );
    MakeName( '', new_name, '', n_name_dat ); FM.Rename( o_name_dat, n_name_dat );
END Rename;

(* ------------------------------------------------------------------------ *)
(* Procedimientos de db_dfile.                                              *)
(* ------------------------------------------------------------------------ *)

PROCEDURE Load( name, fext :ARRAY OF CHAR; VAR data :ARRAY OF BYTE );
    VAR f :DosFile;
BEGIN
    f.SetFExt( fext );
    f.Open( name, SIZE( data ), TRUE );
    f.Get( 0, data );
    f.CloseFile();
END Load;

PROCEDURE Save( name, fext :ARRAY OF CHAR; VAR data :ARRAY OF BYTE );
    VAR f :DosFile;
BEGIN
    f.SetFExt( fext );
    f.Create( name, SIZE( data ) );
    f.New( data );
    f.CloseFile();
END Save;

END db_dos.
