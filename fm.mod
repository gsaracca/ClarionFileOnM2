IMPLEMENTATION MODULE FM;

IMPORT GDialog, DateTime;
IMPORT Lib, Str, SYSTEM, Process;
IMPORT CoreProc;

(* ------------------------------------------------------------------------ *)
(* Control internos de errores: IOresult, FIO : IOresult, FIOR : IOresult   *)
(* ------------------------------------------------------------------------ *)

(*%T _mthread *)
VAR IOR :ARRAY [ 1..Process.MaxProcess ] OF CARDINAL;
(*%E *)
(*%F _mthread *)
VAR IOR :CARDINAL;
(*%E *)

PROCEDURE SetIOR( nro :CARDINAL );
BEGIN
(*%T _mthread *)
  IOR[ CoreProc._getTID() ] := nro;
(*%E *)
(*%F _mthread *)
  IOR := nro;
(*%E *)
END SetIOR;

PROCEDURE IOresult() :CARDINAL;
BEGIN
(*%T _mthread *)
  RETURN IOR[ CoreProc._getTID() ];
(*%E *)
(*%F _mthread *)
  RETURN IOR;
(*%E *)
END IOresult;

PROCEDURE InitIOR();
(*%T _mthread *)
    VAR n :[ 1..Process.MaxProcess ];
(*%E *)
BEGIN
(*%T _mthread *)
    FOR n := 1 TO Process.MaxProcess DO
        IOR[n] := 0;
    END; (* next *)
(*%E *)
(*%F _mthread *)
    IOR := 0;
(*%E *)
END InitIOR;

PROCEDURE DosCall( VAR r :SYSTEM.Registers );
    VAR err :BOOLEAN;
BEGIN
(*%T _mthread *)
    Process.Lock();
(*%E *)
    Lib.Dos( r );
(*%T _mthread *)
    Process.Unlock();
(*%E *)
    err := (BITSET{ SYSTEM.CarryFlag } * r.Flags ) # BITSET{};
    IF err THEN
       SetIOR(r.AX);
     ELSE
       SetIOR(00H);
    END; (* if *)
END DosCall;

PROCEDURE IntrCall( VAR r :SYSTEM.Registers; i :CARDINAL );
BEGIN
(*%T _mthread *)
    Process.Lock();
(*%E *)
    Lib.Intr( r, i );
(*%T _mthread *)
    Process.Unlock();
(*%E *)
END IntrCall;

PROCEDURE MakeASCIIZ( name :ARRAY OF CHAR; VAR ASCIIZ :ARRAY OF CHAR );
BEGIN
    Str.Copy( ASCIIZ, name );
    ASCIIZ[ SIZE( ASCIIZ ) - 1 ] := CHR(0);
END MakeASCIIZ;

(* ------------------------------------------------------------------------ *)
(* Procediemientos internos.                                                *)
(* ------------------------------------------------------------------------ *)

TYPE ConvertType = RECORD
        CASE :BOOLEAN OF
           TRUE  : lo, hi :CARDINAL; |
           FALSE : long   :LONGCARD; |
        END; (* case *)
     END; (* conv *)

PROCEDURE DosLock( file :File; offset, long :LONGCARD );
    VAR r    :SYSTEM.Registers;
        conv :ConvertType;
BEGIN
    r.AH := 05CH;    (* funci¢n del DOS para hacer un lock de un archivo. *)
    r.AL := 00;      (* Par metro de "Lock".                              *)
    r.BX := file;    (* Sobre que archivo se realiza la operaci¢n.        *)
    (* Offset del archivo a "lock". *)
    conv.long := offset;
    r.CX := conv.hi;
    r.DX := conv.lo;
    (* Longitud del sector del archivo a "lock". *)
    conv.long := long;
    r.SI := conv.hi;
    r.DI := conv.lo;
    DosCall( r );
END DosLock;

PROCEDURE DosUnLock( file :File; offset, long :LONGCARD );
    VAR r    :SYSTEM.Registers;
        conv :ConvertType;
BEGIN
    r.AH := 05CH;    (* funci¢n del DOS para hacer un lock de un archivo. *)
    r.AL := 01;      (* Par metro de "UnLock".                            *)
    r.BX := file;    (* Sobre que archivo se realiza la operaci¢n.        *)
    (* Offset del archivo a "unlock". *)
    conv.long := offset;
    r.CX := conv.hi;
    r.DX := conv.lo;
    (* Longitud del sector del archivo a "unlock". *)
    conv.long := long;
    r.SI := conv.hi;
    r.DI := conv.lo;
    DosCall( r );
END DosUnLock;

PROCEDURE Share() :BOOLEAN;
    VAR r :SYSTEM.Registers;
BEGIN
    r.AH := 10H;
    r.AL := 00H;
    IntrCall( r, 02FH );
    RETURN (r.AL = 0FFH);
END Share;

PROCEDURE SetFileAttr( name :ARRAY OF CHAR; attr :CARDINAL );
    VAR nameZ :PathStr;
        r     :SYSTEM.Registers;
BEGIN
    MakeASCIIZ( name, nameZ );
    r.DS := Seg( nameZ );
    r.DX := Ofs( nameZ );
    r.AH := 43H;
    r.AL := 01H;
    r.CX := attr;
    DosCall( r );
END SetFileAttr;

PROCEDURE GetFileAttr( name :ARRAY OF CHAR; VAR attr :CARDINAL );
    VAR nameZ :PathStr;
        r     :SYSTEM.Registers;
BEGIN
    MakeASCIIZ( name, nameZ );
    r.DS := Seg( nameZ );
    r.DX := Ofs( nameZ );
    r.AH := 43H;
    r.AL := 00H;
    DosCall( r );
    attr := r.CX;
END GetFileAttr;

PROCEDURE SplitDir(     dirStr :ARRAY OF CHAR;
                    VAR drive  :SHORTCARD;
                    VAR dir    :ARRAY OF CHAR ) :BOOLEAN;
(*
   Dado un string : "dirStr".
   Devuelve el drive indicado y el directorio indicado.
   Retornando true si todo esta bien.
*)
    VAR pos    :CARDINAL;
        ok     :BOOLEAN;
        drChar :CHAR;
BEGIN
(*
    pos := Str.Pos( dirStr, '..' );
    WHILE pos # MAX( CARDINAL ) DO
        String.Subst( dirStr, '..', '' );
        pos := Str.NextPos( dirStr, '..', pos );
    END; (* while *)
*)
    pos := Str.RCharPos( dirStr, ':' );
    IF (pos # MAX( CARDINAL )) & (pos >= 1) THEN
       Str.Slice( dir, dirStr, pos + 1, 255 );
       drChar := CAP( dirStr[ pos - 1 ] );
       IF (drChar < 'A') OR (drChar > 'Z') THEN
          ok := FALSE;
        ELSE
          drive := SHORTCARD( ORD( drChar ) - 64 );
          ok := TRUE;
       END; (* if *)
     ELSIF (pos = MAX( CARDINAL )) THEN
       drive := FIO.GetDrive();
       Str.Copy( dir, dirStr );
       ok := TRUE;
     ELSE
       ok := FALSE;
    END; (* if *)
    Str.Caps( dir );
    RETURN ok;
END SplitDir;

PROCEDURE ExistsDir( dir :ARRAY OF CHAR ) :BOOLEAN;
(*
   Determina si existe el directorio : "dir".
*)
    VAR existe     :BOOLEAN;
        split      :BOOLEAN;
        olddir     :ARRAY [ 0..20 ] OF CHAR;
        oldIOcheck :BOOLEAN;
        drive      :SHORTCARD;
BEGIN
    oldIOcheck := FIO.IOcheck;
    FIO.IOcheck := FALSE;
    FIO.GetDir( 0, olddir );
    existe := FALSE;
    split := SplitDir( dir, drive, dir );
    IF split THEN
       FIO.ChDir( dir );
       IF FIO.IOresult() = 0 THEN
          FIO.ChDir( olddir );
          existe := TRUE;
       END; (* if *)
    END; (* if *)
    FIO.IOcheck := oldIOcheck;
    RETURN existe;
END ExistsDir;

(* ------------------------------------------------------------------------ *)
(* Control de errores.                                                      *)
(* ------------------------------------------------------------------------ *)

PROCEDURE DosError( function :ARRAY OF CHAR; err_nro :CARDINAL ) :BOOLEAN;
    VAR msg     :ARRAY [ 0..77 ] OF CHAR;
        err_str :ARRAY [ 0..77 ] OF CHAR;
        ok      :BOOLEAN;
BEGIN
    CASE err_nro OF
       000H : err_str := 'ok';                              |
       001H : err_str := 'Invalid function number';         |
       002H : err_str := 'File not found';                  |
       003H : err_str := 'Path not found';                  |
       004H : err_str := 'Too many open files';             |
       005H : err_str := 'Access denied';                   |
       006H : err_str := 'Invalid handle';                  |
       007H : err_str := 'Memory control blocks destroyed'; |
       008H : err_str := 'Insufficient memory';             |
       009H : err_str := 'Invalid memory block address';    |
       00AH : err_str := 'Invalid environment';             |
       00BH : err_str := 'Invalid format';                  |
       00CH : err_str := 'Invalid access code';             |
       00DH : err_str := 'Invalid data';                    |
       00FH : err_str := 'Invalid drive was specified';     |
       010H : err_str := 'Remove the current directory';    |
       011H : err_str := 'Not same device';                 |
       012H : err_str := 'No more files';                   |
       013H : err_str := 'Write-protected disk error';      |
       014H : err_str := 'Unknown unit';                    |
       015H : err_str := 'Drive not ready';                 |
       016H : err_str := 'Unknown command';                 |
       017H : err_str := 'Data error (CRC)';                |
       018H : err_str := 'Bad request structure length';    |
       019H : err_str := 'Seek error';                      |
       01AH : err_str := 'Unknown media type';              |
       01BH : err_str := 'Sector not found';                |
       01CH : err_str := 'Printer out of paper';            |
       01DH : err_str := 'Write fault';                     |
       01EH : err_str := 'Read fault';                      |
       01FH : err_str := 'General failure';                 |
       020H : err_str := 'Sharing Violation';               |
       021H : err_str := 'Lock Violation';                  |
       022H : err_str := 'Invalid disk change';             |
       023H : err_str := 'FCB unavailable';                 |
       024H : err_str := 'Sharing buffer overflow';         |
       025H : err_str := 'Code page mismatch';              |
       027H : err_str := 'Handle disk full';                |
       032H : err_str := 'Network request not found';       |
       033H : err_str := 'Remote computer not listening';   |
       034H : err_str := 'Duplicate name on network';       |
       035H : err_str := 'Network name not found';          |
       036H : err_str := 'Network busy';                    |
       037H : err_str := 'Network device no longer exists'; |
       038H : err_str := 'NetBIOS command limit exceeded';  |
       039H : err_str := 'Network adapter error';           |
       03AH : err_str := 'Incorret network response';       |
       03BH : err_str := 'Unexpected network error';        |
       03CH : err_str := 'Incompatible remote adapter';     |
       03DH : err_str := 'Print queue full';                |
       03EH : err_str := 'Not enough space for print file'; |
       03FH : err_str := 'Print file deleted';              |
       040H : err_str := 'Network name deleted';            |
       041H : err_str := 'Access denied';                   |
       042H : err_str := 'Network device type incorret';    |
       043H : err_str := 'Netowrk name not found';          |
       044H : err_str := 'Network name limit exceeded';     |
       045H : err_str := 'NetBIOS session limit exceeded';  |
       046H : err_str := 'Sharing temporary paused';        |
       047H : err_str := 'Network request not acepted';     |
       048H : err_str := 'Print or disk redirect is paused';|
       050H : err_str := 'File already exists';             |
       051H : err_str := 'Duplicate FCB';                   |
       052H : err_str := 'Cannot make directory entry';     |
       053H : err_str := 'Fail on INT 24';                  |
       054H : err_str := 'Too many redirections';           |
       055H : err_str := 'Duplicate redirection';           |
       056H : err_str := 'Invalid password';                |
       057H : err_str := 'Invalid parameter';               |
       058H : err_str := 'Network data fault';              |
       0F0H : err_str := 'Disk Full (write failed)';  (* JPI internal *)
       ELSE   err_str := 'Unknown DOS Error';
    END; (* case *)
    IF err_nro = 000H THEN
       ok := TRUE;
     ELSE
       Str.Concat( msg, '¯ "', function );
       Str.Append( msg, '" : ' );
       Str.Append( msg, err_str );
       Str.Append( msg, ' (' );
       Str.CardToStr(LONGCARD(err_nro), err_str, 16, ok);
       Str.Append( msg, err_str );
       Str.Append( msg, ') ¨ Reintento ? ®' );
       ok := ~ GDialog.Wait_yes( msg );
    END; (* if *)
    RETURN ok;
END DosError;

(* ------------------------------------------------------------------------ *)
(* Control de errores.                                                      *)
(* ------------------------------------------------------------------------ *)

PROCEDURE fm_error( proc :ARRAY OF CHAR ) :BOOLEAN;
BEGIN
    RETURN DosError( proc, IOresult() );
END fm_error;

PROCEDURE fio_error( proc :ARRAY OF CHAR ) :BOOLEAN;
BEGIN
    RETURN DosError( proc, FIO.IOresult() );
END fio_error;

PROCEDURE fior_error( proc :ARRAY OF CHAR ) :BOOLEAN;
BEGIN
    RETURN DosError( proc, FIOR.IOresult() );
END fior_error;

(* ------------------------------------------------------------------------ *)
(* Procedimientos externos.                                                 *)
(* ------------------------------------------------------------------------ *)

PROCEDURE SetShare( share_mode :BITSET );
BEGIN
    FIO.ShareMode := share_mode;
END SetShare;

PROCEDURE Exists( name :ARRAY OF CHAR ) :BOOLEAN;
   VAR ok        :BOOLEAN;
       full_path :FIOR.PathStr;
BEGIN
    ok := FIOR.FindPath( name, full_path );
    RETURN ok;
END Exists;

PROCEDURE Lookup( name :ARRAY OF CHAR; new, append :BOOLEAN ) :File;
    VAR f :File;
BEGIN
    IF Exists( name ) THEN
       IF append THEN
          f := Append( name );
        ELSE
          f := Open( name );
       END; (* if *)
     ELSE
       f := Create( name );
    END; (* if *)
    RETURN f;
END Lookup;

PROCEDURE Open( name :ARRAY OF CHAR ) :File;
    VAR f :File;
BEGIN
    LOOP
       f := FIOR.OpenRW( name );
       IF fior_error( 'OPEN' ) THEN
          EXIT;
       END; (* if *)
    END; (* loop *)
    RETURN f;
END Open;

PROCEDURE Append( name :ARRAY OF CHAR ) :File;
    VAR f :File;
BEGIN
    LOOP
       f := FIOR.OpenRW( name );
       IF fior_error( 'APPEND' ) THEN
          EXIT;
       END; (* if *)
    END; (* loop *)
    Seek( f, Size(f));
    RETURN f;
END Append;

PROCEDURE Create( name :ARRAY OF CHAR ) :File;
    VAR f :File;
BEGIN
    LOOP
       f := FIOR.Create( name );
       IF fior_error( 'CREATE' ) THEN
          EXIT;
       END; (* if *)
    END; (* loop *)
    RETURN f;
END Create;

PROCEDURE Erase( name :ARRAY OF CHAR );
BEGIN
    LOOP
       FIOR.Erase( name );
       IF fior_error( 'ERASE' ) THEN
          EXIT;
       END; (* case *)
    END; (* loop *)
END Erase;

PROCEDURE Rename( source, target :ARRAY OF CHAR );
    VAR _source :PathStr;
        _target :PathStr;
BEGIN
    Str.Copy( _source, source );
    Str.Copy( _target, target );
    IF FIOR.FindPath( _source, _source ) &
       FIOR.FindNewPath( _target, _target )
     THEN
       LOOP
          FIO.Rename( _source, _target );
          IF fio_error( 'RENAME' ) THEN
             EXIT;
          END; (* if *)
       END; (* loop *)
    END; (* if *)
END Rename;

PROCEDURE Close( f :File );
BEGIN
    LOOP
       FIO.Close( f );
       IF fio_error( 'CLOSE' ) THEN
          EXIT;
       END; (* if *)
    END; (* loop *)
END Close;

PROCEDURE Lock( f :File; pos, size :LONGCARD ) :BOOLEAN;
    VAR ok :BOOLEAN;
BEGIN
(*%T multi *)
    IF size = 0 THEN RETURN TRUE; END; (* if *)
    LOOP
       DosLock( f, pos, size );
       ok := fm_error( 'LOCK' );
       IF ok THEN
          EXIT;
       END; (* if *)
    END; (* loop *)
    RETURN ok;
(*%E *)
(*%F multi *)
    RETURN TRUE;
(*%E *)
END Lock;

PROCEDURE UnLock( f :File; pos, size :LONGCARD ) :BOOLEAN;
    VAR ok :BOOLEAN;
BEGIN
(*%T multi *)
    IF size = 0 THEN RETURN TRUE; END; (* if *)
    LOOP
       DosUnLock( f, pos, size );
       ok := fm_error( 'UNLOCK' );
       IF ok THEN
          EXIT;
       END; (* if *)
    END; (* loop *)
    RETURN ok;
(*%E *)
(*%F multi *)
    RETURN TRUE;
(*%E *)
END UnLock;

PROCEDURE RdBlock( f :File; VAR buff :ARRAY OF BYTE; size :CARDINAL ) :BOOLEAN;
    VAR b  :CARDINAL;
        ok :BOOLEAN;
BEGIN
    LOOP
       b:=FIO.RdBin( f, buff, size );
       ok := fio_error( 'READ' );
       IF ok THEN
          EXIT;
       END; (* if *)
    END; (* loop *)
    RETURN ok;
END RdBlock;

PROCEDURE WrBlock( f :File; buff :ARRAY OF BYTE; size :CARDINAL ) :BOOLEAN;
    VAR ok :BOOLEAN;
BEGIN
    LOOP
       FIO.WrBin( f, buff, size );
       ok := fio_error( 'WRITE' );
       IF ok THEN
          EXIT;
       END; (* if *)
    END; (* loop *)
    RETURN ok;
END WrBlock;

PROCEDURE Seek( f :File; pos :LONGCARD ) :BOOLEAN;
    VAR ok :BOOLEAN;
BEGIN
    LOOP
       FIO.Seek( f, pos );
       ok := fio_error( 'SEEK' );
       IF ok THEN
          EXIT;
       END; (* if *)
    END; (* loop *)
    RETURN ok;
END Seek;

PROCEDURE Flush( f :File ) :BOOLEAN;
    VAR ok :BOOLEAN;
BEGIN
    LOOP
       FIO.Flush( f );
       ok := fio_error( 'FLUSH' );
       IF ok THEN
          EXIT;
       END; (* if *)
    END; (* loop *)
    RETURN ok;
END Flush;

PROCEDURE GetPos( f :File ) :LONGCARD;
    VAR pos :LONGCARD;
BEGIN
    LOOP
       pos := FIO.GetPos( f );
       IF fio_error( 'GETPOS' ) THEN
          EXIT;
       END; (* if *)
    END; (* loop *)
    RETURN pos;
END GetPos;

PROCEDURE Size( f :File ) :LONGCARD;
    VAR size :LONGCARD;
BEGIN
    LOOP
       size := FIO.Size( f );
       IF fio_error( 'SIZE' ) THEN
          EXIT;
       END; (* if *)
    END; (* loop *)
    RETURN size;
END Size;

PROCEDURE Truncate( f :File );
BEGIN
    LOOP
       FIO.Truncate( f );
       IF fio_error( 'TRUNCATE' ) THEN
          EXIT;
       END; (* if *)
    END; (* loop *)
END Truncate;

PROCEDURE MakeName( path, tail, ext :ARRAY OF CHAR; VAR fname :ARRAY OF CHAR );
    VAR fpath, ftail, fext :PathStr;
BEGIN
    Str.Copy( fpath, path );
    Str.Copy( ftail, tail );
    Str.Copy( fext,  ext  );
    IF fpath[0] # 0C THEN
       Str.Append( fpath, '\' );
       Str.Prepend( ftail, fpath );
    END; (* if *)
    FIOR.RemoveExtension( ftail );
    Str.Copy( fname, ftail );
    FIOR.ChangeExtension( fname, fext );
END MakeName;

(* ------------------------------------------------------------------------ *)
(* Procedimientos generales de seguimiento.                                 *)
(* ------------------------------------------------------------------------ *)

PROCEDURE AddError( err_str :ARRAY OF CHAR; break :BOOLEAN );
    CONST log_file = 'base_ndx.err';
    VAR f    :File;
        s1,
        s2   :ARRAY [ 0..12 ] OF CHAR;
        date :DateTime.DateType;
        time :DateTime.TimeType;
BEGIN
    f := Lookup( log_file, TRUE, TRUE );
    DateTime.GetDate( date ); DateTime.DateToStr( date, s1 );
    DateTime.GetTime( time ); DateTime.TimeToStr( time, s2 );
    FIO.WrStr( f, '* Informe: ' ); FIO.WrStr( f, s1 );
    FIO.WrStr( f, ' ' );           FIO.WrStr( f, s2 );
    FIO.WrStr( f, '.' ); FIO.WrLn( f );
    FIO.WrStr( f, err_str );
    FIO.WrStr( f, '.' ); FIO.WrLn( f );
    Close( f );
    IF break THEN
       HALT;
    END; (* if *)
END AddError;

PROCEDURE Recover ( file_name, err_str :ARRAY OF CHAR; cond  :BOOLEAN ) :BOOLEAN;
    VAR a  :ARRAY [ 0..80 ] OF CHAR;
        ok :BOOLEAN;
BEGIN
    IF cond THEN
       Str.Concat( a, '¯ Error, ', err_str );
       Str.Append( a, ' : ' );
       Str.Append( a, file_name );
       Str.Append( a, '.  ¨ Se recupera ? ®' );
       AddError( a, FALSE );
       ok := GDialog.Wait_yes( a );
       IF ~ ok THEN
          HALT;
       END; (* if *)
     ELSE
       ok := FALSE;
    END; (* if *)
    RETURN ok;
END Recover;

PROCEDURE DoError( file_name, err_str :ARRAY OF CHAR );
    VAR a :ARRAY [ 0..79 ] OF CHAR;
BEGIN
    Str.Concat( a, 'Error ==> ', err_str );
    Str.Append( a, ' : ' );
    Str.Append( a, file_name );
    Str.Append( a, '.' );
    AddError( a, TRUE );
END DoError;

(* ------------------------------------------------------------------------ *)
(* Implementaci¢n de procedimientos internos.                               *)
(* ------------------------------------------------------------------------ *)

PROCEDURE IsShare();
BEGIN
    IF ~ Share() THEN
       Lib.FatalError( 'Share not installed. Please load and try again.' );
     ELSE
       share := TRUE;
    END; (* if *)
END IsShare;

BEGIN
    InitIOR();
    share := FALSE;
(*%T multi *)
    IsShare();
    FIO.IOcheck   := FALSE;
    FIO.ShareMode := FIO.ShareDenyNone;
(*%E *)
END FM.
