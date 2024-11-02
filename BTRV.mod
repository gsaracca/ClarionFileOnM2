IMPLEMENTATION MODULE BTRV;

(*# debug( vid => full ) *)

IMPORT SYSTEM,Lib,Str,IO,Window;
FROM Storage IMPORT ALLOCATE, DEALLOCATE;

(*%T _XTD*)
   IMPORT TSXLIB;
(*%E*)

(* ------------------------------------------------------------------------ *)
(* TopSpeed Modula-2 Interface for BTRIEVE - Supports TopSpeed Extender     *)
(* Public Domain - May be used without restriction                          *)
(*                                                                          *)
(* Versi¢n para MYLIB 2.0                                                   *)
(* (c) 1994.                                                                *)
(* Ver    : 1.00.                                                           *)
(* Release: 1000.0.                                                         *)
(* Date   : 19-05-94.                                                       *)
(* By     : Hector Gustavo Saracca.                                         *)
(* ------------------------------------------------------------------------ *)

(* ------------------------------------------------------------------------ *)
(* Internal Btrieve functions - for flex parameters use                     *)
(* ------------------------------------------------------------------------ *)

PROCEDURE BTrieve1( fn :CARDINAL );
    VAR NullControl  :File;
        NullBuffer   :LONGCARD;
        NullBuffSize :CARDINAL;
        NullKey      :KeyType;
BEGIN
    NullBuffSize := SIZE(NullBuffer);
    BTrieve(fn,NullControl,NullBuffer,NullBuffSize,NullKey,0);
END BTrieve1;

PROCEDURE BTrieve2( fn :CARDINAL; VAR f :File );
    VAR NullBuffer   :LONGCARD;
        NullBuffSize :CARDINAL;
        NullKey      :KeyType;
BEGIN
    NullBuffSize := SIZE(NullBuffer);
    BTrieve(fn,f,NullBuffer,NullBuffSize,NullKey,0);
END BTrieve2;

PROCEDURE BTrieve3( fn :CARDINAL; VAR f :File; Key :SHORTCARD );
    VAR NullBuffer     :LONGCARD;
        NullBuffSize :CARDINAL;
        NullKey        :KeyType;
BEGIN
    NullBuffSize := SIZE(NullBuffer);
    BTrieve(fn,f,NullBuffer,NullBuffSize,NullKey,Key);
END BTrieve3;

PROCEDURE BTrieve4( fn :CARDINAL; VAR f :File; VAR KeyBuff :ARRAY OF BYTE; Key :SHORTCARD );
    VAR NullBuffer     :LONGCARD;
        NullBuffSize :CARDINAL;
BEGIN
    NullBuffSize := SIZE(NullBuffer);
    BTrieve(fn,f,NullBuffer,NullBuffSize,KeyBuff,Key);
END BTrieve4;

(* ------------------------------------------------------------------------ *)
(* Btrieve file functions - function number in brackets                     *)
(* ------------------------------------------------------------------------ *)

PROCEDURE Open( FileName, OwnerName :ARRAY OF CHAR; op_enMode :SHORTCARD ) :File;
    VAR DatBuff  :ARRAY [ 0..7 ] OF CHAR;
        KeyBuff  :KeyType;
        BuffSize :CARDINAL;
        f        :File;
BEGIN
    Str.Copy(KeyBuff,FileName);
    Str.Copy(DatBuff,OwnerName);
    DatBuff[7] := 0C;
    BuffSize := Str.Length(DatBuff) + 1;
    BTrieve(op_Open,f,DatBuff,BuffSize,KeyBuff,op_enMode);
    RETURN f;
END Open;

PROCEDURE Create( FileName :ARRAY OF CHAR; FileDesc :ARRAY OF BYTE; DescSize :CARDINAL ) :File;
    VAR KeyBuff   :KeyType;
        f         :File;
        desc_size :CARDINAL;
BEGIN
    Str.Copy(KeyBuff,FileName);
    desc_size := DescSize;
    BTrieve(op_Create,f,FileDesc,desc_size,KeyBuff,0);
    RETURN f;
END Create;

PROCEDURE Close( VAR f :File );
BEGIN
    BTrieve2(op_Close,f);
END Close;

PROCEDURE Status( VAR f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE );
BEGIN
    BTrieve(op_Stat,f,DatBuff,BuffSize,KeyBuff,0);
END Status;

PROCEDURE Extend( VAR f :File; FileName :ARRAY OF CHAR; UseNow :BOOLEAN);
    VAR KeyBuff :KeyType;
        KeyID     :SHORTCARD;
BEGIN
    Str.Copy(KeyBuff,FileName);
    IF UseNow THEN KeyID := 255 ELSE KeyID := 0; END;
    BTrieve4(op_Extend,f,KeyBuff,KeyID);
END Extend;

PROCEDURE SetOwner( VAR f :File; OwnerName :ARRAY OF CHAR; AccessMode :SHORTCARD );
    VAR DatBuff   :OwnerType;
        KeyBuff    :KeyType;
        BuffSize :CARDINAL;
BEGIN
    Str.Copy(DatBuff,OwnerName);
    Str.Copy(KeyBuff,OwnerName);
    DatBuff[7] := 0C;
    BuffSize := Str.Length(DatBuff) + 1;
    BTrieve(op_SetOwner,f,DatBuff,BuffSize,KeyBuff,AccessMode);
END SetOwner;

PROCEDURE ClearOwner( VAR f :File );
BEGIN
     BTrieve2( op_ClearOwner, f );
END ClearOwner;

(* ------------------------------------------------------------------------ *)
(* Btrieve transactions functions - function number in brackets             *)
(* ------------------------------------------------------------------------ *)

PROCEDURE BeginTransaction();
BEGIN
    BTrieve1(op_BeginTrans);
END BeginTransaction;

PROCEDURE EndTransaction();
BEGIN
    BTrieve1(op_EndTrans);
END EndTransaction;

PROCEDURE AbortTransaction();
BEGIN
    BTrieve1(op_AbortTrans);
END AbortTransaction;

(* ------------------------------------------------------------------------ *)
(* Btrieve records functions - function number in brackets                  *)
(* ------------------------------------------------------------------------ *)

PROCEDURE Insert( VAR f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve(op_Insert,f,DatBuff,BuffSize,KeyBuff,KeyID);
END Insert;

PROCEDURE Update( VAR f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD);
BEGIN
    BTrieve(op_Update,f,DatBuff,BuffSize,KeyBuff,0);
END Update;

PROCEDURE Delete( VAR f :File; KeyID :SHORTCARD );
BEGIN
    BTrieve3(op_Delete,f,KeyID);
END Delete;

PROCEDURE GetPos( VAR f :File; VAR pos :PosType );
    VAR BuffSize :CARDINAL;
        NullKey  :KeyType;
BEGIN
    BuffSize := SIZE(pos);
    BTrieve( op_GetPos, f, pos, BuffSize, NullKey, 0 );
END GetPos;

PROCEDURE GetDirect( VAR f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD);
BEGIN
    BTrieve(op_GetDirect,f,DatBuff,BuffSize,KeyBuff,KeyID);
END GetDirect;

PROCEDURE StepDirect( VAR f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL );
    VAR NullKey :KeyType;
BEGIN
    BTrieve(op_StepDirect,f,DatBuff,BuffSize,NullKey,0);
END StepDirect;

(* ------------------------------------------------------------------------ *)
(* Btrieve Find functions.                                                  *)
(* ------------------------------------------------------------------------ *)

PROCEDURE FindEQ( VAR f :File; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve4(op_GetEQ+op_KeyOnly,f,KeyBuff,KeyID);
END FindEQ;

PROCEDURE FindNext( VAR f :File; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve4(op_GetNext+op_KeyOnly,f,KeyBuff,KeyID);
END FindNext;

PROCEDURE FindPrev( VAR f :File; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve4(op_GetPrev+op_KeyOnly,f,KeyBuff,KeyID);
END FindPrev;

PROCEDURE FindGT( VAR f :File; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve4(op_GetGT+op_KeyOnly,f,KeyBuff,KeyID);
END FindGT;

PROCEDURE FindGE( VAR f :File; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve4(op_GetGE+op_KeyOnly,f,KeyBuff,KeyID);
END FindGE;

PROCEDURE FindLT( VAR f :File; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve4(op_GetLT+op_KeyOnly,f,KeyBuff,KeyID);
END FindLT;

PROCEDURE FindLE( VAR f :File; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve4(op_GetLE+op_KeyOnly,f,KeyBuff,KeyID);
END FindLE;

PROCEDURE FindFirst( VAR f :File; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve4(op_GetFirst+op_KeyOnly,f,KeyBuff,KeyID);
END FindFirst;

PROCEDURE FindLast( VAR f :File; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve4(op_GetLast+op_KeyOnly,f,KeyBuff,KeyID);
END FindLast;

(* ------------------------------------------------------------------------ *)
(* Btrieve Get functions.                                                   *)
(* ------------------------------------------------------------------------ *)

PROCEDURE GetEQ( VAR f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD);
BEGIN
    BTrieve(op_GetEQ,f,DatBuff,BuffSize,KeyBuff,KeyID);
END GetEQ;

PROCEDURE GetNext( VAR f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve(op_GetNext,f,DatBuff,BuffSize,KeyBuff,KeyID);
END GetNext;

PROCEDURE GetPrev( VAR f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve(op_GetPrev,f,DatBuff,BuffSize,KeyBuff,KeyID);
END GetPrev;

PROCEDURE GetGT( VAR f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD);
BEGIN
    BTrieve(op_GetGT,f,DatBuff,BuffSize,KeyBuff,KeyID);
END GetGT;

PROCEDURE GetGE( VAR f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD);
BEGIN
    BTrieve(op_GetGE,f,DatBuff,BuffSize,KeyBuff,KeyID);
END GetGE;

PROCEDURE GetLT( VAR f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve(op_GetLT,f,DatBuff,BuffSize,KeyBuff,KeyID);
END GetLT;

PROCEDURE GetLE( VAR f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve(op_GetLE,f,DatBuff,BuffSize,KeyBuff,KeyID);
END GetLE;

PROCEDURE GetFirst( VAR f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve(op_GetFirst,f,DatBuff,BuffSize,KeyBuff,KeyID);
END GetFirst;

PROCEDURE GetLast( VAR f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve(op_GetLast,f,DatBuff,BuffSize,KeyBuff,KeyID);
END GetLast;

(* ------------------------------------------------------------------------ *)
(* Btrieve general functions - function number in brackets                  *)
(* ------------------------------------------------------------------------ *)

PROCEDURE Reset();
BEGIN
    BTrieve1(op_Reset);
END Reset;

PROCEDURE Stop();
BEGIN
    BTrieve1(op_Stop);
END Stop;

PROCEDURE SetDirectory( DirName :ARRAY OF CHAR );
    VAR KeyBuff     :KeyType;
        NullControl :File;
BEGIN
    Str.Copy(KeyBuff,DirName);
    BTrieve4(op_SetDir,NullControl,KeyBuff,0);
END SetDirectory;

PROCEDURE GetDir( Drive :SHORTCARD; VAR DirName :ARRAY OF CHAR);
    VAR NullControl :File;
BEGIN
    BTrieve4(op_GetDir,NullControl,DirName,Drive);
END GetDir;

(* ------------------------------------------------------------------------ *)
(* Nuevas funciones de Extracci¢n de datos.                                 *)
(* (c) 1994                                                                 *)
(* Implemetadas por: Saracca, H‚ctor Gustavo.                               *)
(* ------------------------------------------------------------------------ *)

PROCEDURE StepFirst( f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve( op_StepFirst, f, DatBuff, BuffSize, KeyBuff, KeyID );
END StepFirst;

PROCEDURE StepLast( f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve( op_StepLast, f, DatBuff, BuffSize, KeyBuff, KeyID );
END StepLast;

PROCEDURE StepPrev( f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve( op_StepPrev, f, DatBuff, BuffSize, KeyBuff, KeyID );
END StepPrev;

PROCEDURE GetNextExt( f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve( op_GetNextExt, f, DatBuff, BuffSize, KeyBuff, KeyID );
END GetNextExt;

PROCEDURE GetPrevExt( f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve( op_GetPrevExt, f, DatBuff, BuffSize, KeyBuff, KeyID );
END GetPrevExt;

PROCEDURE StepNextExt( f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve( op_StepNextExt, f, DatBuff, BuffSize, KeyBuff, KeyID );
END StepNextExt;

PROCEDURE StepPrevExt( f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve( op_StepPrevExt, f, DatBuff, BuffSize, KeyBuff, KeyID );
END StepPrevExt;

PROCEDURE InsertExt( f :File; VAR DatBuff :ARRAY OF BYTE; VAR BuffSize :CARDINAL; VAR KeyBuff :ARRAY OF BYTE; KeyID :SHORTCARD );
BEGIN
    BTrieve( op_InsertExt, f, DatBuff, BuffSize, KeyBuff, KeyID );
END InsertExt;

(* ------------------------------------------------------------------------ *)
(* Low Level Btrieve Call                                                   *)
(* ------------------------------------------------------------------------ *)

VAR ProcId :CARDINAL;  (* initialize to no process id *)
    Multi  :BOOLEAN;   (* set to true if BMulti is loaded *)
    VSet   :BOOLEAN;   (* set to true if we have checked for BMulti *)

(*%T _XTD*)
TYPE rADDRESS  = LONGCARD;

PROCEDURE RealAlloc( VAR handle :CARDINAL; size :CARDINAL; VAR src :ARRAY OF BYTE) :rADDRESS;
(*
   Needed to cop_y to Real Memory (i.e. addressable from real mode)
*)
    VAR csize :CARDINAL;
BEGIN
    IF size=0 THEN handle:=0; RETURN 0 END;
    handle := TSXLIB.ALLOCLOWSEG(size);
    IF Seg(src)<>0 THEN (* cop_y in *)
       (* protect against buffers that are too short *)
       csize := TSXLIB.GETSEGLIMIT(Seg(src));
       IF (csize>=Ofs(src)) THEN
          DEC(csize,Ofs(src)-1);
          IF (csize=0)OR(csize>size) THEN csize := size END;
          Lib.FastMove(ADR(src),[handle:0],csize);
       END;
     ELSE
       Lib.Fill([handle:0],size,0);
    END; (* if *)
    RETURN TSXLIB.MAKEREALADDR(handle,0);
END RealAlloc;

PROCEDURE RealFree( handle :CARDINAL; size :CARDINAL; VAR dst :ARRAY OF BYTE );
    VAR csize : CARDINAL;
BEGIN
    IF handle=0 THEN RETURN END;
    IF Seg(dst)<>0 THEN (* cop_y out *)
       (* protect against buffers that are too short *)
       csize := TSXLIB.GETSEGLIMIT(Seg(dst));
       IF (csize>=Ofs(dst)) THEN
          DEC(csize,Ofs(dst)-1);
          IF (csize=0)OR(csize>size) THEN csize := size END;
          Lib.FastMove([handle:0],ADR(dst),csize);
       END; (* if *)
    END; (* if *)
    TSXLIB.FREESEG(handle);
END RealFree;

PROCEDURE BTrieve(     op_      :CARDINAL;       (* op_eration          *)
                   VAR pos     :File;            (* Position Block      *)
                   VAR data    :ARRAY OF BYTE;   (* Data Buffer         *)
                   VAR datalen :CARDINAL;        (* Data Size           *)
                   VAR kbuf    :ARRAY OF BYTE;   (* Key Buffer          *)
                       key     :SHORTCARD );     (* Key Number          *)

    CONST VarID         = 06176H; (* id for variable Size records - 'va'*)
          BtrInt        = 07BH;
          Btr2Int       = 02FH;
          BtrOffset     = 00033H;
          MultiFunction = 0AB00H;

    TYPE rBtrParms = RECORD
            UserBufAddr   :rADDRESS;   (* data buffer address        *)
            UserBufLen    :CARDINAL;   (* data buffer Size           *)
            UserCurAddr   :rADDRESS;   (* currency block address     *)
            UserFCBAddr   :rADDRESS;   (* file control block address *)
            UserFunction  :CARDINAL;   (* Btrieve op_eration         *)
            UserKeyAddr   :rADDRESS;   (* key buffer address         *)
            UserKeySize   :SHORTCARD;  (* key buffer Size            *)
            UserKeyNumber :SHORTCARD;  (* key number                 *)
            UserStatAddr  :rADDRESS;   (* return Error address       *)
            xFaceID       :CARDINAL;   (* language interface id      *)
         END;

    VAR Stat     :CARDINAL;         (* Btrieve Error code       *)
        Params   :rBtrParms;        (* Btrieve parameter block  *)
        r        :SYSTEM.Registers;
        DataSel  :CARDINAL;
        PosSel   :CARDINAL;
        KeySel   :CARDINAL;
        StatSel  :CARDINAL;
        ParamSel :CARDINAL;
        rParams  :rADDRESS;
BEGIN
    Lib.Fill(ADR(r),SIZE(r),0);
    r.AX:= 03500H + BtrInt;
    TSXLIB.REALINTR(ADR(r), 21H);
    (* NB using Lib.Intr will return prot mode address *)
    IF r.BX # BtrOffset THEN (* make sure Btrieve is installed *)
       SetError( op_Null, BtrieveAbsent );
    END;
    IF NOT VSet THEN (* if we haven't checked for Multi-User version *)
       r.AX:= 03000H;
       Lib.Intr(r, 021H);
       IF r.AL >= 3 THEN (* DOS version >= 3.0 *)
          VSet:= TRUE;
          r.AX:= MultiFunction;
          Lib.Intr(r, Btr2Int);
          Multi:= r.AL = 4DH   (* ORD('M') *)
        ELSE
          Multi:= FALSE
       END; (* if *)
    END; (* make normal btrieve call *)
    IF datalen > HIGH(data)+1 THEN datalen:= HIGH(data)+1 END;
    WITH Params DO
       UserBufAddr   := RealAlloc(DataSel,datalen,data);  (* set data buffer address *)
       UserBufLen    := datalen;                          (* set Size *)
       UserFCBAddr   := RealAlloc(PosSel,38+90,pos);      (* set FCB address*)
       UserCurAddr   := UserFCBAddr+38;
       UserFunction  := op_;                               (* set Btrieve op_eration code *)
       UserKeyAddr   := RealAlloc(KeySel,255,kbuf);       (* set key buffer address *)
       UserKeySize := 255;
       UserKeyNumber := key;                              (* set key number *)
       UserStatAddr  := RealAlloc(StatSel,SIZE(Stat),Stat);
       xFaceID       := VarID;                            (* set language id *)
    END; (* with *)
    rParams := RealAlloc(ParamSel,SIZE(Params),Params);
    r.DX := CARDINAL(rParams);
    r.DS := CARDINAL(rParams>>16);
    IF NOT Multi THEN  (* MultiUser version not installed *)
       TSXLIB.REALINTR(ADR(r), BtrInt); (* passing real addresses *)
     ELSE
       LOOP
          r.BX:= ProcId;
          IF r.BX # 0 THEN r.AX:= 2 ELSE r.AX:= 1 END;
          INC(r.AX, MultiFunction);
          Lib.Intr(r, Btr2Int);
          IF r.AL = 0 THEN EXIT END;
          r.AX:= 200H;
          TSXLIB.REALINTR(ADR(r), 07FH); (* passing real addresses *)
       END; (* loop_ *)
       IF ProcId = 0 THEN ProcId:= r.BX END
    END; (* if *)
    RealFree(ParamSel,SIZE(Params),Params);
    RealFree(DataSel,datalen,data);
    RealFree(PosSel,38,pos);
    RealFree(KeySel,255,kbuf);
    RealFree(StatSel,SIZE(Stat),Stat);
    datalen:= Params.UserBufLen;
    SetError( op_, ErrorType(Stat) );
END BTrieve;
(*%E*)

(*%F _XTD*)
PROCEDURE BTrieve(     op_      :CARDINAL;       (* op_eration          *)
                   VAR pos     :File;            (* Position Block      *)
                   VAR data    :ARRAY OF BYTE;   (* Data Buffer         *)
                   VAR datalen :CARDINAL;        (* Data Size           *)
                   VAR kbuf    :ARRAY OF BYTE;   (* Key Buffer          *)
                       key     :SHORTCARD );     (* Key Number          *)

    CONST VarID         = 06176H; (* id for variable Size records - 'va'*)
          BtrInt        = 07BH;
          Btr2Int       = 02FH;
          BtrOffset     = 00033H;
          MultiFunction = 0AB00H;

    TYPE BtrParms = RECORD
            UserBufAddr   :ADDRESS;      (* data buffer address        *)
            UserBufLen    :CARDINAL;     (* data buffer Size           *)
            UserCurAddr   :ADDRESS;      (* currency block address     *)
            UserFCBAddr   :ADDRESS;      (* file control block address *)
            UserFunction  :CARDINAL;     (* Btrieve op_eration         *)
            UserKeyAddr   :ADDRESS;      (* key buffer address         *)
            UserKeySize   :SHORTCARD;    (* key buffer Size            *)
            UserKeyNumber :SHORTCARD;    (* key number                 *)
            UserStatAddr  :ADDRESS;      (* return Error address       *)
            xFaceID       :CARDINAL;     (* language interface id      *)
         END;

    VAR Stat  :CARDINAL;         (* Btrieve Error code      *)
        XData :BtrParms;         (* Btrieve parameter block *)
        r     :SYSTEM.Registers;

BEGIN
    r.AX:= 03500H + BtrInt;
    Lib.Intr(r, 021H);
    IF r.BX # BtrOffset THEN (* make sure Btrieve is installed *)
       SetError( op_Null, BtrieveAbsent );
    END; (* if *)
    IF NOT VSet THEN (* if we haven't checked for Multi-User version *)
       r.AX:= 03000H;
       Lib.Intr(r, 021H);
       IF r.AL >= 3 THEN (* DOS version >= 3.0 *)
          VSet:= TRUE;
          r.AX:= MultiFunction;
          Lib.Intr(r, Btr2Int);
          Multi:= r.AL = 4DH   (* ORD('M') *)
        ELSE
          Multi:= FALSE
       END; (* if *)
    END; (* make normal btrieve call *)
    IF datalen > HIGH(data)+1 THEN datalen:= HIGH(data)+1 END;
    WITH XData DO
       UserBufAddr   := ADR(data);        (* set data buffer address *)
       UserBufLen    := datalen;          (* set Size *)
       UserFCBAddr   := ADR(pos);         (* set FCB address*)
       UserCurAddr   := ADR(pos[38]);
       UserFunction  := op_;               (* set Btrieve op_eration code *)
       UserKeyAddr   := ADR(kbuf);        (* set key buffer address *)
       UserKeySize   := 255;
       UserKeyNumber := key;              (* set key number *)
       UserStatAddr  := ADR(Stat);        (* set Error address *)
       xFaceID       := VarID;            (* set language id *)
    END; (* with *)
    r.DX:= SYSTEM.Ofs(XData);
    r.DS:= SYSTEM.Seg(XData);
    IF NOT Multi THEN  (* MultiUser version not installed *)
       Lib.Intr(r, BtrInt)
     ELSE
       LOOP
          r.BX:= ProcId;
          IF r.BX # 0 THEN r.AX:= 2 ELSE r.AX:= 1 END;
          INC(r.AX, MultiFunction);
          Lib.Intr(r, Btr2Int);
          IF r.AL = 0 THEN EXIT END;
          r.AX:= 200H;
          Lib.Intr(r, 07FH)
       END; (* loop_ *)
       IF ProcId = 0 THEN ProcId:= r.BX END
    END; (* if *)
    datalen:= XData.UserBufLen;
    SetError( op_, ErrorType(Stat) );
END BTrieve;
(*%E*)

(* ------------------------------------------------------------------------ *)
(* Errores del BTrieve                                                      *)
(* ------------------------------------------------------------------------ *)

(*# define( write_err => on  ) *)
(*# define( trace_msg => off ) *)

VAR Error :ErrorType;
    Abort :BOOLEAN;

PROCEDURE SetError( op :CARDINAL; err :ErrorType );
BEGIN
    Error := err;
    IF (Error # Ok) & Abort THEN
     (*%T write_err *)
       WrError( op, err );
     (*%E *)
       HALT;
    (*%T trace_msg *)
     ELSE
     (*%T write_err *)
       WrError( op, err );
     (*%E *)
    (*%E *)
    END; (* if *)
END SetError;

PROCEDURE GetError() :ErrorType;
BEGIN
    RETURN Error;
END GetError;

PROCEDURE SetAbort( on :BOOLEAN );
BEGIN
    Abort := on;
END SetAbort;

PROCEDURE ErrorStr( err :ErrorType; VAR str :ARRAY OF CHAR );
    VAR msg :ARRAY [ 0..25 ] OF CHAR;
BEGIN
    CASE err OF
         Ok                     : msg := 'Ok';                         |
         BadOpcode              : msg := 'Bad op-code';                |
         IOError                : msg := 'IO error';                   |
         NoOpen                 : msg := 'No Open';                    |
         KeyNotFound            : msg := 'Key not found';              |
         DuplicateKey           : msg := 'Duplicate key';              |
         InvalidKey             : msg := 'Invalid key';                |
         ChangedKey             : msg := 'Cahnged key';                |
         InvalidPosition        : msg := 'Invalid position';           |
         EndOfFile              : msg := 'End of file';                |
         NonModifiable          : msg := 'Non modifable';              |
         BadFileName            : msg := 'Bad file name';              |
         FileNotFound           : msg := 'File not found';             |
         ExtensionError         : msg := 'Extension error';            |
         PreOpenError           : msg := 'Pre-open error';             |
         PreImageError          : msg := 'Pre-image error';            |
         ExpansionError         : msg := 'Expansion error';            |
         CloseError             : msg := 'Close error';                |
         DiskFull               : msg := 'Disk full';                  |
         Unrecoverable          : msg := 'Unrecoverable';              |
         BtrieveAbsent          : msg := 'BTrieve absent';             |
         KeyBuffSize            : msg := 'Key buffer length';          |
         RecBuffSize            : msg := 'Rec buffer length';          |
         PosBlockSize           : msg := 'Pos block length';           |
         PageSize               : msg := 'Page length';                |
         CreateError            : msg := 'Create error';               |
         KeySegments            : msg := 'Key segments';               |
         KeyPosition            : msg := 'Key position';               |
         RecSize                : msg := 'Rec Size';                   |
         KeySize                : msg := 'Key Size';                   |
         NotBtrieveFile         : msg := 'Not btrieve file';           |
         AlreadyExtended        : msg := 'Already extended';           |
         CantExtend             : msg := "Can't extend";               |
         BadExtendName          : msg := 'Bad extend name';            |
         DirectoryError         : msg := 'Directory error';            |
         TransactError          : msg := 'Transact error';             |
         NestedTransact         : msg := 'Nested transact';            |
         TransactControl        : msg := 'Transact control';           |
         EndAbortError          : msg := 'En abort error';             |
         TransactMaximum        : msg := 'Transact maximun';           |
         TransactFileErr        : msg := 'Transact file error';        |
         AccelFailure           : msg := 'Accel failure';              |
         BadRecAddress          : msg := 'Bad rec address';            |
         NullKeyPath            : msg := 'Null-key path';              |
         BadKeySpecs            : msg := 'Bad-key specs';              |
         AccessDenied           : msg := 'Access denied';              |
         MaxOpenFiles           : msg := 'Max open files';             |
         BadCollation           : msg := 'Bad collation';              |
         CollationKeyType       : msg := 'Collation key-type';         |
         OwnerSet               : msg := 'Owner set';                  |
         InvalidOwner           : msg := 'Invalid owner';              |
         CacheWriteError        : msg := 'Cache write error';          |
         BadBtInterface         : msg := 'Bad interface';              |
         BadVariablePage        : msg := 'Bad variable page';          |
         AutoIncErr             : msg := 'Auto-inc error';             |
         IncompleteIndex        : msg := 'Incomplete index';           |
         EMSErr                 : msg := 'EMS error';                  |
         CompressBufTooShort    : msg := 'Compress buffer too short';  |
         FileAlreadyExists      : msg := 'File already exists';        |
         Err33, Err60..Err79    : msg := 'errors: 33, 60..79';         |
         AccessConflict         : msg := 'Access conflict';            |
         LockFailed             : msg := 'Lock failed';                |
         LostPosition           : msg := 'Lost position';              |
         UnreadTransact         : msg := 'Unread transact';            |
         RecInUse               : msg := 'Rec in use';                 |
         FileInUse              : msg := 'File in use';                |
         FileTableFull          : msg := 'File table full';            |
         HandleTableFull        : msg := 'Handle table full';          |
         OpenModeError          : msg := 'Open mode error';            |
         DeviceNameError        : msg := 'Device name error';          |
         DeviceTableFull        : msg := 'Device table full';          |
         ServerError            : msg := 'Server error';               |
         TransactTabFull        : msg := 'Transact table full';        |
         IncompatibleLockType   : msg := 'Incompatible lock type';     |
         PermissionErr          : msg := 'Permision error';            |
         SessionNoLongerValid   : msg := 'Session no longer valid';    |
         ComEnvErr              : msg := 'Com. env. error';            |
         DataBufTooSmall        : msg := 'Data buffer too small';      |
         InternalTransactionErr : msg := 'Internal transaction error'; |
         DemoVersion            : msg := 'Demo version';               |
       ELSE
         msg := 'error desconocido';
     END; (* case *)
     Str.Copy( str, msg );
END ErrorStr;

PROCEDURE OpStr( op :CARDINAL; VAR str :ARRAY OF CHAR );
    VAR msg :ARRAY [ 0..20 ] OF CHAR;
BEGIN
    CASE op OF
       op_Open          : msg := 'Open';              |
       op_Close         : msg := 'Close';             |
       op_Insert        : msg := 'Insert';            |
       op_Update        : msg := 'Update';            |
       op_Delete        : msg := 'Delete';            |
       op_GetEQ         : msg := 'Get Equal';         |
       op_GetNext       : msg := 'Get Next';          |
       op_GetPrev       : msg := 'Get Prev';          |
       op_GetGT         : msg := 'Get Great-That';    |
       op_GetGE         : msg := 'Get Great-Equal';   |
       op_GetLT         : msg := 'Get Less-That';     |
       op_GetLE         : msg := 'Get Less-Equal';    |
       op_GetFirst      : msg := 'Get First';         |
       op_GetLast       : msg := 'Get Last';          |
       op_Create        : msg := 'Create';            |
       op_Stat          : msg := 'Status';            |
       op_Extend        : msg := 'Extend file';       |
       op_SetDir        : msg := 'Set Dir';           |
       op_GetDir        : msg := 'Get Dir';           |
       op_BeginTrans    : msg := 'Begin Transaction'; |
       op_EndTrans      : msg := 'End Transaction';   |
       op_AbortTrans    : msg := 'Abort Transaction'; |
       op_GetPos        : msg := 'Get Pos';           |
       op_GetDirect     : msg := 'Get Direct';        |
       op_StepDirect    : msg := 'Step Direct';       |
       op_Stop          : msg := 'Stop';              |
       op_Version       : msg := 'Version';           |
       op_Unlock        : msg := 'Un-lock';           |
       op_Reset         : msg := 'Reset';             |
       op_SetOwner      : msg := 'Set Owner';         |
       op_ClearOwner    : msg := 'Clear Owner';       |
       op_CreateSuppIdx : msg := 'Create Supp Idx';   |
       op_DropSuppIdx   : msg := 'Drop Supp Idx';     |
       op_StepFirst     : msg := 'Step First';        |
       op_StepLast      : msg := 'Step Last';         |
       op_StepPrev      : msg := 'Step Prev';         |
       op_GetNextExt    : msg := 'GetNext Ext';       |
       op_GetPrevExt    : msg := 'Get Prev Ext';      |
       op_StepNextExt   : msg := 'Step Next Ext';     |
       op_StepPrevExt   : msg := 'Step Prev Ext';     |
       op_InsertExt     : msg := 'Insert Ext';        |
       op_KeyOnly       : msg := 'Key Only';          |
       op_Null          : msg := 'Null-operation';    |
     ELSE
       msg := 'operaci¢n desconocida';
    END; (* case *)
    Str.Copy( str, msg );
END OpStr;

PROCEDURE WrError( op :CARDINAL; err :ErrorType );
    VAR msg :ARRAY [ 0..25 ] OF CHAR;
BEGIN
    Window.GotoXY(1,1);
    Win.WrStr( 'BTrieve error: ');
    Win.WrStr( '"' );
    ErrorStr( err, msg );
    Win.WrStr( msg );
    Win.WrStr( '" #:' );
    Win.WrCard( ORD(err), 3 );
    IF op # op_Null THEN
       Win.WrStr( ', in operation: "' );
       OpStr( op, msg );
       Win.WrStr( msg );
       Win.WrStr( '"' );
    END; (* if *)
    Win.WrStr( '.' );
    Win.WrLn();
END WrError;

PROCEDURE GetVer( VAR str :ARRAY OF CHAR );

   TYPE VerType = RECORD
           Number  :CARDINAL;
           Rev     :INTEGER;
           Product :CHAR;
        END; (* VerType *)

   VAR NullControl :File;
       NullKey     :KeyType;
       aux_str     :ARRAY [ 0..1 ] OF CHAR;
       ver         :VerType;
       size        :CARDINAL;
       ok          :BOOLEAN;
BEGIN
    size := SIZE(VerType);
    BTrieve( op_Version, NullControl, ver, size, NullKey, 0 );
    Str.CardToStr( LONGCARD(ver.Number), aux_str, 10, ok );
    Str.Concat( str, aux_str, '.' );
    Str.IntToStr(  LONGINT(ver.Rev),    aux_str, 10, ok );
    Str.Append( str, aux_str );
    Str.Append( str, ver.Product );
END GetVer;

(* ------------------------------------------------------------------------ *)
(* Procedimientos varios de ayuda de BTrieve.                               *)
(* ------------------------------------------------------------------------ *)

PROCEDURE Lng2pos( i :LONGCARD ) :PosType;
    VAR pos :PosType;
BEGIN
    pos[0] := CARDINAL( i DIV 65536 );
    pos[1] := CARDINAL( i MOD 65536 );
    RETURN pos;
END Lng2pos;

PROCEDURE Pos2lng( p :PosType ) :LONGCARD;
BEGIN
    RETURN LONGCARD(p[0]) * 65536 + LONGCARD(p[1]);
END Pos2lng;

TYPE BFile = POINTER TO RECORD
        file       :File;
        num_segs   :CARDINAL;
        spec       :FileSpec;
        spec_size  :CARDINAL;
        index_buff :KeyType;
        cur_index  :SHORTCARD;  (* KeyRange *)
     END; (* BTFile *)

PROCEDURE CountSegs( fdesc :FileSpec ) :CARDINAL;

    VAR NumSegs :CARDINAL;
        i       :CARDINAL;
        j       :KeyRange;

    PROCEDURE Count();
    BEGIN
        WITH fdesc DO
           REPEAT
                IF (KeyArray[j].KeyFlags & Segmented) = Segmented THEN
                   IF (KeyArray[j].KeyFlags & AltCol) = AltCol THEN
                      (* HasAltCol := TRUE; *)
                   END; (* if *)
                   INC(NumSegs);
                   INC(j);
                 ELSE
                   IF (KeyArray[j].KeyFlags & AltCol) = AltCol THEN
                      (* HasAltCol := TRUE; *)
                   END; (* if *)
                   INC(i);
                   INC(j);
               END; (* if *)
           UNTIL (KeyArray[j-1].KeyFlags & Segmented) # Segmented;
        END; (* with *)
    END Count;

BEGIN
    i := 1;
    j := 0;
    NumSegs := fdesc.NumKeys;
    WHILE i <= fdesc.NumKeys DO
        Count();
    END; (* while *)
    RETURN NumSegs;
END CountSegs;

PROCEDURE InitSpecs(     rec_size  :CARDINAL;
                         file_flgs :CARDINAL;
                         page_size :CARDINAL;
                         pre_alloc :CARDINAL;
                         keys      :ARRAY OF KeySpec;
                     VAR num_segs  :CARDINAL ) :FileSpec;
    VAR spec :FileSpec;
        i    :KeyRange;
BEGIN
    WITH spec DO
       RecSize     := rec_size;      (* only serve to reserve space for *)
       PageSize    := page_size;     (* the buffer.                     *)
       NumKeys     := HIGH(keys)+1;
       NumRecs     := 0;
       FileFlags   := file_flgs;
       Reserved[0] := CHR(0);
       Reserved[1] := CHR(0);
       PreAlloc    := pre_alloc;
       Lib.Fill( ADR(KeyArray), SIZE(KeyArray), 0 );
       FOR i := 0 TO HIGH(keys) DO
           KeyArray[i] := keys[i];
       END; (* next *)
       num_segs := CountSegs( spec );
    END; (* with *)
    RETURN spec;
END InitSpecs;

PROCEDURE BCreate( name :ARRAY OF CHAR; specs :FileSpec ) :BFile;
    VAR bfile :BFile;
BEGIN
    specs.Reserved[0] := CHR(0);
    specs.Reserved[1] := CHR(0);
    NEW(bfile);
    bfile^.num_segs  := CountSegs( specs );
    bfile^.spec      := specs;
    bfile^.spec_size := 16 + (bfile^.num_segs * 16);
    bfile^.file      := Create( name, bfile^.spec, bfile^.spec_size );
    bfile^.file      := Open( name, '', BTRV.Normal );
    RETURN bfile;
END BCreate;

TYPE MaxSpecType = ARRAY [ 0..664 ] OF BYTE;
     (* 665 = 16 for filespec + 384 for max key specs *)

PROCEDURE BOpen( name :ARRAY OF CHAR; open_mode :SHORTCARD ) :BFile;

    VAR FileBufLen :CARDINAL; (* + 265 for an alternate collating sequence. *)
        KeyBufLen  :CARDINAL; (* Max of 24 keys * 16 bytes per key spec.    *)
        bfile      :BFile;
        SpecBuff   :MaxSpecType;
BEGIN
    NEW( bfile );
    FileBufLen := 665;     (* + 265 for an alternate collating sequence.    *)
    KeyBufLen  := 384;     (* Max of 24 keys * 16 bytes per key spec.       *)
    bfile^.file := Open( name, '', open_mode );
    IF GetError() = Ok THEN
       Status( bfile^.file, SpecBuff, FileBufLen, KeyBufLen );
       IF GetError() = Ok THEN
          bfile^.spec      := FileSpec(SpecBuff);
          bfile^.spec_size := FileBufLen;
          bfile^.num_segs  := CountSegs(bfile^.spec);
        ELSE
          Close( bfile^.file );
       END; (* if *)
    END; (* if *)
    RETURN bfile;
END BOpen;

PROCEDURE BClose( VAR f :BFile );
BEGIN
    Close( f^.file );
    DISPOSE( f );
END BClose;

PROCEDURE SUpdate( VAR f :BFile );
    VAR SpecBuff   :MaxSpecType;
        FileBufLen :CARDINAL; (* + 265 for an alternate collating sequence. *)
        KeyBufLen  :CARDINAL; (* Max of 24 keys * 16 bytes per key spec.    *)
BEGIN
    FileBufLen := 665;     (* + 265 for an alternate collating sequence.    *)
    KeyBufLen  := 384;     (* Max of 24 keys * 16 bytes per key spec.       *)
    Status( f^.file, SpecBuff, FileBufLen, KeyBufLen );
    IF GetError() = Ok THEN
       f^.spec := FileSpec(SpecBuff);
    END; (* if *)
END SUpdate;

PROCEDURE SetIDX( VAR f :BFile; index :KeyRange );
BEGIN
    f^.cur_index := SHORTCARD(index);
END SetIDX;

PROCEDURE GetSpec( VAR f :BFile ) :FileSpec;
BEGIN
    RETURN f^.spec;
END GetSpec;

PROCEDURE BRecords( VAR f :BFile ) :LONGCARD;
BEGIN
    BTRV.BeginTransaction();
    BTRV.SUpdate( f );
    BTRV.EndTransaction();
    RETURN f^.spec.NumRecs;
END BRecords;

PROCEDURE BGetPos( VAR f :BFile ) :LONGCARD;
    VAR pos :PosType;
BEGIN
    GetPos( f^.file, pos );
    RETURN Pos2lng(pos);
END BGetPos;

PROCEDURE BAdd( VAR f :BFile; VAR d :ARRAY OF BYTE ) :LONGCARD;
BEGIN
    Insert( f^.file, d, f^.spec.RecSize, f^.index_buff, f^.cur_index );
    RETURN BGetPos( f );
END BAdd;

PROCEDURE BGet( VAR f :BFile; i :LONGCARD; VAR d :ARRAY OF BYTE );
    VAR pos :BTRV.PosType;
BEGIN
    pos := Lng2pos(i);
    Lib.Move( ADR(pos), ADR(d), SIZE(pos) );
    BTRV.GetDirect( f^.file, d, f^.spec.RecSize, f^.index_buff, 0 );
END BGet;

PROCEDURE BPut( VAR f :BFile; i :LONGCARD; VAR d :ARRAY OF BYTE );
    VAR d1 :ARRAY [ 0..MaxRecSize ] OF BYTE;
BEGIN
    BTRV.BGet( f, i, d1 );
    BTRV.Update( f^.file, d, f^.spec.RecSize, f^.index_buff, f^.cur_index );
END BPut;

PROCEDURE BDel( VAR f :BFile; i :LONGCARD );
    VAR d1 :ARRAY [ 0..MaxRecSize ] OF BYTE;
BEGIN
    BGet( f, i, d1 );
    Delete( f^.file, f^.cur_index );
END BDel;

PROCEDURE BSrch( VAR f :BFile;     srch :ARRAY OF BYTE;
                               VAR pos  :LONGCARD;
                               VAR d    :ARRAY OF BYTE ) :BOOLEAN;
    VAR ok :BOOLEAN;
BEGIN
    BTRV.GetLE( f^.file, d, f^.spec.RecSize, srch, f^.cur_index );
    ok := BTRV.GetError() = BTRV.Ok;
    IF ok THEN
       pos := BGetPos( f );
    END; (* if *)
    RETURN ok;
END BSrch;

PROCEDURE BGetFirst( VAR f :BFile; VAR pos :LONGCARD; VAR d :ARRAY OF BYTE );
BEGIN
    BTRV.GetFirst( f^.file, d, f^.spec.RecSize, f^.index_buff, f^.cur_index );
    pos := BGetPos( f );
END BGetFirst;

PROCEDURE BGetNext( VAR f :BFile; VAR pos :LONGCARD; VAR d :ARRAY OF BYTE );
BEGIN
    BTRV.GetNext( f^.file, d, f^.spec.RecSize, f^.index_buff, f^.cur_index );
    pos := BGetPos( f );
END BGetNext;

PROCEDURE CopyFile( source, target :ARRAY OF CHAR; mode :CopyMode );
(*
   Programmer is responsible for assuring that 'CurrentFile' exists and can be
   opened.  Function will overwrite any existing file with 'NewFile' name.
   The integer returned here can be meaningless if the current file does not
   exist or is not opened properly.  This function is as streamlined as
   possible, but puts RESPONSIBILITY on the programmer.

   It is entirely possible that this clone function will NOT return a byte for
   byte matching file, if cloning an 'empty' Btrieve file.  This would be due
   to the inability to determine the number of pages pre-allocated when a file
   was created, if preallocation had been used.  The Btrieve Stat call uses
   the 'Preallocate # of pages' bytes to return the number of unused pages!!
   Thus, the CloneFile function clears the Preallocation bit in the FileFlags
   before creating the new file.
*)

TYPE DBufferType = ARRAY [ 1..MaxRecSize ] OF BYTE;
     KBufferType = ARRAY [ 1..MaxKeySize ] OF BYTE;

VAR HasSuppIdx        :BOOLEAN;
    NumberSuppSegs    :CARDINAL;
    NumberSuppIdx     :CARDINAL;
    NewNumKeys        :CARDINAL;
    NewOffset         :CARDINAL;
    DBuffOffset       :CARDINAL;
    Counter, Counter1 :CARDINAL;

    CurFile           :BFile;
    NewFile           :BFile;

    NewDBuffer        :DBufferType;
    NewDBufferLen     :CARDINAL;
    NewKBuffer        :KBufferType;

    SuppIdxList       :ARRAY [ 0..23 ] OF BOOLEAN;
    SuppIdx           :ARRAY [ 0..23 ] OF KeySpec;
    (* will hold list of indexes *)

BEGIN
    HasSuppIdx     := FALSE;
    NumberSuppSegs :=  0;
    NumberSuppIdx  :=  0;
    NewOffset      := 17;
    DBuffOffset    :=  1;

    CurFile := BOpen( source, ReadOnly );

    (* establish new filename *)

    (*
       Clear the PreAllocate file flag bit if it had been set in CurrentBFile.
    *)
    CurFile^.spec.FileFlags := CurFile^.spec.FileFlags & 0FDH;
(*    CurFileSpecs.UnusedPgs := 0;*)

        (* If preallocate file flag was set, the *)
        (* cloned file will have no pages pre-   *)
        (* allocated...NO way to get the         *)
        (* original # of pre-allocated pages!    *)

    NewFile^.spec_size := CurFile^.spec_size;      (* Initialize...may reduce *)
    NewNumKeys  := CurFile^.spec.NumKeys;        (* both of these later.    *)
    Lib.Fill( ADR(SuppIdxList), SIZE(SuppIdxList), FALSE );

    Lib.Fill( ADR(NewFile^.spec), SIZE(NewFile^.spec), 0 ); (* initialize spec w/zeros *)
    Lib.Move( ADR(CurFile^.spec), ADR(NewFile^.spec), 16 ); (* get filespecs, not keys *)

    (*
       Determine if there are any supplemental indexes in source file.  If so,
       set indicator HasSuppIdx to true, set boolean in an array to true, and
       get a count of number of supplemental indexes, and count of total number
       of supplemental index segments.
    *)
    WITH CurFile^.spec DO
       FOR Counter := 1 TO CurFile^.num_segs DO
          WITH KeyArray[Counter-1] DO
             IF (KeyFlags AND Supplemental) = Supplemental THEN
                HasSuppIdx := TRUE;
                SuppIdxList[Counter-1] := TRUE;
                Lib.Move(ADR(KeyArray[Counter-1]),
                         ADR(SuppIdx[NumberSuppSegs]), 16);
                SuppIdx[NumberSuppSegs].KeyFlags :=     (*Zero supplemental bit*)
                   SuppIdx[NumberSuppSegs].KeyFlags & 0FF7FH;
                INC(NumberSuppSegs);      (*inc count of supplemental segments.*)
                IF (KeyArray[Counter-1].KeyFlags & Segmented) # Segmented
                  THEN
                    INC(NumberSuppIdx);    (*inc count of supplemental indexes.*)
                END; (* if *)
             END; (* if *)
          END; (* with *)
       END; (* next *)
    END; (* with *)
    IF ((mode = Drop) OR (mode = Retain)) AND HasSuppIdx THEN
       Counter1 := 0;
       FOR Counter := 1 TO CurFile^.spec.NumKeys DO
           IF SuppIdxList[Counter1] THEN
              DEC(NewNumKeys);
           END; (* if *)
           REPEAT
               IF (SuppIdxList[Counter1] = FALSE) THEN
                  Lib.Move( ADR(CurFile^.spec.KeyArray[Counter1]),
                            ADR(NewFile^.spec.KeyArray[Counter1]), 16);
                  INC(NewOffset, 16);
                ELSE
                  DEC(NewFile^.spec_size, 16);
               END; (* if *)
               INC(Counter1);
           UNTIL (CurFile^.spec.KeyArray[Counter1-1].KeyFlags & Segmented) <> Segmented;
       END; (* next *)
       NewFile^.spec.NumKeys := NewNumKeys;
(*
       IF (CurFilerentBFile^.HasAltCol) THEN
          Lib.Move( ADR(CurrentSpecs.Entire[17 + (CurrentBFile^.NumSegs * 16)]),
                    ADR(NewFileSpec^.Entire[NewOffset]), 265);
       END; (* if *)
*)
       (*
          Next line executed if source file has supplemental indexes, whether
          they are to be dropped or retained.
       *)
       NewFile := BCreate( target, NewFile^.spec );
    END; (* if *)

    (*
       If retaining the supplemental indexes, then at this point we're ready to
       add them to the newly created file.
    *)

    IF (mode = Retain) & HasSuppIdx THEN
       NewFile := BCreate( target, NewFile^.spec );
       Counter1 := 0;
       FOR Counter := 1 TO NumberSuppIdx DO
           REPEAT
               Lib.Move( ADR(SuppIdx[Counter1]), ADR(NewDBuffer[DBuffOffset]), 16);
               INC(DBuffOffset, 16);
               INC(Counter1);
           UNTIL ((SuppIdx[Counter1-1].KeyFlags) & Segmented) # Segmented;
           NewDBufferLen := Counter1 * 16;
           BTrieve( op_CreateSuppIdx, NewFile^.file, NewDBuffer, NewDBufferLen, NewKBuffer, Zero );
           Lib.Fill( ADR(NewDBuffer), SIZE(NewDBuffer), 0 );
           DBuffOffset := 1;
       END; (* next *)
       BClose( NewFile );
    END; (* if *)

    (*
       WARNING!! If user program specified 'None' and there actually ARE one or
       more supplemental indexes in the source file, they WILL be retained in
       the target file, as permanent indexes!
    *)

    IF (mode = None) OR ((mode = Retain) AND (~ HasSuppIdx)) OR
       ((mode = Drop) & (~ HasSuppIdx))
     THEN
        NewFile := BCreate( target, NewFile^.spec );
    END; (* if *)

    BClose( CurFile );
END CopyFile;

PROCEDURE InitLogic( FieldType  :BYTE;
                     FieldLen   :CARDINAL;
                     Offset     :CARDINAL;
                     CompCode   :BYTE;
                     Expression :BYTE;
                     BoolVal    :BOOLEAN;
                     Value      :CharArrayType ) :LogicType;

    VAR Logic :LogicType;
BEGIN
    Logic.FieldType  := FieldType;
    Logic.FieldLen   := FieldLen;
    Logic.Offset     := Offset;
    Logic.CompCode   := CompCode;
    Logic.Expression := Expression;
    Logic.FieldComp  := BoolVal;
    Logic.Value      := Value;
    RETURN Logic;
END InitLogic;

BEGIN (* main *)
    VSet   := FALSE;
    Multi  := FALSE;
    ProcId := 0;
    SetAbort( TRUE );
    SetError( op_Null, Ok );
    Reset();
    IF GetError() = BtrieveAbsent THEN
       Lib.FatalError( 'Please load BTrieve before running this program.' );
    END; (* if *)
END BTRV.
