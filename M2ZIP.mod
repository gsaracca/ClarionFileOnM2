IMPLEMENTATION MODULE M2ZIP;

IMPORT PPack, Lib;

(* ------------------------------------------------------------------------ *)
(* Implementation in M2 de KH Compression.                                  *)
(* ------------------------------------------------------------------------ *)

TYPE HashTable = ARRAY [ 0..4095 ] OF INTEGER;

PROCEDURE XOR( a, b :CARDINAL ) :CARDINAL;
BEGIN
  RETURN CARDINAL(BITSET(a) / BITSET(b));
END XOR;

PROCEDURE HI( a :CARDINAL ) :SHORTCARD;
BEGIN
    RETURN SHORTCARD((a >> 8) AND 0FFH);
END HI;

PROCEDURE LO( a :CARDINAL ) :SHORTCARD;
BEGIN
    RETURN SHORTCARD(a AND 0FFH);
END LO;

PROCEDURE GetMatch(    Source     :BufferPtr;
                       X          :BufferIndex;
                       SourceSize :BufferSize;
                   VAR Hash       :HashTable;
                   VAR Size       :CARDINAL;
                   VAR Pos        :BufferIndex ) :BOOLEAN;
    VAR HashValue :CARDINAL;
        GetMatch  :BOOLEAN;
BEGIN
     HashValue := (40543*(
                  XOR(((
                  XOR( (CARDINAL(Source^[X]) << 4), CARDINAL(Source^[X+1]) )) << 4),
                  CARDINAL(Source^[X+2]))) >> 4) AND 0FFFH;
     GetMatch := FALSE;
     IF (Hash[HashValue] <> -1) AND (INTEGER(X)-Hash[HashValue] < 4096) THEN
        Pos := Hash[HashValue];
        Size := 0;
        WHILE ((Size < 18) AND (Source^[X+Size] = Source^[Pos+Size])
              AND (X+Size < SourceSize)) DO
           Size:=Size+1;
        END; (* while *)
        GetMatch := (Size >= 3)
     END; (* if *)
     Hash[HashValue] := X;
     RETURN GetMatch;
END GetMatch;

CONST FLAG_Compress = 40H;
      FLAG_Copied   = 80H;

PROCEDURE KHZIP( src, trg :BufferPtr; src_size :BufferSize ) :BufferSize;
    VAR Hash         :HashTable;
        Key,Bit      :CARDINAL;
        Command,Size :CARDINAL;
        X,Y,Z,Pos    :BufferIndex;
BEGIN
     FOR Key := 0 TO 4095 DO Hash[Key] := -1; END; (* next *)
     trg^[0] := FLAG_Compress;
     X       := 0;
     Y       := 3;
     Z       := 1;
     Bit     := 0;
     Command := 0;
     WHILE (X < src_size) AND (Y <= src_size) DO
        IF (Bit > 15) THEN
           trg^[Z]   := HI(Command);
           trg^[Z+1] := LO(Command);
           Z         := Y;
           Bit       := 0;
           Y         := Y + 2
        END; (* if *)
        Size := 1;
        WHILE ((src^[X] = src^[X+Size]) & (Size < 0FFFH) & (X+Size < src_size)) DO
            Size := Size + 1;
        END; (* while *)
        IF (Size >= 16) THEN
           trg^[Y]   := 0;
           trg^[Y+1] := HI(Size-16);
           trg^[Y+2] := LO(Size-16);
           trg^[Y+3] := src^[X];
           Y         := Y+4;
           X         := X+Size;
           Command := (Command << 1) + 1;
        ELSE
           IF (GetMatch(src,X,src_size,Hash,Size,Pos)) THEN
              Key := ((X-Pos) << 4) + (Size-3);
              trg^[Y]   := HI(Key);
              trg^[Y+1] := LO(Key);
              Y         := Y+2;
              X         := X+Size;
              Command   := (Command << 1) + 1
            ELSE
              trg^[Y] := src^[X];
              Y       := Y+1;
              X       := X+1;
              Command := Command << 1;
           END; (* if *)
           Bit:=Bit+1;
        END; (* if *)
     END; (* while *)
     Command   := Command << (16-Bit);
     trg^[Z]   := HI(Command);
     trg^[Z+1] := LO(Command);
     IF (Y > src_size) THEN
        Lib.Move( ADR(src^[0]), ADR(trg^[1]), src_size );
        trg^[0] := FLAG_Copied;
        Y       := src_size + 1;
     END; (* if *)
     RETURN Y;
END KHZIP;

PROCEDURE KHUNZIP( src, trg :BufferPtr; src_size :BufferSize ) :BufferSize;
    VAR X,Y,Pos :BufferIndex;
        Command :CARDINAL;
        Size,K  :CARDINAL;
        Bit     :BYTE;
BEGIN
     IF (src^[0] = FLAG_Copied) THEN
        FOR Y := 1 TO src_size-1 DO
            trg^[Y-1] := src^[Y]
        END; (* next *)
      ELSE
        Y       := 0;
        X       := 3;
        Command := CARDINAL((src^[1] << 8)) + CARDINAL(src^[2]);
        Bit     := 16;
        WHILE (X < src_size) DO
           IF (Bit = BYTE(0)) THEN
              Command := CARDINAL(src^[X] << 8) + CARDINAL(src^[X+1]);
              Bit     := 16;
              X       := X+2;
           END; (* if *)
           IF ((Command AND 8000H) = 0) THEN
              trg^[Y] := src^[X];
              X       := X+1;
              Y       := Y+1
            ELSE
              Pos := CARDINAL(src^[X] << 4) + CARDINAL(src^[X+1] >> 4);
              IF (Pos = 0) THEN
                 Size := CARDINAL(src^[X+1] << 8) + CARDINAL(src^[X+2]) + 15;
                 FOR K := 0 TO Size DO
                     trg^[Y+K] := src^[X+3];
                 END; (* next *)
                 X := X+4;
                 Y := Y+(Size+1);
               ELSE
                 Size := (CARDINAL(src^[X+1]) AND 0FH) + 2;
                 FOR K := 0 TO Size DO
                     trg^[Y+K] := trg^[Y-Pos+K];
                 END; (* next *)
                 X := X+2;
                 Y := Y+(Size+1);
              END; (* if *)
           END; (* if *)
           Command := Command << 1;
           Bit     := Bit - BYTE(1);
        END; (* while *)
     END; (* if *)
     RETURN Y;
END KHUNZIP;

(* ------------------------------------------------------------------------ *)
(* Interface to M2 Compresion                                               *)
(* ------------------------------------------------------------------------ *)

PROCEDURE Zip( src, trg :BufferPtr; src_size :BufferSize ) :BufferSize;
BEGIN
    RETURN PPack.Packer( src_size, src, trg );
END Zip;

PROCEDURE UnZip( src, trg :BufferPtr; src_size :BufferSize ) :BufferSize;
    VAR size :CARDINAL;
BEGIN
    size := PPack.UnpackedSize( src_size, src );
    PPack.Unpacker( src_size, src, trg );
    RETURN size;
END UnZip;

END M2ZIP.
