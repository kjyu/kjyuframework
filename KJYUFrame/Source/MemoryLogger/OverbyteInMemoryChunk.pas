{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  Author:       François PIETTE
  Creation:     Feb 09, 2014
  Description:  Class to represent a buffer for TInMemoryLogger. Buffers
                are organized as a simply linked list.
  Version:      1.00 History:
  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
unit OverbyteInMemoryChunk;

interface

uses SysUtils;

type
  EInMemoryException = class(Exception);
  EInMemoryRangeException = class(EInMemoryException);
  EInMemoryNoBufferException = class(EInMemoryException);
  TInMemoryChunk = class;
  TInMemoryChunkAcquireEvent = procedure(Sender: TObject;
    var Chunk: TInMemoryChunk) of object; // Store data in ASCII

  TInMemoryChunk = class(TObject)
  strict private
    FBuffer: TBytes;
    FBufferSize: Integer;
    FBufferNext: TInMemoryChunk;
    FWriteCount: Integer;
    FOnAcquireChunk: TInMemoryChunkAcquireEvent;
    function AcquireChunk: TInMemoryChunk;
  public
    constructor Create(Size: Integer);
    destructor Destroy; override;
    function Write(const Msg: String; var ByteCount: Integer): TInMemoryChunk;
    property Buffer: TBytes read FBuffer write FBuffer;
    property BufferNext: TInMemoryChunk read FBufferNext write FBufferNext;
    property BufferSize: Integer read FBufferSize write FBufferSize;
    property WriteCount: Integer read FWriteCount write FWriteCount;
    property OnAcquireChunk: TInMemoryChunkAcquireEvent read FOnAcquireChunk
      write FOnAcquireChunk;
  end;

const
  ERROR_MSG_NO_BUFFER = 'Log failed. No buffer available';
  ERROR_MSG_SIZE_TO_LOW = 'Create buffer failed. Min size is 16';

implementation

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
{ TInMemoryChunk }
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
constructor TInMemoryChunk.Create(Size: Integer);
begin
  if Size < 16 then
    raise EInMemoryRangeException.Create(ERROR_MSG_SIZE_TO_LOW);
  FWriteCount := 0;
  FBufferNext := nil;
  FBufferSize := Size;
  SetLength(FBuffer, FBufferSize);
end;
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

destructor TInMemoryChunk.Destroy;
begin
  inherited Destroy;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
function TInMemoryChunk.AcquireChunk: TInMemoryChunk;
begin
  Result := nil;
  if Assigned(FOnAcquireChunk) then
    FOnAcquireChunk(Self, Result);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
// Write a msg into the buffer, allocating a new one if required
// Returns the last buffer used (The current one or the new allocated)
function TInMemoryChunk.Write(const Msg: String; var ByteCount: Integer)
  : TInMemoryChunk;
var
  I: Integer;
  Len: Integer;
  AvailBytes: Integer;
  NewBuffer: TInMemoryChunk;
begin
  Result := Self;
  Len := Length(Msg);
  if Len <= 0 then
    Exit;
  AvailBytes := Result.BufferSize - Result.WriteCount;
  I := Low(Msg);
  while I <= High(Msg) do
  begin
    // Simple and incorrect unicode to ascii conversion
    Result.Buffer[Result.WriteCount] := Ord(Msg[I]);
    Result.WriteCount := Result.WriteCount + 1;
    Inc(I);
    Inc(ByteCount);
    Dec(AvailBytes);
    if AvailBytes <= 0 then
    begin
      // No more room in current buffer, allocate new one
      NewBuffer := AcquireChunk;
      // Get a free chunk
      if not Assigned(NewBuffer) then
      begin
        Result := nil;
        Exit;
      end;
      Result.BufferNext := NewBuffer;
      Result := NewBuffer;
      Result.BufferNext := nil;
      Result.WriteCount := 0;
      AvailBytes := Result.BufferSize;
    end;
  end;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
end.
