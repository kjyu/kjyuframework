{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
  Author:       François PIETTE
  Creation:     Feb 09, 2014
  Description:  Fast multithread safe in memory logging
  Version:      1.00
  History:
  * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
unit OverbyteInMemoryLogger;

interface

uses Windows, ShlObj, Types, Classes, SysUtils, SyncObjs, Generics.Collections,
  OverbyteInMemoryChunk;

type
  TInMemoryLoggerOption = (
    imloAddCRLF, // Add a CRLF at end of each Log()
    imloAddDate, // Add a date in front of each log()
    imloAddTime, // Add a time in front of each log()
    imloAddTimeMSec, // Add millisecond to time
    imloFlushOnDestroy // Flush to file when destroyed
    );
  TInMemoryLoggerOptions = set of TInMemoryLoggerOption;

  TInMemoryLogger = class(TComponent)
  private
  protected
    FCritSectBuffer: TCriticalSection; // To protect buffer access
    FCritSectFile: TCriticalSection; // To protect file write access
    FBufferHead: TInMemoryChunk;
    FBufferCurrent: TInMemoryChunk;
    FBufferSize: Integer;
    FOptions: TInMemoryLoggerOptions;
    FFileName: String;
    FBufferAvail: TInMemoryChunk;
    FByteCount: Integer;
    function AcquireChunk: TInMemoryChunk;
    procedure ReleaseChunk(Chunk: TInMemoryChunk);
    procedure AcquireChunkHandler(Sender: TObject; var Chunk: TInMemoryChunk);
    function GetByteCount: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Log(const Msg: String);
    procedure FlushToFile;
    procedure SetDefaultFileName;
  published
    property BufferSize: Integer read FBufferSize write FBufferSize;
    property Options: TInMemoryLoggerOptions read FOptions write FOptions;
    property FileName: String read FFileName write FFileName;
    property ByteCount: Integer read GetByteCount;
  end;

var
  CompanyFolder: String = 'OverByte';

implementation

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

{ TInMemoryLogger }
{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
constructor TInMemoryLogger.Create(AOwner: TComponent);
begin
  FCritSectBuffer := TCriticalSection.Create;
  FCritSectFile := TCriticalSection.Create;
  FBufferSize := 4096;
  // Default buffer size;
  FOptions := [imloAddCRLF, imloFlushOnDestroy, imloAddDate, imloAddTime,
    imloAddTimeMSec];
  inherited Create(AOwner);
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
destructor TInMemoryLogger.Destroy;
var
  Chunk1: TInMemoryChunk;
  Chunk2: TInMemoryChunk;
begin
  if imloFlushOnDestroy in FOptions then
    FlushToFile;
  if Assigned(FCritSectBuffer) then
    FCritSectBuffer.Acquire;
  try
    // Free currently active buffers
    Chunk1 := FBufferHead;
    FBufferHead := nil;
    while Assigned(Chunk1) do
    begin
      Chunk2 := Chunk1.BufferNext;
      FreeAndNil(Chunk1);
      Chunk1 := Chunk2;
    end;
    // Free available buffers
    Chunk1 := FBufferAvail;
    FBufferAvail := nil;
    while Assigned(Chunk1) do
    begin
      Chunk2 := Chunk1.BufferNext;
      FreeAndNil(Chunk1);
      Chunk1 := Chunk2;
    end;
  finally
    if Assigned(FCritSectBuffer) then
    begin
      FCritSectBuffer.Release;
      FreeAndNil(FCritSectBuffer);
    end;
  end;
  FreeAndNil(FCritSectFile);
  inherited Destroy;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
function TInMemoryLogger.AcquireChunk: TInMemoryChunk;
begin
  FCritSectBuffer.Acquire;
  try
    // Check if a buffer is available
    if not Assigned(FBufferAvail) then
      Result := nil
    else
    begin
      // Take one buffer from the buffer available list
      Result := FBufferAvail;
      FBufferAvail := FBufferAvail.BufferNext;
      Result.BufferNext := nil;
    end;
  finally
    FCritSectBuffer.Release;
  end;
  // If we got no buffer, then create a new one
  if not Assigned(Result) then
  begin
    Result := TInMemoryChunk.Create(FBufferSize);
    Result.OnAcquireChunk := AcquireChunkHandler;
  end;
end; { * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }

procedure TInMemoryLogger.ReleaseChunk(Chunk: TInMemoryChunk);
begin
  if not Assigned(Chunk) then
    Exit;
  FCritSectBuffer.Acquire;
  try // Add the buffer to the available buffer list
    Chunk.BufferNext := FBufferAvail;
    FBufferAvail := Chunk; // Clear data
    Chunk.WriteCount := 0;
{$IFDEF DEBUG}
    // When debugging, clear buffer memory
    if Assigned(Chunk.Buffer) then
      FillChar(PByte(Chunk.Buffer)^, Chunk.BufferSize, 0);
{$ENDIF}
  finally
    FCritSectBuffer.Release;
  end;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
// Build filename with LocalApp data location, company folder and
// same filename es executable with .log extension
procedure TInMemoryLogger.SetDefaultFileName;
var
  ExeName: array [0 .. MAX_PATH] of Char;
  Path: array [0 .. MAX_PATH] of Char;
  AppName: String;
begin
  // Fetch Windows LocalApp data folder location
  SHGetFolderPath(0, CSIDL_LOCAL_APPDATA, 0, SHGFP_TYPE_CURRENT, @Path[0]);
  // Fetch executable file path
  GetModuleFileName(0, ExeName, High(ExeName));
  AppName := ChangeFileExt(ExtractFileName(ExeName), '');
  // Build complete file name
  FFileName := IncludeTrailingPathDelimiter(Path) + CompanyFolder + '\' +
    AppName + '\' + AppName + '.Log';
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
procedure TInMemoryLogger.AcquireChunkHandler(Sender: TObject;
  var Chunk: TInMemoryChunk);
begin
  Chunk := AcquireChunk;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
procedure TInMemoryLogger.FlushToFile;
var
  Stream: TFileStream;
  Mode: Integer;
  Next: TInMemoryChunk;
begin
  // Check if critical section are assigned. The may be not assigned
  // in case of exception raise during contructor execution.
  if not Assigned(FCritSectFile) then
    Exit;
  if not Assigned(FCritSectBuffer) then
    Exit;
  FCritSectFile.Acquire;
  try
    if (not Assigned(FBufferHead)) or (FBufferHead.WriteCount <= 0) then
      Exit;
    if FFileName = '' then
      Exit;
    if FileExists(FFileName) then
      Mode := fmOpenWrite
    else
      Mode := fmCreate;
    Stream := TFileStream.Create(FFileName, Mode);
    try
      Stream.Seek(0, TSeekOrigin.soEnd);
      FCritSectBuffer.Acquire;
      try
        while Assigned(FBufferHead) do
        begin
          Stream.Write(FBufferHead.Buffer[0], FBufferHead.WriteCount);
          Dec(FByteCount, FBufferHead.WriteCount);
          FBufferHead.WriteCount := 0;
          Next := FBufferHead.BufferNext;
          ReleaseChunk(FBufferHead);
          FBufferHead := Next;
          // Release/Acquire the critical section to enhance concurrency
          FCritSectBuffer.Release;
          Sleep(0);
          // Let other thread take hand
          FCritSectBuffer.Acquire;
        end;
      finally
        FCritSectBuffer.Release;
      end;
    finally
      FreeAndNil(Stream);
    end;
  finally
    FCritSectFile.Release;
  end;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
function TInMemoryLogger.GetByteCount: Integer;
begin
  FCritSectBuffer.Acquire;
  try
    Result := FByteCount;
  finally
    FCritSectBuffer.Release;
  end;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
procedure TInMemoryLogger.Log(const Msg: String);
var
  NewBuffer: TInMemoryChunk;
  Buf: String;
begin
  FCritSectBuffer.Acquire;
  try
    if not Assigned(FBufferHead) then
    begin
      NewBuffer := AcquireChunk;
      FBufferHead := NewBuffer;
      FBufferCurrent := NewBuffer;
    end;
    if not Assigned(FBufferCurrent) then
      raise EInMemoryNoBufferException.Create(ERROR_MSG_NO_BUFFER);
    if (imloAddTime in FOptions) and (imloAddDate in FOptions) then
    begin
      // Add both date and time
      if imloAddTimeMSec in FOptions then
        Buf := FormatDateTime('YYYYMMDD HHNNSS.ZZZ ', Now)
      else
        Buf := FormatDateTime('YYYYMMDD HHNNSS ', Now);
      FBufferCurrent := FBufferCurrent.Write(Buf, FByteCount);
    end
    else if imloAddDate in FOptions then
    begin
      // Add date only
      Buf := FormatDateTime('YYYYMMDD ', Now);
      FBufferCurrent := FBufferCurrent.Write(Buf, FByteCount);
    end
    else if imloAddTime in FOptions then
    begin
      // Add time only
      if imloAddTimeMSec in FOptions then
        Buf := FormatDateTime('HHNNSS.ZZZ ', Now)
      else
        Buf := FormatDateTime('HHNNSS ', Now);
      FBufferCurrent := FBufferCurrent.Write(Buf, FByteCount);
    end;
    if not Assigned(FBufferCurrent) then
      raise EInMemoryNoBufferException.Create(ERROR_MSG_NO_BUFFER);
    FBufferCurrent := FBufferCurrent.Write(Msg, FByteCount);
    if imloAddCRLF in FOptions then
      FBufferCurrent := FBufferCurrent.Write(#13#10, FByteCount);
  finally
    FCritSectBuffer.Release;
  end;
end;

{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * }
end.
