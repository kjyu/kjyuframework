program Project1;

uses
  FMX.Forms,
  OverbyteInMemoryChunk in '..\Source\MemoryLogger\OverbyteInMemoryChunk.pas',
  OverbyteInMemoryLogger in '..\Source\MemoryLogger\OverbyteInMemoryLogger.pas',
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
