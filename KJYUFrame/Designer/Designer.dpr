program Designer;

uses
  FMX.Forms,
  DesignMain in 'DesignMain.pas' {Form2},
  ProjectUnit in 'ProjectUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
