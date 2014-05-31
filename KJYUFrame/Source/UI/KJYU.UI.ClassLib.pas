unit KJYU.UI.ClassLib;

interface
uses
  System.Classes, System.Generics.Collections, System.SysUtils,
  //
  KJYU.UI.Types, KJYU.UI.Classes;
type
  TClassLib = class
  strict private
    FDicClass: TDictionary<string,TKJYUObjectClass>;
  public
    constructor Create;
    destructor Destroy; override;
    //
    procedure RegisterClass(const AName: string; AKJYUObjectClass: TKJYUObjectClass);
    procedure UnRegisterClass(const AName: string);
    function GetClass(AName: string):TKJYUObjectClass;
  end;
  function G_ClassLib: TClassLib;
implementation

{ TClassLib }

constructor TClassLib.Create;
begin
  FDicClass := TDictionary<string,TKJYUObjectClass>.Create;
end;

destructor TClassLib.Destroy;
begin
  FDicClass.Free;
  inherited;
end;

function TClassLib.GetClass(AName: string): TKJYUObjectClass;
begin
  if not FDicClass.TryGetValue(UpperCase(AName), Result) then
    Result := nil;
end;

procedure TClassLib.RegisterClass(const AName: string;
  AKJYUObjectClass: TKJYUObjectClass);
begin
  FDicClass.AddOrSetValue(UpperCase(AName), AKJYUObjectClass);
end;

procedure TClassLib.UnRegisterClass(const AName: string);
begin
  if FDicClass.ContainsKey(UpperCase(AName)) then
    FDicClass.Remove(UpperCase(AName));
end;
//------------------------------------------------------------------------------
var
  gClassLib: TClassLib;

procedure RegisterAllClass;
begin
  if not Assigned(gClassLib) then Exit;

  gClassLib.RegisterClass('KPanel', TKJYUGraphicsObject);
end;

function G_ClassLib: TClassLib;
begin
  if not Assigned(gClassLib) then
  begin
    gClassLib := TClassLib.Create;
    RegisterAllClass;
  end;

  Result := gClassLib;
end;

initialization
  gClassLib := TClassLib.Create;
  RegisterAllClass;
finalization
  gClassLib.Free;
end.
