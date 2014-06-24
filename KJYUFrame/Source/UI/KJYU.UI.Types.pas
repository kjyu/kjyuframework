unit KJYU.UI.Types;

interface
{$M+}
uses
  System.Generics.Collections, System.SysUtils,
  //
  KJYU.UI.Exceptions, KJYU.UI.Consts, KJYU.UI.Inf;

type
  TKJYUAlign = (alNone, alLeft, alRight, alTop, alBottom, alClient, alFitLeft,
    alFitRight, alFitTop, alFitBottom);
  TKJYUOperation = (Insert, Remove);
  TKJYUObjState = set of (stDesigning, stLoading);
  TKJYUObject = class;
  TKJYUObjectClass = class of TKJYUObject;
  TKJYUObjectList = TList<TKJYUObject>;

  TKJYUObject = class(TObject)
  private
    FHandle: THandle;
    FChildren: TKJYUObjectList;
    FParent: TKJYUObject;
    FObjState: TKJYUObjState;
    FName: string;
    FRoot: IKJYURoot;
    procedure SetParent(const Value: TKJYUObject);
    procedure SetRoot(const Value: IKJYURoot);
    function IsChild(AObj: TKJYUObject): Boolean; virtual;
    function GetChildrenCount: Integer;
    procedure SetName(const Value: string);
  protected
    { Parent }
    procedure ChangeParent; virtual;
    { Children }
    procedure DoAddObject(const AObj: TKJYUObject); virtual;
    procedure DoRemoveObject(const AObj: TKJYUObject); virtual;
    procedure DoInsertObject(AIndex: Integer; const AObj: TKJYUObject); virtual;
    procedure DoDeleteChildren; virtual;
  public
    constructor Create(AOwner: TKJYUObject); virtual;
    destructor Destroy; override;
    { Children }
    procedure AddObject(const AObj: TKJYUObject);
    procedure RemoveObject(const AObj: TKJYUObject); overload;
    procedure RemoveObject(AIndex: Integer); overload;
    procedure InsertObject(AIndex: Integer; const AObj: TKJYUObject);
    procedure DeleteChildren;
    { Design }
    procedure SetDesign(Value: Boolean; SetChildren: Boolean = True);
    property ObjState: TKJYUObjState read FObjState write FObjState;
    { children }
    property ChildrenCount: Integer read GetChildrenCount;
    property Handle: THandle read FHandle;
    property Parent: TKJYUObject read FParent write SetParent;
    property Root: IKJYURoot read FRoot write SetRoot;
  published
    property Name: string read FName write SetName;
  end;

implementation

uses
  System.Math;

{ TKJYUObject }

procedure TKJYUObject.AddObject(const AObj: TKJYUObject);
begin
  if Assigned(AObj) and (AObj.Parent <> Self) then
  begin
    if Assigned(AObj.Parent) then
      AObj.Parent := nil;
    DoAddObject(AObj);
  end;
end;

procedure TKJYUObject.ChangeParent;
begin
end;

constructor TKJYUObject.Create(AOwner: TKJYUObject);
begin
  inherited Create;
  if Assigned(AOwner) then
    AOwner.DoAddObject(Self);
end;

procedure TKJYUObject.DeleteChildren;
begin
  DoDeleteChildren;
end;

destructor TKJYUObject.Destroy;
begin
  if Assigned(FParent) then
    FParent.RemoveObject(Self);
  DeleteChildren;
  inherited;
end;

procedure TKJYUObject.DoAddObject(const AObj: TKJYUObject);
begin
  if not Assigned(FChildren) then
  begin
    FChildren := TKJYUObjectList.Create;
    FChildren.Capacity := 10;
  end;
  AObj.FParent := Self;
  AObj.Root := Root;
  AObj.ChangeParent;
  // do something about add a obj to this obj
  FChildren.Add(AObj);
end;

procedure TKJYUObject.DoDeleteChildren;
var
  I: Integer;
  LChild: TKJYUObject;
begin
  if Assigned(FChildren) then
  begin
    for I := FChildren.Count - 1 downto 0 do
    begin
      LChild := TKJYUObject(FChildren[I]);
      FChildren.Delete(I);
      LChild.FParent := nil;
      LChild.DisposeOf;
    end;
    FreeAndNil(FChildren);
  end;
end;

procedure TKJYUObject.DoInsertObject(AIndex: Integer; const AObj: TKJYUObject);
begin
  DoAddObject(AObj);
  if FChildren.Count > 0 then
  begin
    FChildren.Insert(AIndex, FChildren[FChildren.Count - 1]);
    FChildren.Delete(FChildren.Count - 1);
  end;
end;

procedure TKJYUObject.DoRemoveObject(const AObj: TKJYUObject);
var
  LIdx: Integer;
begin
  if Assigned(FChildren) then
  begin
    LIdx := FChildren.IndexOf(AObj);
    if LIdx >= 0 then
    begin
      AObj.FParent := nil;
      FChildren.Delete(LIdx);
    end;
  end;
end;

function TKJYUObject.GetChildrenCount: Integer;
begin
  Result := 0;
  if Assigned(FChildren) then
    Result := FChildren.Count;
end;

procedure TKJYUObject.InsertObject(AIndex: Integer; const AObj: TKJYUObject);
begin
  if Assigned(AObj) and (AObj.Parent <> Self) then
  begin
    if Assigned(AObj.Parent) then
      AObj.Parent := nil;
    DoInsertObject(AIndex, AObj);
  end;
end;

function TKJYUObject.IsChild(AObj: TKJYUObject): Boolean;
begin
  Result := False;
  while not Result and Assigned(AObj) do
  begin
    Result := AObj.Equals(Self);
    if not Result then
      AObj := AObj.Parent;
  end;
end;

procedure TKJYUObject.RemoveObject(const AObj: TKJYUObject);
begin
  if Assigned(AObj) then
    DoRemoveObject(AObj);
end;

procedure TKJYUObject.RemoveObject(AIndex: Integer);
begin
  if Assigned(FChildren) and InRange(AIndex, 0, FChildren.Count - 1) then
    RemoveObject(FChildren[AIndex]);
end;

procedure TKJYUObject.SetDesign(Value, SetChildren: Boolean);
var
  I: Integer;
begin
  if Value then
    Include(FObjState, stDesigning)
  else
    Exclude(FObjState, stDesigning);
  if SetChildren then
    for I := 0 to FChildren.Count - 1 do
      FChildren[I].SetDesign(Value);
end;

procedure TKJYUObject.SetName(const Value: string);
begin
  FName:= Value;
//  if FName <> Value then
//  begin
//    if (Value <> '') and not IsValidIdent(Value) then
//      raise EKJYUObjError.CreateResFmt(@SKErrorObjName, [Value]);
//    //ºÏ≤È «∑Ò√˚≥∆≥ÂÕª°£°£°£
////    if FParent <> nil then
////      FParent.ValidateRename(Self, FName, Value) else
////      ValidateRename(nil, FName, Value);
////    SetReference(False);
////    ChangeName(NewName);
////    SetReference(True);
//  end;
end;

procedure TKJYUObject.SetParent(const Value: TKJYUObject);
begin
  if Value = Self then
    Exit;
  if FParent <> Value then
  begin
    if IsChild(Value) then
      raise EKJYUInvalidOperation.Create(SKCannotCreateCircularDependence);
    if Assigned(FParent) then
      FParent.RemoveObject(Self);
    if Assigned(Value) then
      Value.AddObject(Self)
    else
      FParent := Value;
  end;
end;

procedure TKJYUObject.SetRoot(const Value: IKJYURoot);
begin
  FRoot := Value;
end;

end.
