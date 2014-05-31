unit KJYU.UI.Container;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls,
  System.UITypes, System.Types,FMX.Graphics, FMX.Objects,
  FMX.Platform, FMX.Forms, Xml.XMLIntf, Xml.XMLDoc,
  //
  KJYU.UI.Types, KJYU.UI.Classes, KJYU.UI.Inf, KJYU.UI.ClassLib;

type
  TOpenKJYUGraphicsObject = class(TKJYUGraphicsObject);

  TKGContainer = class(TControl, IKJYURoot)
  private
    FKJYUObj: TKJYUGraphicsObject;
    FFocused: IKJYUGraphicsObject;
    FConfigFilePath: string;
    FReloadConfig: Boolean; //当前焦点元件
    { Private declarations }
    procedure CreateRootKJYUGraphicsObject;
    procedure FreeKJYUObjs;
    {IKJYURoot}
    function GetRootLeft: Single;
    function GetRootTop: Single;
    {FromService}
    function GetFMXWindowsService: IFMXWindowService;
    function GetFMXMouseService: IFMXMouseService;
    procedure SetConfigFilePath(const Value: string);
    procedure SetReloadConfig(const Value: Boolean);
  protected
    procedure Paint; override;
    procedure Move; override;
    procedure Resize; override;
    procedure Show; override;
    { Protected declarations }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    {Root}
    function GetFocused: IKJYUGraphicsObject;
    procedure SetFocused(const Value: IKJYUGraphicsObject);
    function NewFocusedControl(const Value: IKJYUGraphicsObject): IKJYUGraphicsObject;
    {LoadConfig}
    procedure LoadConfig();
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure SetNewScene(AScene: IScene); override;
    {解析内部元件配置文件}
    procedure DoReloadConfigFile;
    procedure AnalysisConfigFile(const AFileName: string);
    procedure AnalysisLayout(ALayoutNode: IXMLNode; AParentGObj: TKJYUGraphicsObject);
    procedure SetGraphicsObjConfigProp(AObjNode: IXMLNode; AObj: TKJYUGraphicsObject);
    {IKJYURoot}
    property RootLeft: Single read GetRootLeft;
    property RootTop: Single read GetRootTop;
    {FormServer}
     property WinService: IFMXWindowService read GetFMXWindowsService;
     property MouseService: IFMXMouseService read GetFMXMouseService;
  published
    //主要用在设计时重新加载下配置文件
    property ReloadConfig: Boolean read FReloadConfig write SetReloadConfig;
    property ConfigFilePath: string read FConfigFilePath write SetConfigFilePath;
  published
    property Action;
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property Cursor default crDefault;
    property DesignVisible default True;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled default True;
    property Locked default False;
    property Height;
    property HitTest default True;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property TouchTargetExpansion;
    property Visible default True;
    property Width;
    property TabOrder;
    property TabStop;
    property OnApplyStyleLookup;
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    property OnKeyDown;
    property OnKeyUp;
    property OnCanFocus;
    property OnEnter;
    property OnExit;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPainting;
    property OnPaint;
    property OnResize;
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('KJYUFrameUI', [TKGContainer]);
end;

{ TKGContainer }

procedure TKGContainer.AfterConstruction;
begin
  inherited;
end;

constructor TKGContainer.Create(AOwner: TComponent);
begin
  inherited;
  SetAcceptsControls(True);
end;

procedure TKGContainer.CreateRootKJYUGraphicsObject;
var
  LObj, LObj2: TKJYUGraphicsObject;
  LState: TKJYUObjState;
begin
  if csDesigning in ComponentState then
  begin
    LState := [stDesigning];
  end;
  FKJYUObj := TKJYUGraphicsObject.Create(nil);
  FKJYUObj.SetNewScene(Scene);
  FKJYUObj.Top := 0;
  FKJYUObj.Left := 0;
  FKJYUObj.Width := Self.Width;
  FKJYUObj.Height := Self.Height;
  FKJYUObj.Visible := True;
  FKJYUObj.Name := 'RootKJYUGraphicsObject';
  FKJYUObj.Root := Self as IKJYURoot;
  FKJYUObj.ObjState := LState;
  //设为焦点原件
  FFocused := FKJYUObj;
end;

destructor TKGContainer.Destroy;
begin
  FreeKJYUObjs;
  inherited;
end;

procedure TKGContainer.FreeKJYUObjs;
begin
  FKJYUObj.Free;
end;

procedure TKGContainer.MouseClick(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  LObj: IKJYUGraphicsObject;
begin
  inherited;
  if Assigned(FKJYUObj) then
  begin
    if PtInRect(FKJYUObj.AbsRect, PointF(X, Y)) then
    begin
      LObj := TOpenKJYUGraphicsObject(FKJYUObj).ObjectAtPoint(PointF(X, Y));
      if Assigned(LObj) then
        LObj.MouseClick(Button, Shift, X, Y);
    end;
  end;
end;

procedure TKGContainer.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  LObj: IKJYUGraphicsObject;
begin
  inherited;
  if Assigned(FKJYUObj) then
  begin
    if PtInRect(FKJYUObj.AbsRect, PointF(X, Y)) then
    begin
      LObj := TOpenKJYUGraphicsObject(FKJYUObj).ObjectAtPoint(PointF(X, Y));
      if Assigned(LObj) then
        LObj.MouseDown(Button, Shift, X, Y);
    end;
  end;
end;

procedure TKGContainer.MouseMove(Shift: TShiftState; X, Y: Single);
var
  LObj: IKJYUGraphicsObject;
begin
  if Assigned(FKJYUObj) then
  begin
    if PtInRect(FKJYUObj.AbsRect, PointF(X, Y)) then
    begin
      LObj := TOpenKJYUGraphicsObject(FKJYUObj).ObjectAtPoint(PointF(X, Y));
      if Assigned(LObj) then
        LObj.MouseMove(Shift, X, Y);
    end;
  end;
  inherited;
end;

procedure TKGContainer.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  LObj: IKJYUGraphicsObject;
begin
  inherited;
   //系统调用刷新2
  if Assigned(FKJYUObj) then
  begin
    if PtInRect(FKJYUObj.AbsRect, PointF(X, Y)) then
    begin
      LObj := TOpenKJYUGraphicsObject(FKJYUObj).ObjectAtPoint(PointF(X, Y));
      if Assigned(LObj) then
        LObj.MouseUp(Button, Shift, X, Y);
    end;
  end;
end;

procedure TKGContainer.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
var
  LObj: IKJYUGraphicsObject;
  LMousePos: TPointF;
begin
  inherited;
   //系统调用刷新2
  if Assigned(FKJYUObj) then
  begin
    if Assigned(MouseService) then
      LMousePos := MouseService.GetMousePos;
    LObj := TOpenKJYUGraphicsObject(FKJYUObj).ObjectAtPoint(ScreenToLocal(LMousePos));
    if Assigned(LObj) then
      LObj.MouseWheel(Shift, WheelDelta, Handled);
  end;
end;

procedure TKGContainer.Move;
begin
  inherited;

end;

function TKGContainer.NewFocusedControl(
  const Value: IKJYUGraphicsObject): IKJYUGraphicsObject;
//var
//  NewFocused: IControl;
//  LParentForm: TCommonCustomForm;
//  P: TFmxObject;
//  NewCanFocus: Boolean;
begin
  Result := Value;
//  Result := nil;
//  if Assigned(Value) then
//  begin
//    LParentForm := ParentFormOfIControl(Value);
//    if LParentForm = self then
//    begin
//      NewFocused := Value;
//      NewCanFocus := False;
//      while (not NewCanFocus) and
//            (Assigned(NewFocused)) and
//            (NewFocused.Visible) and
//            (NewFocused.GetCanParentFocus) do
//      begin
//        NewCanFocus := NewFocused.GetCanFocus;
//        if (not NewCanFocus) then
//        begin
//          if (not Assigned(NewFocused.Parent)) or (not Supports(NewFocused.Parent, IControl, NewFocused)) then
//            break;
//        end;
//      end;
//      if Assigned(NewFocused) then
//        NewCanFocus := NewFocused.GetCanFocus;
//      if NewCanFocus then
//      begin
//        P := NewFocused.Parent;
//        while Assigned(P) and (P.IsIControl) do
//        begin
//          if not P.AsIControl.Visible or not P.AsIControl.Enabled then
//            Exit;
//          P := P.Parent;
//        end;
//        if NewFocused.AbsoluteEnabled then
//          Result := NewFocused;
//      end;
//    end
//    else
//      if Assigned(LParentForm) then
//        Result := LParentForm.NewFocusedControl(Value);
//  end;
end;


procedure TKGContainer.Paint;
var
  R: TRectF;
begin
  R := LocalRect;
  InflateRect(R, -0.5, -0.5);
  Canvas.Stroke.Thickness := 1;
  if (csDesigning in ComponentState) then
  begin
    Canvas.Stroke.Dash := TStrokeDash.Dash;
  end
  else
  begin
    Canvas.Stroke.Dash := TStrokeDash.Custom;
  end;
  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.Stroke.Color := $A0909090;
  Canvas.DrawRect(R, 0, 0, AllCorners, 100);

  inherited;
  //系统调用刷新2
  if Assigned(FKJYUObj) and Assigned(FScene) then
  begin
    LayoutObjects(FKJYUObj);
    TOpenKJYUGraphicsObject(FKJYUObj).PaintInternal;
  end;
end;
procedure TKGContainer.Resize;
begin
  inherited;
  if Assigned(FKJYUObj) then
  begin
    if FKJYUObj.Height <> Self.Height then
      FKJYUObj.Height := Self.Height;
    if FKJYUObj.Width <> Self.Width then
      FKJYUObj.Width := Self.Width;
  end;
end;

{LoadConfigfile}
procedure TKGContainer.LoadConfig;
begin

end;

procedure TKGContainer.DoReloadConfigFile;
var
  I: Integer;
begin
  if Assigned(FKJYUObj) and (FKJYUObj.GObjectsCount > 0) then
  begin
    for I := FKJYUObj.GObjectsCount - 1 downto 0 do
    FKJYUObj.GObjects[I].Free;
  end;

  if FileExists(FConfigFilePath) then
  begin
    AnalysisConfigFile(FConfigFilePath);
  end;

  InvalidateRect(LocalRect);
end;

procedure TKGContainer.Loaded;
begin
  inherited;
  CreateRootKJYUGraphicsObject;
  if FileExists(FConfigFilePath) then
  begin
    AnalysisConfigFile(FConfigFilePath);
  end;
end;

procedure TKGContainer.SetGraphicsObjConfigProp(AObjNode: IXMLNode; AObj: TKJYUGraphicsObject);
  function StrToAlignType(AStr: string): TKJYUAlign;
  begin
    if AStr = 'alleft' then
    Exit(alleft);
    if AStr = 'alnone' then
    Exit(alnone);
    if AStr = 'alclient' then
    Exit(alclient);
    if astr = 'altop' then
    Exit(altop);
    if astr = 'albottom' then
    Exit(albottom);
    if AStr = 'alright' then
    Exit(alright);

    Exit(alnone);
  end;
begin
  if AObjNode.AttributeNodes.Count > 0 then
  begin
    AObj.SetNewScene(Scene);
    AObj.Top := AObjNode.Attributes['top'];;
    AObj.Left := AObjNode.Attributes['left'];;
    AObj.Width := AObjNode.Attributes['width'];;
    AObj.Height := AObjNode.Attributes['height'];;
    AObj.Visible := StrToBool(AObjNode.Attributes['visible']);
    AObj.Align := StrToAlignType(AObjNode.Attributes['align']);
    AObj.Name := AObjNode.Attributes['name'];;
    AObj.Root := Self as IKJYURoot;
    AObj.ObjState := FKJYUObj.ObjState;
  end;
end;

procedure TKGContainer.AnalysisLayout(ALayoutNode: IXMLNode; AParentGObj: TKJYUGraphicsObject);
var
  I: Integer;
  LNode: IXMLNode;
  LName: string;
  LClass: TKJYUObjectClass;
  LObj: TKJYUObject;
begin
  for I := 0 to ALayoutNode.ChildNodes.Count - 1 do
  begin
    LNode := ALayoutNode.ChildNodes[I];
    LName := LNode.NodeName;
    LClass := G_ClassLib.GetClass(LName);
    if Assigned(LClass) then
    begin
      LObj := LClass.Create(AParentGObj);
      //赋值
      if LObj is TKJYUGraphicsObject then
      begin
        SetGraphicsObjConfigProp(LNode, TKJYUGraphicsObject(LObj));
        if TKJYUGraphicsObject(LObj).AcceptGObject then
          AnalysisLayout(LNode, TKJYUGraphicsObject(LObj));
      end;
    end;
  end;
end;

procedure TKGContainer.AnalysisConfigFile(const AFileName: string);
var
  LClass: TKJYUObjectClass;
  LObj: TKJYUObject;
  LDoc: IXMLDocument;
begin
  if not Assigned(FKJYUObj) then
    Exit;

  LDoc := TXMLDocument.Create(Self);
  LDoc.LoadFromFile(AFileName);
  if LDoc.ChildNodes.Count > 1 then
  begin
    if LDoc.ChildNodes[1].NodeName = 'Layout' then
    begin
      AnalysisLayout(LDoc.ChildNodes[1], FKJYUObj);
    end;
  end;
end;

procedure TKGContainer.SetConfigFilePath(const Value: string);
begin
  if FConfigFilePath <> Value then
  begin
    FConfigFilePath := Value;
  end;
end;

procedure TKGContainer.SetFocused(const Value: IKJYUGraphicsObject);
begin
  FFocused := Value;
end;

procedure TKGContainer.SetNewScene(AScene: IScene);
begin
  inherited;
  if Assigned(FKJYUObj) and Assigned(AScene) then
    FKJYUObj.SetNewScene(AScene);
end;

procedure TKGContainer.SetReloadConfig(const Value: Boolean);
begin
  FReloadConfig := Value;
  if Assigned(FKJYUObj) and (stDesigning in FKJYUObj.ObjState) then
  begin
    DoReloadConfigFile;
  end;
end;

procedure TKGContainer.Show;
begin

  inherited;
end;

{IKJYURoot}
function TKGContainer.GetRootLeft: Single;
begin
  Result := Self.AbsoluteRect.Left;
end;

function TKGContainer.GetRootTop: Single;
begin
  Result := Self.AbsoluteRect.Top;
end;

{FormService}
function TKGContainer.GetFMXMouseService: IFMXMouseService;
begin
  Result := nil;
  TPlatformServices.Current.SupportsPlatformService(IFMXMouseService, IInterface(Result));
end;

function TKGContainer.GetFMXWindowsService: IFMXWindowService;
begin
  Result := nil;
  TPlatformServices.Current.SupportsPlatformService(IFMXWindowService, IInterface(Result));
end;


function TKGContainer.GetFocused: IKJYUGraphicsObject;
begin
  Result := FFocused;
end;

end.
