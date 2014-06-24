unit KJYU.UI.Container;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls,
  System.UITypes, System.Types, FMX.Graphics, FMX.Objects,
  FMX.Platform, FMX.Forms, Xml.XMLIntf, Xml.XMLDoc,
  //
  KJYU.UI.Types, KJYU.UI.Classes, KJYU.UI.Inf, KJYU.UI.ClassLib,
  KJYU.UI.FuncLib;

const
  KJYU_DESIGNPOINT_SIZE = 3;

type
  TOpenKJYUGraphicsObject = class(TKJYUGraphicsObject);
  TInBondType = (ibtNone, ibtLeft, ibtTop, ibtRight, ibtBottom, ibtLeftTop,
    ibtRightTop, ibtLeftBottom, ibtRightBottom);
  TObjDesignState = (dsNone, dsMove, dsResize);

  TKGContainer = class(TControl, IKJYURoot)
  private
    FKJYUObj: TKJYUGraphicsObject;
    FFocused: IKJYUGraphicsObject; // 当前焦点元件
    FCaptured: IKJYUGraphicsObject; // 当前捕获鼠标信息的元件
    FConfigFilePath: string;
    FReloadConfig: Boolean;
    { Designing KJYUFrame Object }
    FIsDesigning: Boolean;
    FInBond: Boolean;
    FInBondType: TInBondType;
    FResizing: Boolean;
    FOldPointF: TPointF;
    FDesigningRect: TRectF; // 设计期矩形用来画元件边框以及操作元件大小
    FDesigningPintRects: TArray<TRectF>; // 设计期8个点矩形
    FNewDesignFocused, FOldDesignFocused: IKJYUGraphicsObject;
    FObjDesignState: TObjDesignState;
    FObjDesignResizeType: TInBondType;
    FDesignPress, FDesignMove, FDesignResize: Boolean;
    FDesignOldPoint, FDesignNewPoint: TPointF;
    FOnKJYUObjSelected: TNotifyEvent;
    { Private declarations }
    procedure CreateRootKJYUGraphicsObject;
    procedure FreeKJYUObjs;
    { IKJYURoot }
    function GetRootLeft: Single;
    function GetRootTop: Single;
    { FromService }
    function GetFMXWindowsService: IFMXWindowService;
    function GetFMXMouseService: IFMXMouseService;
    procedure SetConfigFilePath(const Value: string);
    procedure SetReloadConfig(const Value: Boolean);
    procedure SetIsDesigning(const Value: Boolean);
    { Designing KJYUFrame Object }
    procedure SetCurrentCursor(X, Y: Single);
    // 设计期点击到元件的话设置下焦点
    procedure SetDesigningFocused(Button: TMouseButton; X, Y: Single);
    procedure ButtonDownOnDesigning(Button: TMouseButton; X, Y: Single);
    procedure DrawDesigningRect;
    procedure ProcessDesigningObjectOnMouseMove(X, Y: Single);
    // 判断是移动还是设置大小
    procedure SetObjDesignState(Button: TMouseButton; X, Y: Single);
    { Send Event To Frame }
    procedure SendToFrameMouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); virtual;
    procedure SendToFrameMouseMove(Shift: TShiftState; X, Y: Single); virtual;
    procedure SendToFrameMouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); virtual;
    procedure SendToFrameMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      var Handled: Boolean); virtual;
    procedure SendToFrameMouseClick(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); virtual;
    procedure SendToFramePaint;
    procedure SetOnKJYUObjSelected(const Value: TNotifyEvent);
  protected
    procedure Paint; override;
    procedure Move; override;
    procedure Resize; override;
    procedure Show; override;
    { Protected declarations }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer;
      var Handled: Boolean); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    { Root }
    function GetFocused: IKJYUGraphicsObject;
    procedure SetFocused(const Value: IKJYUGraphicsObject);
    function NewFocusedControl(const Value: IKJYUGraphicsObject)
      : IKJYUGraphicsObject;
    { LoadConfig }
    procedure LoadConfig();
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure SetNewScene(AScene: IScene); override;
    { 解析内部元件配置文件 }
    procedure DoReloadConfigFile;
    procedure AnalysisConfigFile(const AFileName: string);
    procedure AnalysisLayout(ALayoutNode: IXMLNode;
      AParentGObj: TKJYUGraphicsObject);
    procedure SetGraphicsObjConfigProp(AObjNode: IXMLNode;
      AObj: TKJYUGraphicsObject);
    { IKJYURoot }
    property RootLeft: Single read GetRootLeft;
    property RootTop: Single read GetRootTop;
    function GetCaptured: IKJYUGraphicsObject;
    procedure SetCaptured(const Value: IKJYUGraphicsObject);
    { FormServer }
    property WinService: IFMXWindowService read GetFMXWindowsService;
    property MouseService: IFMXMouseService read GetFMXMouseService;
    property Captured: IKJYUGraphicsObject read GetCaptured write SetCaptured;
  published
    // 主要用在设计时重新加载下配置文件
    property ReloadConfig: Boolean read FReloadConfig write SetReloadConfig;
    property ConfigFilePath: string read FConfigFilePath
      write SetConfigFilePath;
    { Designing KJYUFrame Object }
    property IsDesigning: Boolean read FIsDesigning write SetIsDesigning;
    property OnKJYUObjSelected: TNotifyEvent read FOnKJYUObjSelected
      write SetOnKJYUObjSelected;
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

uses codesitelogging;

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
  // Specifies whether the control captures mouse events.
  // When a control captures the mouse, all subsequent mouse events
  // go to that control until the user releases the mouse button
  AutoCapture := True;
  SetAcceptsControls(True);
end;

procedure TKGContainer.CreateRootKJYUGraphicsObject;
var
  LObj, LObj2: TKJYUGraphicsObject;
  LState: TKJYUObjState;
begin
  if (csDesigning in ComponentState) or FIsDesigning then
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
  // 设为焦点原件
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

procedure TKGContainer.MouseClick(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited;
  if FIsDesigning and FResizing then
    Exit;
  if FIsDesigning and FDesignPress and (FObjDesignState <> dsNone) then
    Exit;
  // 如果是设计期的话将当前焦点设为鼠标所在元件
  if FIsDesigning then
    ButtonDownOnDesigning(Button, X, Y)
  else
    SendToFrameMouseClick(Button, Shift, X, Y);
end;

procedure TKGContainer.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited;
  // 左键按下，如果在设计期并且在边框上的话进入调整大小状态
  if (Button = TMouseButton.mbLeft) and FIsDesigning then
  begin
    if FInBond then
    begin
      FOldPointF := PointF(X, Y);
      FResizing := True;
    end
    else if PtInRect(FDesigningRect, PointF(X, Y)) then
    begin
      FDesignPress := True;
      FDesignOldPoint := PointF(X, Y);
    end;
  end;

  SendToFrameMouseDown(Button, Shift, X, Y);
end;

procedure TKGContainer.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  SetCurrentCursor(X, Y);
  // 如果在设计期并且在Root容器调整中则调整大小
  if FIsDesigning then
  begin
    // 调整Root容器
    if FResizing then
    begin
      case FInBondType of
        ibtNone:
          ;
        ibtLeft:
          ;
        ibtTop:
          ;
        ibtRight:
          begin
            Width := Width + X - FOldPointF.X;
          end;
        ibtBottom:
          begin
            Height := Height + Y - FOldPointF.Y;
          end;
        ibtRightBottom:
          begin
            Width := Width + X - FOldPointF.X;
            Height := Height + Y - FOldPointF.Y;
          end;
      end;
      FOldPointF := PointF(X, Y);
    end
    else
    begin
      ProcessDesigningObjectOnMouseMove(X, Y);
    end;

    Exit;
  end;
  SendToFrameMouseMove(Shift, X, Y);
end;

procedure TKGContainer.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  inherited;
  if FIsDesigning then
  begin
    FResizing := False;
    FObjDesignState := dsNone;
    FDesignPress := False;
    Exit;
  end;

  // 有捕获鼠标的元件的话传送事件给元件
  if Assigned(FCaptured) and (not FIsDesigning) then
  begin
    FCaptured.MouseUp(Button, Shift, X, Y);
    Exit;
  end;
  SendToFrameMouseUp(Button, Shift, X, Y);
end;

procedure TKGContainer.MouseWheel(Shift: TShiftState; WheelDelta: Integer;
  var Handled: Boolean);
var
  LObj: IKJYUGraphicsObject;
  LMousePos: TPointF;
begin
  inherited;
  SendToFrameMouseWheel(Shift, WheelDelta, Handled);
end;

procedure TKGContainer.Move;
begin
  inherited;

end;

function TKGContainer.NewFocusedControl(const Value: IKJYUGraphicsObject)
  : IKJYUGraphicsObject;
// var
// NewFocused: IControl;
// LParentForm: TCommonCustomForm;
// P: TFmxObject;
// NewCanFocus: Boolean;
begin
  Result := Value;
  // Result := nil;
  // if Assigned(Value) then
  // begin
  // LParentForm := ParentFormOfIControl(Value);
  // if LParentForm = self then
  // begin
  // NewFocused := Value;
  // NewCanFocus := False;
  // while (not NewCanFocus) and
  // (Assigned(NewFocused)) and
  // (NewFocused.Visible) and
  // (NewFocused.GetCanParentFocus) do
  // begin
  // NewCanFocus := NewFocused.GetCanFocus;
  // if (not NewCanFocus) then
  // begin
  // if (not Assigned(NewFocused.Parent)) or (not Supports(NewFocused.Parent, IControl, NewFocused)) then
  // break;
  // end;
  // end;
  // if Assigned(NewFocused) then
  // NewCanFocus := NewFocused.GetCanFocus;
  // if NewCanFocus then
  // begin
  // P := NewFocused.Parent;
  // while Assigned(P) and (P.IsIControl) do
  // begin
  // if not P.AsIControl.Visible or not P.AsIControl.Enabled then
  // Exit;
  // P := P.Parent;
  // end;
  // if NewFocused.AbsoluteEnabled then
  // Result := NewFocused;
  // end;
  // end
  // else
  // if Assigned(LParentForm) then
  // Result := LParentForm.NewFocusedControl(Value);
  // end;
end;

procedure TKGContainer.Paint;
var
  R: TRectF;
begin
  R := LocalRect;
  InflateRect(R, -0.5, -0.5);
  Canvas.Stroke.Thickness := 1;
  if (csDesigning in ComponentState) or FIsDesigning then
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
  // 布局并画KJYUObject元件
  SendToFramePaint;
  // 设计时的话画当前设计时焦点元件边框
  if FIsDesigning then
    DrawDesigningRect;
end;

procedure TKGContainer.ProcessDesigningObjectOnMouseMove(X, Y: Single);
var
  LObj, LParent: TKJYUGraphicsObject;
  LRefRect: TRectF;
  LNeedRefParent: Boolean;
  LH, LW: Single;
begin
  LNeedRefParent := False;
  // 调整元件大小
  if (FObjDesignState in [dsResize, dsMove]) and FPressed then
  begin
    LObj := TKJYUGraphicsObject(FNewDesignFocused);

    case FObjDesignState of
      dsMove:
        begin
          // 暂时只有在布局为None的时候才能移动
          if not(LObj.Align in [TKJYUAlign.alNone]) then
            Exit;

          LObj.Left := LObj.Left + X - FDesignOldPoint.X;
          LObj.Top := LObj.Top + Y - FDesignOldPoint.Y;
        end;
      dsResize:
        begin
          case FObjDesignResizeType of
            ibtLeft:
              begin
                LW := LObj.Width - (X - FDesignOldPoint.X);
                if LW < 0 then
                begin
                  LObj.Left := LObj.Left - LObj.Width;
                  LObj.Width := Abs(LW);
                  FObjDesignResizeType := ibtRight;
                end
                else
                begin
                  LObj.Width := LW;
                  LObj.Left := LObj.Left + (X - FDesignOldPoint.X);
                end;
              end;
            ibtTop:
              begin
                LH := LObj.Height + FDesignOldPoint.Y - Y;
                if LH < 0 then
                begin
                  LObj.Top := LObj.Top + LObj.Height;
                  LObj.Height := Abs(LH);
                  FObjDesignResizeType := ibtBottom;
                end
                else
                begin
                  LObj.Height := LH;
                  LObj.Top := LObj.Top + Y - FDesignOldPoint.Y;
                end;
              end;
            ibtRight:
              begin
                LW := LObj.Width + X - FDesignOldPoint.X;
                if LW < 0 then
                begin
                  LObj.Left := LObj.Left - LObj.Width;
                  LObj.Width := Abs(LW);
                  FObjDesignResizeType := ibtLeft;
                end
                else
                begin
                  LObj.Width := LW;
                end;
              end;
            ibtBottom:
              begin
                LH := LObj.Height + Y - FDesignOldPoint.Y;
                if LH < 0 then
                begin
                  LObj.Top := LObj.Top - LObj.Height;
                  LObj.Height := Abs(LH);
                  FObjDesignResizeType := ibtTop;
                end
                else
                begin
                  LObj.Height := LH;
                end;
              end;
            ibtLeftTop:
              ;
            ibtRightTop:
              begin
              end;
            ibtLeftBottom:
              begin
                // 未考虑负数
                LW := LObj.Width - (X - FDesignOldPoint.X);
                LH := LObj.Height + (Y - FDesignOldPoint.Y);
                LObj.Width := LW;
                LObj.Left := LObj.Left + (X - FDesignOldPoint.X);
                LObj.Height := LObj.Height + (Y - FDesignOldPoint.Y);
              end;
            ibtRightBottom:
              begin
                LObj.Width := LObj.Width + X - FDesignOldPoint.X;
                LObj.Height := LObj.Height + Y - FDesignOldPoint.Y;
              end;
          end;
        end;
    end;
    FDesignOldPoint := PointF(X, Y);
    // 通知父元件布局并刷新
    if (Assigned(LObj.Parent)) then
    begin
      if LObj.Align in [alLeft, alRight, alTop, alBottom, alClient] then
        LNeedRefParent := True;

      LRefRect := FDesigningRect;
      LayoutObjects(TKJYUGraphicsObject(LObj.Parent));
      if LNeedRefParent then
      begin
        InvalidateRect(TKJYUGraphicsObject(LObj.Parent).AbsRect);
      end;

      begin
        FDesigningRect := LObj.AbsRect;
        FDesigningPintRects := SplitRectFToEightPoint(FDesigningRect,
          KJYU_DESIGNPOINT_SIZE);
        InflateRect(LRefRect, KJYU_DESIGNPOINT_SIZE, KJYU_DESIGNPOINT_SIZE);
        InvalidateRect(LRefRect);
        LRefRect := LObj.AbsRect;
        InflateRect(LRefRect, KJYU_DESIGNPOINT_SIZE, KJYU_DESIGNPOINT_SIZE);
        InvalidateRect(LRefRect);
      end;
    end;
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
    // 在设计期重新设置下包含当前元件的Rect大小
    if FIsDesigning and Assigned(FNewDesignFocused) then
    begin
      FDesigningRect := TKJYUGraphicsObject(FNewDesignFocused).AbsRect;
      FDesigningPintRects := SplitRectFToEightPoint(FDesigningRect,
        KJYU_DESIGNPOINT_SIZE);
    end;
  end;
end;

{ LoadConfigfile }
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

procedure TKGContainer.SetGraphicsObjConfigProp(AObjNode: IXMLNode;
  AObj: TKJYUGraphicsObject);
  function StrToAlignType(AStr: string): TKJYUAlign;
  begin
    if AStr = 'alleft' then
      Exit(alLeft);
    if AStr = 'alnone' then
      Exit(alNone);
    if AStr = 'alclient' then
      Exit(alClient);
    if AStr = 'altop' then
      Exit(alTop);
    if AStr = 'albottom' then
      Exit(alBottom);
    if AStr = 'alright' then
      Exit(alRight);

    Exit(alNone);
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

procedure TKGContainer.SetIsDesigning(const Value: Boolean);
begin
  FIsDesigning := Value;
end;

procedure TKGContainer.AnalysisLayout(ALayoutNode: IXMLNode;
  AParentGObj: TKJYUGraphicsObject);
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
      // 赋值
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

{ Send Event To Frame }
procedure TKGContainer.SendToFrameMouseClick(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  LObj: IKJYUGraphicsObject;
begin
  if Assigned(FKJYUObj) then
  begin
    if PtInRect(FKJYUObj.AbsRect, PointF(X, Y)) then
    begin
      LObj := TOpenKJYUGraphicsObject(FKJYUObj).ObjectAtPoint(PointF(X, Y));
      if Assigned(LObj) then
      begin
        LObj.MouseClick(Button, Shift, X, Y);
        // 设置当前焦点元件
        FFocused := LObj;
      end;
    end;
  end;
end;

procedure TKGContainer.SendToFrameMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  LObj: IKJYUGraphicsObject;
begin
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

procedure TKGContainer.SendToFrameMouseMove(Shift: TShiftState; X, Y: Single);
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
end;

procedure TKGContainer.SendToFrameMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  LObj: IKJYUGraphicsObject;
begin
  // 系统调用刷新2
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

procedure TKGContainer.SendToFrameMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  // 系统调用刷新2
  if Assigned(FKJYUObj) then
  begin
    // 焦点元件执行鼠标滚动事件
    if Assigned(FFocused) then
      FFocused.MouseWheel(Shift, WheelDelta, Handled);
  end;
end;

procedure TKGContainer.SendToFramePaint;
begin
  if Assigned(FKJYUObj) and Assigned(FScene) then
  begin
    LayoutObjects(FKJYUObj);
    TOpenKJYUGraphicsObject(FKJYUObj).PaintInternal;
  end;
end;

procedure TKGContainer.SetCaptured(const Value: IKJYUGraphicsObject);
begin
  FCaptured := Value;
end;

procedure TKGContainer.SetConfigFilePath(const Value: string);
begin
  if FConfigFilePath <> Value then
  begin
    FConfigFilePath := Value;
  end;
end;

procedure TKGContainer.SetCurrentCursor(X, Y: Single);
var
  LRBondRect, LBBondRect: TRectF;
  LDLRect, LDTRect, LDRRect, LDBRect: TRectF;
  LPointF: TPointF;
begin
  if FIsDesigning then
  begin
    LPointF := PointF(X, Y);
    // 正在设置Root容器或者元件大小的话退出
    if FResizing or ((FObjDesignState = dsResize) and FDesignPress) then
      Exit;
    // 检测是否在操作Root容器边框
    // 右边界
    LRBondRect := RectF(Self.Left + Self.Width - 2, Self.Top,
      Self.Left + Self.Width, Self.Top + Self.Height);
    // 下边界
    LBBondRect := RectF(Self.Left, Self.Top + Self.Height - 2,
      Self.Left + Self.Width, Self.Top + Self.Height);
    if PtInRect(LRBondRect, PointF(X, Y)) and PtInRect(LBBondRect, PointF(X, Y))
    then
    begin
      FInBond := True;
      FInBondType := ibtRightBottom;
      Cursor := crSizeNWSE;
    end
    else if PtInRect(LRBondRect, PointF(X, Y)) then
    begin
      FInBond := True;
      FInBondType := ibtRight;
      Cursor := crSizeWE;
    end
    else if PtInRect(LBBondRect, PointF(X, Y)) then
    begin
      FInBond := True;
      FInBondType := ibtBottom;
      Cursor := crSizeNS;
    end
    else if PtInRect(FDesigningRect, PointF(X, Y)) then
    begin
      // 检测是否在焦点元件并且在边框上
      // 如果在设置当前鼠标状态以及操作状态
      if PtInRect(FDesigningPintRects[0], LPointF) then
      begin
        FObjDesignState := dsResize;
        Cursor := crSizeNWSE;
        FObjDesignResizeType := ibtLeftTop;
      end
      else if PtInRect(FDesigningPintRects[6], LPointF) then
      begin
        FObjDesignState := dsResize;
        Cursor := crSizeNESW;
        FObjDesignResizeType := ibtLeftBottom;
      end
      else if PtInRect(FDesigningPintRects[2], LPointF) then
      begin
        FObjDesignState := dsResize;
        Cursor := crSizeNESW;
        FObjDesignResizeType := ibtRightTop;
      end
      else if PtInRect(FDesigningPintRects[4], LPointF) then
      begin
        FObjDesignState := dsResize;
        Cursor := crSizeNWSE;
        FObjDesignResizeType := ibtRightBottom;
      end
      else if PtInRect(FDesigningPintRects[3], LPointF) then
      begin
        FObjDesignState := dsResize;
        Cursor := crSizeWE;
        FObjDesignResizeType := ibtRight;
      end
      else if PtInRect(FDesigningPintRects[7], LPointF) then
      begin
        FObjDesignState := dsResize;
        Cursor := crSizeWE;
        FObjDesignResizeType := ibtLeft;
      end
      else if PtInRect(FDesigningPintRects[1], LPointF) then
      begin
        FObjDesignState := dsResize;
        Cursor := crSizeNS;
        FObjDesignResizeType := ibtTop;
      end
      else if PtInRect(FDesigningPintRects[5], LPointF) then
      begin
        FObjDesignState := dsResize;
        Cursor := crSizeNS;
        FObjDesignResizeType := ibtBottom;
      end
      else
      begin
        FObjDesignState := dsMove;
        Cursor := crDefault;
      end;
    end
    else
    begin
      FInBond := False;
      FInBondType := ibtNone;
      FObjDesignState := dsNone;
      Cursor := crDefault;
    end;
  end;
end;

procedure TKGContainer.SetDesigningFocused(Button: TMouseButton; X, Y: Single);
var
  LObj: IKJYUGraphicsObject;
begin
  // 系统调用刷新2
  if Assigned(FKJYUObj) then
  begin
    if PtInRect(FKJYUObj.AbsRect, PointF(X, Y)) then
    begin
      LObj := TOpenKJYUGraphicsObject(FKJYUObj).ObjectAtPoint(PointF(X, Y));
      if Assigned(LObj) then
      begin
        FOldDesignFocused := FNewDesignFocused;
        FNewDesignFocused := LObj;
      end;
    end;
  end;
end;

procedure TKGContainer.DrawDesigningRect;
var
  I: Integer;
begin
  if FDesigningRect <> TRectF.Empty then
  begin
    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.Fill.Color := TAlphaColorRec.Red; // random(2147483647);
    for I := Low(FDesigningPintRects) to High(FDesigningPintRects) do
    begin
      Canvas.FillRect(FDesigningPintRects[I], 0, 0, AllCorners, 100);
    end;
  end;
end;

procedure TKGContainer.ButtonDownOnDesigning(Button: TMouseButton;
  X, Y: Single);
var
  LRefRect: TRectF;
begin
  // 设置当前设计期焦点元件
  SetDesigningFocused(Button, X, Y);
  // 刷新下
  if Assigned(FNewDesignFocused) then
  begin
    FDesigningRect := TKJYUGraphicsObject(FNewDesignFocused).AbsRect;
    FDesigningPintRects := SplitRectFToEightPoint(FDesigningRect,
      KJYU_DESIGNPOINT_SIZE);
    LRefRect := FDesigningRect;
    InflateRect(LRefRect, KJYU_DESIGNPOINT_SIZE, KJYU_DESIGNPOINT_SIZE);
    InvalidateRect(LRefRect);
  end;
  if Assigned(FOldDesignFocused) then
  begin
    LRefRect := TKJYUGraphicsObject(FOldDesignFocused).AbsRect;
    InflateRect(LRefRect, KJYU_DESIGNPOINT_SIZE, KJYU_DESIGNPOINT_SIZE);
    InvalidateRect(LRefRect);
  end;
  //
  if Assigned(FOnKJYUObjSelected) then
  begin
    FOnKJYUObjSelected(TKJYUGraphicsObject(FNewDesignFocused));
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

procedure TKGContainer.SetObjDesignState(Button: TMouseButton; X, Y: Single);
var
  LR, TR, RR, BR: TRectF;
begin
  if TMouseButton.mbLeft = Button then
  begin
    LR := RectF(Self.Left, Self.Top, Self.Left + 2, Self.Top + Self.Height);
    TR := RectF(Self.Left, Self.Top, Self.Left + Self.Width, Self.Top + 2);
    RR := LR;
    OffsetRect(RR, Self.Width - 2, 0);
    BR := TR;
    OffsetRect(BR, 0, Self.Height - 2);
  end;
end;

procedure TKGContainer.SetOnKJYUObjSelected(const Value: TNotifyEvent);
begin
  FOnKJYUObjSelected := Value;
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

{ IKJYURoot }
function TKGContainer.GetRootLeft: Single;
begin
  Result := Self.AbsoluteRect.Left;
end;

function TKGContainer.GetRootTop: Single;
begin
  Result := Self.AbsoluteRect.Top;
end;

{ FormService }
function TKGContainer.GetCaptured: IKJYUGraphicsObject;
begin
  Result := FCaptured;
end;

function TKGContainer.GetFMXMouseService: IFMXMouseService;
begin
  Result := nil;
  TPlatformServices.Current.SupportsPlatformService(IFMXMouseService,
    IInterface(Result));
end;

function TKGContainer.GetFMXWindowsService: IFMXWindowService;
begin
  Result := nil;
  TPlatformServices.Current.SupportsPlatformService(IFMXWindowService,
    IInterface(Result));
end;

function TKGContainer.GetFocused: IKJYUGraphicsObject;
begin
  Result := FFocused;
end;

end.
