{*******************************************************}
{                                                       }
{       KJYFrameWorks                                   }
{       版权所有 (C) 2014 Azure                         }
{-------------------------------------------------------}
{Note:                                                  }
{     FrameWork基类                                     }
{     1.非可视元件：主要是为了方便设计时在对象查看器中  }
{        设置属性                                       }
{     2.可视元件                                        }
{*******************************************************}


unit KJYU.UI.Classes;

interface

uses
  System.Generics.Collections,System.SysUtils,System.Types,FMX.Graphics,
  System.Classes, FMX.Controls, FMX.Types,System.UITypes, FMX.Dialogs,
  //
  KJYU.UI.Types, KJYU.UI.Inf;

type
  TKJYUGraphicsObject = class;
  TKJYUNormalObject = class;
  TKJYUGObjectList = TList<TKJYUGraphicsObject>;
//  TOnPaintEvent = procedure(Sender: TObject; Canvas: TCanvas; const ARect: TRectF) of object;
  //非可视元件
  TKJYUNormalObject = class(TKJYUObject)

  end;
  //可视元件
  TKJYUGraphicsObject = class(TKJYUObject, IKJYUGraphicsObject)
  private
    FAlign: TKJYUAlign;
    FWidth: Single;
    FTop: Single;
    FLeft: Single;
    FHeight: Single;
    FOnPaint: TOnPaintEvent;
    FUpdating: Integer;
    FOnResize: TNotifyEvent;
    FGObjects: TKJYUGObjectList;
    FVisible: Boolean;
    FCaption: string;
    FFocus: Boolean;
    FIsFocused: Boolean;
    FPressed, FDoubleClick: Boolean;
    FEnabled: Boolean;
    FCanFocus: Boolean;
    FAcceptGObject: Boolean;
    FAutoCapture: Boolean;
    procedure SetAlign(const Value: TKJYUAlign);
    procedure SetHeight(const Value: Single);
    procedure SetLeft(const Value: Single);
    procedure SetTop(const Value: Single);
    procedure SetWidth(const Value: Single);
    {draw}
    function GetCanvas: TCanvas;
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
    procedure SetCaption(const Value: string);
    procedure SetOnClick(const Value: TNotifyEvent);
    procedure SetEnabled(const Value: Boolean);
    procedure SetCanFocus(const Value: Boolean);
    procedure SetAcceptGObject(const Value: Boolean);
  strict private
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseWheel: TMouseWheelEvent;
    FOnCanFocus: TCanFocusEvent;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    function GetGObjectCount: Integer;

  protected
    FScene: IScene;
    procedure DoAddObject(const AObj: TKJYUObject); override;
    procedure DoRemoveObject(const AObj: TKJYUObject); override;
    //
    procedure PaintBackGrand; virtual;
    procedure PaintForceGrand; virtual;
    procedure PaintChildren; virtual;
    { align }
    procedure DoRealign; virtual;
    { changes }
    procedure Resize; virtual;
    procedure Show; virtual;
    procedure Hide; virtual;
    //
    procedure PaintInternal;
    {Matrix}
    function GetLocalRect: TRectF; virtual;
    function GetClipRect: TRectF; virtual;
    function GetAbsRect: TRectF; virtual;
    function GetScreenRect: TRectF; virtual;
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    //
    function GetObject: TObject;
    function ObjectAtPoint(P: TPointF): IKJYUGraphicsObject; virtual;
    {Event}
    procedure Capture;
    procedure ReleaseCapture;
    procedure Click;
    procedure DblClick;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean); virtual;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); virtual;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); virtual;
    procedure KeyUp(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); virtual;
  public
    constructor Create(AOwner: TKJYUObject); override;
    destructor Destroy; override;
    procedure SetNewScene(AScene: IScene); virtual;
    {}
    procedure SetFocus;
    function GetCanFocus: Boolean; virtual;
    //
    procedure Paint; virtual;
    procedure DoPaint; virtual;
    procedure AfterPaint; virtual;
    procedure InvalidateRect(ARect: TRectF);
    { align }
    procedure BeginUpdate; virtual;
    function IsUpdating: Boolean; virtual;
    procedure EndUpdate; virtual;
    { IAlignRoot }
    procedure Realign;
    //
    function PointInObject(X, Y: Single): Boolean; virtual;
    {Change}
    property OnResize: TNotifyEvent read FOnResize write FOnResize;

    property Scene: IScene read FScene;

    property GObjectsCount: Integer read GetGObjectCount;
    //event
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnClick: TNotifyEvent read FOnClick write SetOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnCanFocus: TCanFocusEvent read FOnCanFocus write FOnCanFocus;
    //
    property GObjects: TKJYUGObjectList read FGObjects;
    property Canvas: TCanvas read GetCanvas;
    property LocalRect: TRectF read GetLocalRect;    //position-0,0
    property AbsRect: TRectF read GetAbsRect;        //相对于Root容器的坐标
    property ScreenRect: TRectF read GetScreenRect;  //相对于Root容器窗体
  published
    property Left: Single read FLeft write SetLeft;
    property Top: Single read FTop write SetTop;
    property Width: Single read FWidth write SetWidth;
    property Height: Single read FHeight write SetHeight;
    property Align: TKJYUAlign read FAlign write SetAlign;
    property AcceptGObject: Boolean read FAcceptGObject write SetAcceptGObject;
    //
    property OnDraw: TOnPaintEvent read FOnPaint write FOnPaint;
    //
    property Visible: Boolean read GetVisible write SetVisible;
    property Caption: string read FCaption write SetCaption;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property CanFocus: Boolean read FCanFocus write SetCanFocus;
    property AutoCapture: Boolean read FAutoCapture write FAutoCapture default False;
  end;
  procedure LayoutObjects(AObj: TKJYUGraphicsObject);
implementation
//布局,设置内部元件的Left，Top以AObj左上角为原点，
//如果Fit的话Heigh,Width通过比例计算得到
procedure LayoutObjects(AObj: TKJYUGraphicsObject);
var
  I: Integer;
  LObj: TKJYUGraphicsObject;
//
  LL: Single; //left
  LT: Single; //top
  LW: Single; //remote width
  LH: Single; //remote heigh
//
begin
  LL := 0;
  LT := 0;
  LW := AObj.Width;
  LH := AObj.Height;
  for I := 0 to AObj.GObjectsCount - 1 do
  begin
    LObj := AObj.GObjects[I];
    case LObj.Align of
      alNone: {不用调整};
      alLeft:
        begin
          LObj.Left := LL;
          LObj.Top := LT;
          LObj.Height := LH;
          LL := LL + LObj.Width;
          LW := LW - LObj.Width;
        end;
      alRight:
        begin
          LObj.Left := AObj.Width - LObj.Width;
          LObj.Top := LT;
          LObj.Height := LH;
        end;
      alTop:
        begin
          LObj.Left := LL;
          LObj.Top := LT;
          LObj.Width := AObj.Width - LL;
          LT := LT + LObj.Height;
          LH := LH - LObj.Height;
        end;
      alBottom:
        begin
          LObj.Left := LL;
          LObj.Top := LH - LObj.Height;
          LH := LH - LObj.Height;
        end;
      alClient:
        begin
          LObj.Left := LL;
          LObj.Top := LT;
          LObj.Width := LW;
          LObj.Height := LH;
        end;
      alFitLeft: ;
      alFitRight: ;
      alFitTop: ;
      alFitBottom: ;
    end;
    if LObj.GObjectsCount > 0 then
      LayoutObjects(LObj);
  end;
end;
{ TKJYUGraphicsObject }

procedure TKJYUGraphicsObject.DoAddObject(const AObj: TKJYUObject);
var
  LGObj: TKJYUGraphicsObject;
  LIsKJYUGraphicsObj: Boolean;
begin
  inherited;
  if not Assigned(AObj) then Exit;

  if not Assigned(FGObjects) then
  begin
    FGObjects := TKJYUGObjectList.Create;
    FGObjects.Capacity := 10;
  end;

  LIsKJYUGraphicsObj := AObj is TKJYUGraphicsObject;
  if LIsKJYUGraphicsObj then
  begin
    LGObj := AObj as TKJYUGraphicsObject;
    //do something about AKJYUGraphicsObj
    LGObj.Parent := Self;
    LGObj.SetNewScene(Self.FScene);
    LGObj.ChangeParent;
    FGObjects.Add(LGObj);
  end;
end;

procedure TKJYUGraphicsObject.DoPaint;
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self, Canvas, LocalRect);
end;

procedure TKJYUGraphicsObject.DoRealign;
begin
  //布局下
end;

procedure TKJYUGraphicsObject.DoRemoveObject(const AObj: TKJYUObject);
var
  LIdx: integer;
  LGObj: TKJYUGraphicsObject;
begin
  inherited;
  if AObj is TKJYUGraphicsObject then
  begin
    //do something about AKJYUGraphicsObj
    LGObj := AObj as TKJYUGraphicsObject;
    LIdx := FGObjects.IndexOf(LGObj);
    if LIdx >= 0 then
    begin
      LGObj.Parent := nil;
      LGObj.SetNewScene(nil);
      if Assigned(FGObjects) then
        FGObjects.Delete(LIdx);
      //重新布局下
      if LGObj.Align <> TKJYUAlign.alNone then
        Realign;
    end;
  end;
end;

procedure TKJYUGraphicsObject.PaintBackGrand;
begin
  Canvas.BeginScene();
  if not (stDesigning in ObjState) then
  begin
    Canvas.Fill.Kind := TBrushKind.Solid;
    Canvas.Fill.Color := random(2147483647);// TAlphaColorRec.Red;
    Canvas.FillRect(AbsRect,0,0,AllCorners,100);
    Canvas.DrawLine(AbsRect.TopLeft, AbsRect.BottomRight, 100);
  end;
  Canvas.DrawRect(AbsRect, 0, 0, AllCorners, 100);
  Canvas.EndScene;
end;

procedure TKJYUGraphicsObject.PaintForceGrand;
begin

end;

procedure TKJYUGraphicsObject.PaintInternal;
begin
  if (Width < 1) or (Height < 1) then Exit;
  //在Canvas上画图
  //先画自己然后画子元件
  Paint;
  PaintChildren;
  AfterPaint;
  if Assigned(FOnPaint) then
  begin
//    Canvas.SetMatrix(AbsoluteMatrix);
    DoPaint;
  end;
end;

function TKJYUGraphicsObject.PointInObject(X, Y: Single): Boolean;
begin
  Result := PtInRect(AbsRect, PointF(X, Y))
end;

procedure TKJYUGraphicsObject.EndUpdate;
begin
  //子元件也需要EndUpdate
  Dec(FUpdating);
  //重新布局一下
  if not IsUpdating then
     Realign;
end;

function TKJYUGraphicsObject.GetAbsRect: TRectF;
var
  LParentRect: TRectF;
begin
  Result := LocalRect;
  //没有父元件说明是在框架容器上
  if not Assigned(Parent) then
  begin
    Result.Offset(FLeft, FTop);
  end
  else
  begin
    if Parent is TKJYUGraphicsObject then
    begin
      LParentRect :=  TKJYUGraphicsObject(Parent).AbsRect;
      Result.Offset(LParentRect.Left + FLeft, LParentRect.Top + FTop);
    end;
  end;
end;

function TKJYUGraphicsObject.GetCanFocus: Boolean;
begin
  Result := FCanFocus and Enabled {and ParentedVisible};
  if Result and (Assigned(OnCanFocus)) then
  begin
    OnCanFocus(self, Result);
  end;
end;

function TKJYUGraphicsObject.GetCanvas: TCanvas;
begin
  //取得画布
  if Assigned(FScene) then
  begin
    Result := FScene.GetCanvas;
//    FScene.Canvas.SetMatrix();
  end
  else
    Result := nil;
end;

function TKJYUGraphicsObject.GetClipRect: TRectF;
begin
  Result := RectF(0, 0, Width, Height);
end;

function TKJYUGraphicsObject.GetGObjectCount: Integer;
begin
  Result := 0;
  if Assigned(FGObjects) then
    Result := FGObjects.Count;
end;

function TKJYUGraphicsObject.GetLocalRect: TRectF;
begin
  //取得客户区结构体
  Result := RectF(0, 0, FWidth, FHeight);
end;

function TKJYUGraphicsObject.GetObject: TObject;
begin
  Result := Self;
end;

function TKJYUGraphicsObject.GetScreenRect: TRectF;
begin
  Result := AbsRect;
  if Assigned(Root) then
   OffsetRect(Result, Root.RoottLeft, Root.RoottTop);
end;

function TKJYUGraphicsObject.GetVisible: Boolean;
begin
  Result := FVisible;
end;

procedure TKJYUGraphicsObject.Hide;
begin

end;

procedure TKJYUGraphicsObject.InvalidateRect(ARect: TRectF);
var
  LRect: TRectF;
begin
  LRect := ARect;
  FScene.AddUpdateRect(LRect);
end;

function TKJYUGraphicsObject.IsUpdating: Boolean;
begin
  Result := FUpdating > 0;
end;

procedure TKJYUGraphicsObject.KeyDown(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
begin

end;

procedure TKJYUGraphicsObject.KeyUp(var Key: Word; var KeyChar: WideChar;
  Shift: TShiftState);
begin

end;

procedure TKJYUGraphicsObject.MouseClick(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  if FPressed and not(FDoubleClick) and PointInObject(X, Y) then
  begin
    FPressed := False;
    Click;
  end;
end;

procedure TKJYUGraphicsObject.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  InvalidateRect(ScreenRect);
  if (not (stDesigning in ObjState)) and (not FIsFocused) then
    SetFocus;

  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
  if FAutoCapture then
    Capture;
  //
  if (ssDouble in Shift) then
  begin
    DblClick;
    FDoubleClick := True;
  end
  else if Button = TMouseButton.mbLeft then
  begin
    FPressed := True;
  end;
end;

procedure TKJYUGraphicsObject.MouseMove(Shift: TShiftState; X, Y: Single);
begin
//  InvalidateRect(ScreenRect);
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TKJYUGraphicsObject.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Single);
begin
  ReleaseCapture;
  InvalidateRect(ScreenRect);
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);

end;

procedure TKJYUGraphicsObject.MouseWheel(Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  InvalidateRect(ScreenRect);
  if Assigned(FOnMouseWheel) then
    FOnMouseWheel(Self, Shift, WheelDelta, Handled);
end;

function TKJYUGraphicsObject.ObjectAtPoint(P: TPointF): IKJYUGraphicsObject;
var
  LObj, LNewObj: IKJYUGraphicsObject;
  I: Integer;
begin
  if not Visible then
  begin
    Result := nil;
    Exit;
  end;

  if not PointInObject(P.X, P.Y) then
  begin
    Result := nil;
    Exit;
  end;
  if Assigned(FGObjects) then
  for I := FGObjects.Count - 1 downto 0 do
  begin
    LObj := FGObjects[I];
    if not LObj.Visible then Continue;
    LNewObj := LObj.ObjectAtPoint(P);
    if Assigned(LNewObj) then
    begin
      Result := LNewObj;
      Exit;
    end;
  end;

  Result := nil;
  if PointInObject(P.X, P.Y) then
    Result := Self;
end;

procedure TKJYUGraphicsObject.Realign;
begin
  if stDesigning in ObjState then
    Exit;
  if IsUpdating then
    Exit;
  //判断是否需要重新布局
  DoRealign;
end;

procedure TKJYUGraphicsObject.ReleaseCapture;
begin
 if (Root <> nil) and (Root.Captured <> nil) and (Root.Captured.GetObject = Self) then
    Root.SetCaptured(nil);
end;

procedure TKJYUGraphicsObject.Resize;
begin
  if Assigned(FOnResize) then
     FOnResize(Self);
end;

procedure TKJYUGraphicsObject.PaintChildren;
var
  I: Integer;
  LObj: TKJYUGraphicsObject;
begin
  if (FScene <> nil) and (GObjectsCount > 0) then
  begin
    //先计算Rect再画
    //
    for I := 0 to GObjectsCount - 1 do
    begin
      LObj := FGObjects[I];
      if LObj.Visible then
      begin
        FGObjects[I].PaintInternal
      end;
    end;
  end;
end;

procedure TKJYUGraphicsObject.Paint;
var
  R: TRectF;
begin
  if (stDesigning in ObjState) then
  begin
    R := AbsRect;
    InflateRect(R, -0.5, -0.5);
    Canvas.Stroke.Thickness := 1;
    Canvas.Stroke.Dash := TStrokeDash.Dash;
    Canvas.Stroke.Kind := TBrushKind.Solid;
    Canvas.Stroke.Color := $A0909090;
    Canvas.DrawRect(R, 0, 0, AllCorners, 100);
  end
  else
    PaintBackGrand;
end;

procedure TKJYUGraphicsObject.AfterPaint;
begin
  PaintForceGrand;
end;

procedure TKJYUGraphicsObject.BeginUpdate;
begin
  inc(FUpdating);
  //子元件也需要BeginUpDate

end;

procedure TKJYUGraphicsObject.Capture;
begin
  if Assigned(Root) then
    Root.Captured := Self;
end;

procedure TKJYUGraphicsObject.Click;
begin
  ShowMessage(Name);
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

constructor TKJYUGraphicsObject.Create(AOwner: TKJYUObject);
begin
  inherited;
  FAcceptGObject := True;
end;

procedure TKJYUGraphicsObject.DblClick;
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

destructor TKJYUGraphicsObject.Destroy;
begin

  inherited;
end;

procedure TKJYUGraphicsObject.SetAcceptGObject(const Value: Boolean);
begin
  FAcceptGObject := Value;
end;

procedure TKJYUGraphicsObject.SetAlign(const Value: TKJYUAlign);
begin
  FAlign := Value;
end;

procedure TKJYUGraphicsObject.SetCanFocus(const Value: Boolean);
begin
  FCanFocus := Value;
end;

procedure TKJYUGraphicsObject.SetCaption(const Value: string);
begin
  FCaption := Value;
end;

procedure TKJYUGraphicsObject.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TKJYUGraphicsObject.SetFocus;
var
  LObj: IKJYUGraphicsObject;
begin
  if Assigned(Root) then
  begin
    LObj := Root.NewFocusedControl(Self);
    if Assigned(LObj) then
      Root.SetFocused(LObj);
  end;
end;

procedure TKJYUGraphicsObject.SetHeight(const Value: Single);
begin
  FHeight := Value;
end;

procedure TKJYUGraphicsObject.SetLeft(const Value: Single);
begin
  FLeft := Value;
end;

procedure TKJYUGraphicsObject.SetNewScene(AScene: IScene);
var
  I: Integer;
begin
  if not Assigned(AScene) then
    Exit;
  FScene := AScene;
  if Assigned(FGObjects) then
    for I := 0 to FGObjects.Count - 1 do
      FGObjects[I].SetNewScene(FScene);
end;

procedure TKJYUGraphicsObject.SetOnClick(const Value: TNotifyEvent);
begin
  FOnClick := Value;
end;

procedure TKJYUGraphicsObject.SetTop(const Value: Single);
begin
  FTop := Value;
end;

procedure TKJYUGraphicsObject.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

procedure TKJYUGraphicsObject.SetWidth(const Value: Single);
begin
  FWidth := Value;
end;

procedure TKJYUGraphicsObject.Show;
begin

end;

{interface}
function TKJYUGraphicsObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
    if GetInterface(IID, Obj) then Result := S_OK
    else Result := E_NOINTERFACE
end;

function TKJYUGraphicsObject._AddRef: Integer;
begin
  Result := -1;
end;

function TKJYUGraphicsObject._Release: Integer;
begin
  Result := -1;
end;

end.
