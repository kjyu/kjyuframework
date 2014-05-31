unit KJYU.UI.Inf;

interface
uses
  System.UITypes, System.Classes, System.Types;
type
  IKJYUGraphicsObject = interface
  ['{5C62910B-0C97-4BD2-8E47-2B8F2AA30C7B}']
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
    //
    function ObjectAtPoint(P: TPointF): IKJYUGraphicsObject;
    //
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseMove(Shift: TShiftState; X, Y: Single);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseWheel(Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    //
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  IKJYURoot = interface
    ['{1C38888A-3A4C-4A1D-9616-408B84B06DEE}']
    function GetRootLeft: Single;
    function GetRootTop: Single;
    property RoottLeft: Single read GetRootLeft;
    property RoottTop: Single read GetRootTop;
    function GetFocused: IKJYUGraphicsObject;
    procedure SetFocused(const Value: IKJYUGraphicsObject);
    property Focused: IKJYUGraphicsObject read GetFocused write SetFocused;
    function NewFocusedControl(const Value: IKJYUGraphicsObject): IKJYUGraphicsObject;
  end;

  
implementation

end.
