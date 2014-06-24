unit KJYU.UI.FuncLib;

interface

uses
  System.Types, System.Classes, System.Rtti, System.TypInfo;
// 计算一个Rect的四个边框检测宽度
procedure SplitRectFToCheckRect(const ARect: TRectF; ABondSize: Single;
  var ALR, ATR, ARR, ABR: TRectF);
function SplitRectFToEightPoint(const ARect: TRectF; ASize: Single)
  : TArray<TRectF>;

implementation

procedure GetClassProperties(AClass: TClass; AStrings: TStrings);
var
  PropCount, I: SmallInt;
  PropList: PPropList;
  PropStr: string;
begin
  PropCount := GetTypeData(AClass.ClassInfo).PropCount;
  GetPropList(AClass.ClassInfo, PropList);
  for I := 0 to PropCount - 1 do
  begin
    case PropList[I]^.PropType^.Kind of
      tkClass:
        PropStr := '[Class] ';
      tkMethod:
        PropStr := '[Method]';
      tkSet:
        PropStr := '[Set]   ';
      tkEnumeration:
        PropStr := '[Enum]  ';
    else
      PropStr := '[Field] ';
    end;
    PropStr := PropStr + PropList[I]^.Name;
    PropStr := PropStr + ': ' + PropList[I]^.PropType^.Name;
    AStrings.Add(PropStr);
  end;
  FreeMem(PropList);
end;

procedure SplitRectFToCheckRect(const ARect: TRectF; ABondSize: Single;
  var ALR, ATR, ARR, ABR: TRectF);
begin
  ALR := TRectF.Empty;
  ATR := TRectF.Empty;
  ARR := TRectF.Empty;
  ABR := TRectF.Empty;
  if (RectWidth(ARect) <= 0) or (RectHeight(ARect) <= 0) or (ABondSize <= 0)
  then
    Exit;

  ALR := RectF(ARect.Left, ARect.Top, ARect.Left + ABondSize,
    ARect.Top + ARect.Height);
  ATR := RectF(ARect.Left, ARect.Top, ARect.Left + ARect.Width,
    ARect.Top + ABondSize);
  ARR := ALR;
  OffsetRect(ARR, ARect.Width - ABondSize, 0);
  ABR := ATR;
  OffsetRect(ABR, 0, ARect.Height - ABondSize);
end;

function SplitRectFToEightPoint(const ARect: TRectF; ASize: Single)
  : TArray<TRectF>;
  function PointToRectF(APointF: TPointF): TRectF;
  begin
    Result := RectF(APointF.X - ASize, APointF.Y - ASize, APointF.X + ASize,
      APointF.Y + ASize);
  end;

var
  LP0, LP1, LP2, LP3, LP4, LP5, LP6, LP7: TPointF;
begin
  SetLength(Result, 8);
  LP0 := ARect.TopLeft;
  LP1 := PointF(LP0.X + RectWidth(ARect) / 2, LP0.Y);
  LP2 := PointF(LP0.X + RectWidth(ARect), LP0.Y);
  LP3 := PointF(LP2.X, LP2.Y + RectHeight(ARect) / 2);
  LP4 := ARect.BottomRight;
  LP5 := PointF(LP1.X, LP4.Y);
  LP6 := PointF(LP0.X, LP4.Y);
  LP7 := PointF(LP0.X, LP3.Y);

  Result[0] := PointToRectF(LP0);
  Result[1] := PointToRectF(LP1);
  Result[2] := PointToRectF(LP2);
  Result[3] := PointToRectF(LP3);
  Result[4] := PointToRectF(LP4);
  Result[5] := PointToRectF(LP5);
  Result[6] := PointToRectF(LP6);
  Result[7] := PointToRectF(LP7);
end;

end.
