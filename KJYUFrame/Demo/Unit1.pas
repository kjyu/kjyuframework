unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  KJYU.UI.Container, System.Math.Vectors, FMX.Controls3D, FMX.Objects3D,
  System.Rtti, FMX.Grid, FMX.Layouts, FMX.TMSListView, FMX.ListBox, FMX.Edit;

type
  TForm1 = class(TForm)
    pnl1: TPanel;
    lst1: TListBox;
    lst2: TListBoxItem;
    lst3: TListBoxItem;
    edt1: TEdit;
    ScrollBox1: TScrollBox;
    ArcDial1: TArcDial;
  private
    { Private declarations }

    { Public declarations }
  end;

var
  Form1: TForm1;
implementation

{$R *.fmx}

end.
