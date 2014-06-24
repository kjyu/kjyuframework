unit DesignMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox, FMX.Edit, FMX.TabControl,
  FMX.Objects, FMX.TreeView, Xml.XMLIntf,System.Rtti, System.TypInfo, FMX.ListView.Types,
  FMX.ListView, FMX.TMSMemo, FMX.TMSMemoStyles, FMX.TMSBaseControl, FMX.Memo,
  //
  ProjectUnit, KJYU.UI.Container, KJYU.UI.Classes;
type
  TForm2 = class(TForm)
    tlb1: TToolBar;
    pnlRectangle2: TPanel;
    VertScrollBox1: TVertScrollBox;
    spl1: TSplitter;
    pnl1: TPanel;
    Expander1: TExpander;
    Expander2: TExpander;
    lst1: TListBox;
    lst2: TListBoxItem;
    lst3: TListBoxItem;
    lst4: TListBoxItem;
    tbc1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    tbc2: TTabControl;
    TabItem3: TTabItem;
    TabItem4: TTabItem;
    spl2: TSplitter;
    tbc3: TTabControl;
    TabItem5: TTabItem;
    TabItem6: TTabItem;
    pnl2: TPanel;
    tvStructure: TTreeView;
    tvProject: TTreeView;
    StyleBook1: TStyleBook;
    lstObjInspector: TListBox;
    lstObjEvent: TListBox;
    mmo1: TMemo;
    btn1: TSpeedButton;
    btn2: TSpeedButton;
    btn3: TSpeedButton;
    btnOpenProject: TSpeedButton;
    dlgOpen1: TOpenDialog;
    KGContainer1: TKGContainer;
    procedure KGContainer1KJYUObjSelected(Sender: TObject);
    procedure lst2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOpenProjectClick(Sender: TObject);
  private
    FProjectManager: TProjectManager;
    FLayoutManager: TLayoutManager;
    procedure InitlizDesigner;
    procedure FinalizDesigner;
    procedure LoadStructure;
    procedure LoadProject;
    { Private declarations }
    procedure ClearStructure;
    procedure AnalysisLayoutNodeToItem(ANode: IXMLNode; AParentNode: TTreeViewItem);
    procedure AnalysisProjectNodeToItem(ANode: IXMLNode; AParentNode: TTreeViewItem);
    procedure AddObjPropToListBox(LPropInfo: PPropInfo; LPropValue: string);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation
uses
  codesitelogging;
{$R *.fmx}

procedure TForm2.InitlizDesigner;
begin
  FProjectManager := TProjectManager.Create(Self);
  FLayoutManager := TLayoutManager.Create(self);
end;

procedure TForm2.AnalysisLayoutNodeToItem(ANode: IXMLNode;
  AParentNode: TTreeViewItem);
var
  I: Integer;
  LNode: IXMLNode;
  LObjName: string;
  LItem: TTreeViewItem;
begin
  for I := 0 to ANode.ChildNodes.Count - 1 do
  begin
    LNode := ANode.ChildNodes[I];
    LObjName := LNode.Attributes['name'];
    if Assigned(AParentNode) then
    begin
      LItem := TTreeViewItem.Create(AParentNode);
      LItem.Text := LObjName;
      LItem.Parent := AParentNode;

      if LNode.HasChildNodes then
        AnalysisLayoutNodeToItem(LNode, LItem);
    end;
  end;
end;

procedure TForm2.AnalysisProjectNodeToItem(ANode: IXMLNode;
  AParentNode: TTreeViewItem);
var
  I: Integer;
  LNode: IXMLNode;
  LNodeName: string;
  LItem: TTreeViewItem;
begin
  for I := 0 to ANode.ChildNodes.Count - 1 do
  begin
    LNode := ANode.ChildNodes[I];
    LNodeName := LNode.Attributes['name'];
    if Assigned(AParentNode) then
    begin
      LItem := TTreeViewItem.Create(AParentNode);
      LItem.Text := LNodeName;
      LItem.Parent := AParentNode;
    end;
  end;
end;

procedure TForm2.btnOpenProjectClick(Sender: TObject);
var
  LProjFile: string;
begin
  if dlgOpen1.Execute and (dlgOpen1.FileName <> '') then
  begin
    FProjectManager.FileName := dlgOpen1.FileName;
    LoadProject;
  end;
end;

procedure TForm2.AddObjPropToListBox(LPropInfo: PPropInfo; LPropValue: string);
var
  LlvItem: TListBoxItem;
begin
  LlvItem := TListBoxItem.Create(nil);
  if LPropInfo.PropType^.Kind = tkMethod then
  begin
    LlvItem.Parent := lstObjEvent;
  end
  else
  begin
    LlvItem.Parent := lstObjInspector;
  end;
  LlvItem.StyleLookup := 'propedit';
  LlvItem.StylesData['propname'] := LPropInfo.Name;
  LlvItem.StylesData['editvalue'] := LPropValue;
end;

procedure TForm2.ClearStructure;
begin
  tvStructure.Clear;
end;

procedure TForm2.FinalizDesigner;
begin
  if Assigned(FLayoutManager) then
  begin
    FreeAndNil(FLayoutManager);
  end;
  if Assigned(FProjectManager) then
  begin
    FreeAndNil(FProjectManager);
  end;
end;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FinalizDesigner;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  InitlizDesigner;
end;

procedure TForm2.KGContainer1KJYUObjSelected(Sender: TObject);
var
  LItem: TTreeViewItem;
  LObj: TKJYUGraphicsObject;
  LObj2: TObject;
  LTypeDate: PTypeData;
  LPropList: PPropList;
  LPropInfo: PPropInfo;
  I, LCount: Integer;
  LPropName, LPropValue: string;
  LOrd: integer;
begin
  if tvStructure.Count > 0 then
  begin
    LItem := tvStructure.ItemByText(TKJYUGraphicsObject(Sender).Name);
    if Assigned(LItem) then
    tvStructure.Selected := LItem;
  end;
  //取得属性及事件
  lstObjInspector.Items.Clear;
  lstObjEvent.Items.Clear;
  LObj := TKJYUGraphicsObject(Sender);
  LCount := GetPropList(LObj.ClassInfo, LPropList);
  if LCount > 0 then
  begin
    for I := 0 to LCount - 1 do
    begin
      LPropValue := '';
      LPropInfo := LPropList^[I];
      case LPropInfo.PropType^.Kind of
        tkInteger, tkChar:
          LPropValue := IntToStr(GetOrdProp(LObj, LPropInfo));
        tkSet:
          LPropValue := '(Set...)';
        tkEnumeration :
          begin
            LOrd := GetOrdProp(LObj, LPropInfo);
            LPropValue := GetEnumName(LPropInfo^.PropType^, LOrd);
          end;
        tkFloat: LPropValue := FloatToStr(GetFloatProp(LObj, LPropInfo));// FloatToStrf(GetFloatProp(LObj, LPropInfo), ffnumber, 5, 10);
        tkString, tkLString: LPropValue := GetStrProp(LObj, LPropInfo);
        tkClass:
          begin
            LObj2 := TObject(GetOrdProp(LObj, LPropInfo));
            If Assigned(LObj2) and (LObj2 is TComponent) then
              LPropValue := TComponent(LObj2).Name
            else
              LPropValue := GetTypeData(LPropInfo^.Proptype^)^.ClassType.ClassName;
            if not Assigned(LObj2) then
              LPropValue := LPropValue + ' (Nil)';
          end;
        tkMethod:
          begin
            LPropValue := GetStrProp(LObj, LPropInfo);
          end;
      end;
      AddObjPropToListBox(LPropInfo, LPropValue);
    end;
  end;

end;

procedure TForm2.LoadProject;
var
  LRootNode: TTreeViewItem;
  LProjectRoot: IXMLNode;
begin
  //
  LProjectRoot := FProjectManager.ProjectRootNode;
  if not Assigned(LProjectRoot) then Exit;
  //
  LRootNode := TTreeViewItem.Create(tvProject);
  LRootNode.Text := LProjectRoot.Attributes['name'];
  tvProject.AddObject(LRootNode);
  AnalysisProjectNodeToItem(LProjectRoot, LRootNode);
end;

procedure TForm2.LoadStructure;
var
  LRootNode: TTreeViewItem;
  LLayoutRoot: IXMLNode;
begin
  ClearStructure;
  LLayoutRoot := FLayoutManager.LayoutRootNode;
  if not Assigned(LLayoutRoot) then Exit;
  //
  LRootNode := TTreeViewItem.Create(tvStructure);
  LRootNode.Text := LLayoutRoot.Attributes['name'];
  tvStructure.AddObject(LRootNode);
  AnalysisLayoutNodeToItem(LLayoutRoot, LRootNode);

end;

procedure TForm2.lst2Click(Sender: TObject);
begin
  FLayoutManager.FileName := 'G:\程序\delphi\mydoc\项目\KJYUFrame\Designer\Win32\Debug\test1.xml';
  LoadStructure;
end;

end.
