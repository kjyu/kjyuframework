unit ProjectUnit;

interface
uses
  System.SysUtils, System.Classes, Xml.XMLIntf, Xml.XMLDoc;
type
  //工程管理器
  TProjectManager = class
  private
    FParent: TComponent;
    FFileName: string;
    FProjectName: string;
    FFileDoc: TXMLDocument;
    procedure SetFileName(const Value: string);
    procedure SetProjectName(const Value: string);
    function GetProjectRootNode: IXMLNode;
    procedure LoadFile;
  public
    constructor Create(AParent: TComponent);
    destructor Destroy; override;
  published
    property FileName: string read FFileName write SetFileName;
    property ProjectName: string read FProjectName write SetProjectName;
    property ProjectRootNode: IXMLNode read GetProjectRootNode;
  end;
  //页面
  TLayoutManager = class
  private
    FParent: TComponent;
    FLayoutName: string;
    FFileName: string;
    FFileDoc: TXMLDocument;
    procedure SetFileName(const Value: string);
    procedure LoadFile;
    function GetLayoutRootNode: IXMLNode;
  public
    constructor Create(AParent: TComponent);
    destructor Destroy; override;
    {节点操作}
    function GetNodeByObjName(const AName: string): IXMLNode;
  published
    property FileName: string read FFileName write SetFileName;
    property LayoutName: string read FLayoutName;
    property LayoutRootNode: IXMLNode read GetLayoutRootNode;
  end;
implementation

{ TLayoutManager }

constructor TLayoutManager.Create(AParent: TComponent);
begin
  FParent := AParent;
  FFileDoc := TXMLDocument.Create(FParent);
end;

destructor TLayoutManager.Destroy;
begin
  if Assigned(FFileDoc) then
  begin
    FreeAndNil(FFileDoc);
  end;
  inherited;
end;

function TLayoutManager.GetLayoutRootNode: IXMLNode;
begin
  Result := nil;

  if FFileDoc.ChildNodes.Count > 1 then
  begin
    if FFileDoc.ChildNodes[1].NodeName = 'Layout' then
    begin
      Result := FFileDoc.ChildNodes[1];
    end;
  end;
end;

function TLayoutManager.GetNodeByObjName(const AName: string): IXMLNode;
begin

end;

procedure TLayoutManager.LoadFile;
begin
  FFileDoc.LoadFromFile(FFileName);
end;

procedure TLayoutManager.SetFileName(const Value: string);
begin
  if FFileName <> Value then
  begin
    FFileName := Value;
    //加载布局文件
    LoadFile;
  end;
end;

{ TProjectManager }

constructor TProjectManager.Create(AParent: TComponent);
begin
  FParent := AParent;
  FFileDoc := TXMLDocument.Create(FParent);
end;

destructor TProjectManager.Destroy;
begin
  if Assigned(FFileDoc) then
  begin
    FreeAndNil(FFileDoc);
  end;
  inherited;
end;

procedure TProjectManager.LoadFile;
begin
  FFileDoc.LoadFromFile(FFileName);
end;

procedure TProjectManager.SetFileName(const Value: string);
begin
 if FFileName <> Value then
  begin
    FFileName := Value;
    //加载布局文件
    LoadFile;
  end;
end;

procedure TProjectManager.SetProjectName(const Value: string);
begin
  FProjectName := Value;
end;

function TProjectManager.GetProjectRootNode: IXMLNode;
begin
  Result := nil;

  if FFileDoc.ChildNodes.Count > 1 then
  begin
    if FFileDoc.ChildNodes[1].NodeName = 'Project' then
    begin
      Result := FFileDoc.ChildNodes[1];
    end;
  end;
end;

end.
