unit TPB.Query;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, Contnrs
, fpjson

, TPB.QueryItem
;

type
{ Exceptions }
  EQueryWrongJSONArray = class(Exception);

TQueryEnumerator = class; // Forward
{ TQuery }
  TQuery = class(TFPObjectList)
  private
    // Compressed JSON field to mimic TJSON one
    FCompressedJSON: Boolean;

    function GetByIndex(const Index: Integer): TQueryItem;
    procedure SetByIndex(const Index: Integer; const AValue: TQueryItem);

    procedure setFromJSON(const AJSON: String);
    procedure setFromJSONData(const AJSONData: TJSONData);
    procedure setFromJSONArray(const AJSONArray: TJSONArray);
    procedure setFromStream(const AStream: TStream);

    function getAsJSON: String;
    function getAsJSONData: TJSONData;
    function getAsJSONArray: TJSONArray;
    function getAsStream: TStream;

  protected
  public
    constructor Create;
    constructor Create(const AJSON: String);
    constructor Create(const AJSONData: TJSONData);
    constructor Create(const AJSONArray: TJSONArray);
    constructor Create(const AStream: TStream);

    destructor Destroy; override;

    function FormatJSON(AOptions : TFormatOptions = DefaultFormat;
      AIndentsize : Integer = DefaultIndentSize): String;

    function GetEnumerator: TQueryEnumerator;

    property Items[Index: Integer]: TQueryItem
      read GetByIndex
      write SetByIndex; default;

    property CompressedJSON: Boolean
      read FCompressedJSON
      write FCompressedJSON;


    property AsJSON: String
      read getAsJSON;
    property AsJSONData: TJSONData
      read getAsJSONData;
    property AsJSONArray: TJSONArray
      read getAsJSONArray;
    property AsStream: TStream
      read getAsStream;
  end;

{ TQueryEnumerator }
  TQueryEnumerator = class(TObject)
  private
    FQuery: TQuery;
    FPosition: Integer;
  protected
  public
    constructor Create(const AQuery: TQuery);
    function GetCurrent: TQueryItem;
    function MoveNext: Boolean;

    property Current: TQueryItem
      read GetCurrent;
  published
  end;

implementation

uses
  TPB.JSON.Utils
;

{ TQuery }
function TQuery.GetByIndex(const Index: Integer): TQueryItem;
begin
  Result := inherited Items[index] as TQueryItem;
end;

procedure TQuery.SetByIndex(const Index: Integer; const AValue: TQueryItem);
begin
  inherited Items[Index]:= AValue;
end;

procedure TQuery.setFromJSON(const AJSON: String);
var
  jData: TJSONData;
begin
  jData:= GetJSONData(AJSON);
  try
    setFromJSONData(jData);
  finally
    jData.Free;
  end;
end;

procedure TQuery.setFromJSONData(const AJSONData: TJSONData);
begin
  if aJSONData.JSONType <> jtArray then
  begin
    raise EQueryWrongJSONArray.Create('JSON data is not an array');
  end;
  setFromJSONArray(aJSONData as TJSONArray);
end;

procedure TQuery.setFromJSONArray(const AJSONArray: TJSONArray);
var
  Index: Integer;
begin
  for Index:= 0 to Pred(AJSONArray.Count) do
  begin
    Add(TQueryItem.Create(AJSONArray[Index]));
  end;
end;

procedure TQuery.setFromStream(const AStream: TStream);
var
  jData: TJSONData;
begin
  jData:= GetJSONData(AStream);
  try
    setFromJSONData(jData);
  finally
    jData.Free;
  end;
end;

function TQuery.getAsJSON: String;
var
  jArray: TJSONArray;
begin
  Result:= '';
  jArray:= getAsJSONArray;
  jArray.CompressedJSON:= FCompressedJSON;
  Result:= jArray.AsJSON;
end;

function TQuery.getAsJSONData: TJSONData;
begin
  Result:= getAsJSONArray as TJSONData;
end;

function TQuery.getAsJSONArray: TJSONArray;
var
  QueryItem: TQueryItem;
begin
  Result:= TJSONArray.Create;
  for QueryItem in Self do
  begin
    Result.Add(QueryItem.AsJSONObject);
  end;
end;

function TQuery.getAsStream: TStream;
begin
  Result:= TStringStream.Create(getAsJSON);
end;

constructor TQuery.Create;
begin
  inherited Create(True);

  FCompressedJSON:= True;
end;

constructor TQuery.Create(const AJSON: String);
begin
  Create;
  setFromJSON(AJSON);
end;

constructor TQuery.Create(const AJSONData: TJSONData);
begin
  Create;
  setFromJSONData(AJSONData);
end;

constructor TQuery.Create(const AJSONArray: TJSONArray);
begin
  Create;
  setFromJSONArray(AJSONArray);
end;

constructor TQuery.Create(const AStream: TStream);
begin
  Create;
  setFromStream(AStream);
end;

destructor TQuery.Destroy;
begin
  inherited Destroy;
end;

function TQuery.FormatJSON(AOptions: TFormatOptions;
  AIndentsize: Integer): String;
begin
  Result:= getAsJSONArray.FormatJSON(AOptions, AIndentsize);
end;

function TQuery.GetEnumerator: TQueryEnumerator;
begin
  Result:= TQueryEnumerator.Create(Self);
end;

{ TQueryEnumerator }
constructor TQueryEnumerator.Create(const AQuery: TQuery);
begin
  inherited Create;
  FQuery := AQuery;
  FPosition := -1;
end;

function TQueryEnumerator.GetCurrent: TQueryItem;
begin
  Result:= FQuery[FPosition];
end;

function TQueryEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FQuery.Count;
end;

end.

