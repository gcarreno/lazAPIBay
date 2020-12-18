unit TPB.QueryItem;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, fpjson
;

type
{ Exceptions }
  EItemWrongJSONObject = class(Exception);
{ TQueryItem }
  TQueryItem = class(TObject)
  private
    FCompressedJSON: Boolean;
    FId: Int64;
    FName: String;
    FInfoHash: String;
    FLeechers: Integer;
    FSeeders: Integer;
    FNumFiles: Integer;
    FSize: Int64;
    FUsername: String;
    FAdded: TDateTime;
    FStatus: String;
    FCategory: Integer;
    FIMDB: String;
    FTotalFound: Integer;

    procedure setFAdded(AValue: TDateTime);
    procedure setFCategory(AValue: Integer);
    procedure setFId(AValue: Int64);
    procedure setFIMDB(AValue: String);
    procedure setFInfoHash(AValue: String);
    procedure setFLeechers(AValue: Integer);
    procedure setFName(AValue: String);
    procedure setFNumFiles(AValue: Integer);
    procedure setFSeeders(AValue: Integer);
    procedure setFSize(AValue: Int64);
    procedure setFStatus(AValue: String);
    procedure setFUsername(AValue: String);

    procedure setFromJSON(const AJSON: String);
    procedure setFromJSONObject(const AJSONObject: TJSONObject);
    procedure setFromJSONData(const AJSONData: TJSONData);
    procedure setFromStream(const AStream: TStream);

    function getAsJSON: String;
    function getAsJSONObject: TJSONObject;
    function getAsJSONData: TJSONData;
    function getAsStream: TStream;
    procedure setTotalFound(AValue: Integer);
  protected
  public
    constructor Create;
    constructor Create(const AJSON: String);
    constructor Create(const AJSONObject: TJSONObject);
    constructor Create(const AJSONData: TJSONData);
    constructor Create(const AStream: TStream);

    function FormatJSON(AOptions : TFormatOptions = DefaultFormat;
      AIndentsize : Integer = DefaultIndentSize): String;

    property CompressedJSON: Boolean
      read FCompressedJSON
      write FCompressedJSON;
    property Id: Int64
      read FId
      write setFId;
    property Name: String
      read FName
      write setFName;
    property InfoHash: String
      read FInfoHash
      write setFInfoHash;
    property Leechers: Integer
      read FLeechers
      write setFLeechers;
    property Seeders: Integer
      read FSeeders
      write setFSeeders;
    property NumFiles: Integer
      read FNumFiles
      write setFNumFiles;
    property Size: Int64
      read FSize
      write setFSize;
    property Username: String
      read FUsername
      write setFUsername;
    property Added: TDateTime
      read FAdded
      write setFAdded;
    property Status: String
      read FStatus
      write setFStatus;
    property Category: Integer
      read FCategory
      write setFCategory;
    property IMDB: String
      read FIMDB
      write setFIMDB;
    property TotalFound: Integer
      read FTotalFound
      write setTotalFound;

    property AsJSON: String
      read getAsJSON;
    property AsJSONData: TJSONData
      read getAsJSONData;
    property AsJSONObject: TJSONObject
      read getAsJSONObject;
    property AsStream: TStream
      read getAsStream;
  published
  end;

implementation

uses
  DateUtils
, TPB.JSON.Utils
;

const
  cJSONId = 'id';
  cJSONName = 'name';
  cJSONInfoHash = 'info_hash';
  cJSONLeechers = 'leechers';
  cJSONSeeders = 'seeders';
  cJSONNumFiles = 'num_files';
  cJSONSize = 'size';
  cJSONUsername = 'username';
  cJSONAdded = 'added';
  cJSONStatus = 'status';
  cJSONCategory = 'category';
  cJSONIMDB = 'imdb';
  cJSONTotalFound = 'total_found';

{ TQueryItem }

procedure TQueryItem.setFId(AValue: Int64);
begin
  if FId=AValue then Exit;
  FId:=AValue;
end;

procedure TQueryItem.setFIMDB(AValue: String);
begin
  if FIMDB=AValue then Exit;
  FIMDB:=AValue;
end;

procedure TQueryItem.setFAdded(AValue: TDateTime);
begin
  if FAdded=AValue then Exit;
  FAdded:=AValue;
end;

procedure TQueryItem.setFCategory(AValue: Integer);
begin
  if FCategory=AValue then Exit;
  FCategory:=AValue;
end;

procedure TQueryItem.setFInfoHash(AValue: String);
begin
  if FInfoHash=AValue then Exit;
  FInfoHash:=AValue;
end;

procedure TQueryItem.setFLeechers(AValue: Integer);
begin
  if FLeechers=AValue then Exit;
  FLeechers:=AValue;
end;

procedure TQueryItem.setFName(AValue: String);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TQueryItem.setFNumFiles(AValue: Integer);
begin
  if FNumFiles=AValue then Exit;
  FNumFiles:=AValue;
end;

procedure TQueryItem.setFSeeders(AValue: Integer);
begin
  if FSeeders=AValue then Exit;
  FSeeders:=AValue;
end;

procedure TQueryItem.setFSize(AValue: Int64);
begin
  if FSize=AValue then Exit;
  FSize:=AValue;
end;

procedure TQueryItem.setFStatus(AValue: String);
begin
  if FStatus=AValue then Exit;
  FStatus:=AValue;
end;

procedure TQueryItem.setFUsername(AValue: String);
begin
  if FUsername=AValue then Exit;
  FUsername:=AValue;
end;

procedure TQueryItem.setFromJSON(const AJSON: String);
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

procedure TQueryItem.setFromJSONObject(const AJSONObject: TJSONObject);
var
  sTmp: String;
begin
  sTmp:=AJSONObject.Get(cJSONId, '0');
  FId:= StrToInt(sTmp);
  FName:= AJSONObject.Get(cJSONName, FName);
  FInfoHash:= AJSONObject.Get(cJSONInfoHash, FInfoHash);
  sTmp:= AJSONObject.Get(cJSONLeechers, '-1');
  FLeechers:= StrToInt(sTmp);
  sTmp:= AJSONObject.Get(cJSONSeeders, '-1');
  FSeeders:= StrToInt(sTmp);
  sTmp:= AJSONObject.Get(cJSONNumFiles, '0');
  FNumFiles:= StrToInt(sTmp);
  sTmp:= AJSONObject.Get(cJSONSize, '0');
  FSize:= StrToInt(sTmp);
  FUsername:= AJSONObject.Get(cJSONUsername, FUsername);
  sTmp:= AJSONObject.Get(cJSONAdded, '0');
  FAdded:= UnixToDateTime(StrToInt(sTmp));
  FStatus:= AJSONObject.Get(cJSONStatus, FStatus);
  sTmp:= AJSONObject.Get(cJSONCategory, '-1');
  FCategory:= StrToInt(sTmp);
  FIMDB:= AJSONObject.Get(cJSONIMDB, FIMDB);
  sTmp:= AJSONObject.Get(cJSONTotalFound, '-1');
  FTotalFound:= StrToInt(sTmp);
end;

procedure TQueryItem.setFromJSONData(const AJSONData: TJSONData);
begin
  if AJSONData.JSONType <> jtObject then
  begin
    raise EItemWrongJSONObject.Create('JSON data is not an object');
  end;
  setFromJSONObject(AJSONData as TJSONObject);
end;

procedure TQueryItem.setFromStream(const AStream: TStream);
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

function TQueryItem.getAsJSON: String;
var
  jObject: TJSONObject;
begin
  Result:= '';
  jObject:= getAsJSONObject;
  jObject.CompressedJSON:= FCompressedJSON;
  Result:= jObject.AsJSON;
end;

function TQueryItem.getAsJSONObject: TJSONObject;
begin
  Result:= TJSONObject.Create;
  Result.Add(cJSONId, IntToStr(FId));
  Result.Add(cJSONName, FName);
  Result.Add(cJSONInfoHash, FInfoHash);
  Result.Add(cJSONLeechers, IntToStr(FLeechers));
  Result.Add(cJSONSeeders, IntToStr(FSeeders));
  Result.Add(cJSONNumFiles, IntToStr(FNumFiles));
  Result.Add(cJSONSize, IntToStr(FSize));
  Result.Add(cJSONUsername, FUsername);
  Result.Add(cJSONAdded, IntToStr(DateTimeToUnix(FAdded)));
  Result.Add(cJSONStatus, FStatus);
  Result.Add(cJSONCategory, IntToStr(FCategory));
  Result.Add(cJSONIMDB, FIMDB);
  Result.Add(cJSONTotalFound, IntToStr(FTotalFound))
end;

function TQueryItem.getAsJSONData: TJSONData;
begin
  Result:= getAsJSONObject as TJSONData;
end;

function TQueryItem.getAsStream: TStream;
begin
  Result:= TStringStream.Create(getAsJSON);
end;

procedure TQueryItem.setTotalFound(AValue: Integer);
begin
  if FTotalFound=AValue then Exit;
  FTotalFound:=AValue;
end;

constructor TQueryItem.Create;
begin
  FId:= 0;
  FName:= '';
  FInfoHash:= '';
  FLeechers:= -1;
  FSeeders:= -1;
  FNumFiles:= 0;
  FSize:= 0;
  FUsername:= '';
  FAdded:= 0.0;
  FStatus:= '';
  FCategory:= -1;
  FIMDB:= '';
  FTotalFound:= -1;
end;

constructor TQueryItem.Create(const AJSON: String);
begin
  Create;
  setFromJSON(AJSON);
end;

constructor TQueryItem.Create(const AJSONObject: TJSONObject);
begin
  Create;
  setFromJSONObject(AJSONObject);
end;

constructor TQueryItem.Create(const AJSONData: TJSONData);
begin
  Create;
  setFromJSONData(AJSONData);
end;

constructor TQueryItem.Create(const AStream: TStream);
begin
  Create;
  setFromStream(AStream);
end;

function TQueryItem.FormatJSON(AOptions: TFormatOptions;
  AIndentsize: Integer): String;
begin
  Result:= getAsJSONObject.FormatJSON(AOptions, AIndentsize);
end;

end.

