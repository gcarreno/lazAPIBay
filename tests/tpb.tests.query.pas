unit TPB.Tests.Query;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, fpcunit
, testregistry

, TPB.Query
, TPB.QueryItem
;

type
{ TTestQuery }
  TTestQuery = class(TTestCase)
  private
    FQuery: TQuery;

    procedure CheckPropertiesFull(const FQueryItem: TQueryItem);
  protected
  public
  published
    procedure TestQueryCreate;
    procedure TestQueryCreateJSON;
    procedure TestQueryIterate;
  end;

implementation

const
  cJSONQuery =
    '[{' +
      '"id": "37231281",' +
      '"name": "NCIS.S18E01.HDTV.x264-PHOENiX[TGx]",' +
      '"info_hash": "84423179B96C05DAE01FFE2EC2681D73AC42EBE3",' +
      '"leechers": "24",' +
      '"seeders": "48",' +
      '"num_files": "2",' +
      '"size": "306706207",' +
      '"username": "sotnikam",' +
      '"added": "1605665933",' +
      '"status": "vip",' +
      '"category": "205",' +
      '"imdb": "tt0364845"' +
    '}]';

procedure TTestQuery.CheckPropertiesFull(const FQueryItem: TQueryItem);
begin
  AssertEquals('Query Item Id 37231281', 37231281, FQueryItem.Id);
  AssertEquals('Query Item Name NCIS.S18E01.HDTV.x264-PHOENiX[TGx]', 'NCIS.S18E01.HDTV.x264-PHOENiX[TGx]', FQueryItem.Name);
  AssertEquals('Query Item Info Hash 84423179B96C05DAE01FFE2EC2681D73AC42EBE3', '84423179B96C05DAE01FFE2EC2681D73AC42EBE3', FQueryItem.InfoHash);
  AssertEquals('Query Item Leechers 24', 24, FQueryItem.Leechers);
  AssertEquals('Query Item Seeders 48', 48, FQueryItem.Seeders);
  AssertEquals('Query Item Num Files 2', 2, FQueryItem.NumFiles);
  AssertEquals('Query Item Size 306706207', 306706207, FQueryItem.Size);
  AssertEquals('Query Item Username sotnikam', 'sotnikam', FQueryItem.Username);
  AssertEquals('Query Item Added 44153.0964', 44153.0964, FQueryItem.Added);
  AssertEquals('Query Item Status vip', 'vip', FQueryItem.Status);
  AssertEquals('Query Item Category 205', 205, FQueryItem.Category);
  AssertEquals('Query Item IMDB tt0364845', 'tt0364845', FQueryItem.IMDB);
end;

{ TTestQuery }
procedure TTestQuery.TestQueryCreate;
begin
  FQuery:= TQuery.Create;
  AssertEquals('Query count 0', 0, FQuery.Count);
  FQuery.Free;
end;

procedure TTestQuery.TestQueryCreateJSON;
begin
  FQuery:= TQuery.Create(cJSONQuery);
  AssertEquals('Query count 1', 1, FQuery.Count);
  CheckPropertiesFull(FQuery[0]);
  FQuery.Free;
end;

procedure TTestQuery.TestQueryIterate;
var
  QueryItem: TQueryItem;
begin
  FQuery:= TQuery.Create(cJSONQuery);
  AssertEquals('Query count 1', 1, FQuery.Count);
  for QueryItem in FQuery do
  begin
    CheckPropertiesFull(QueryItem);
  end;
  FQuery.Free;
end;

initialization
  RegisterTest(TTestQuery);
end.

