{ Implements Tests.QueryItem

  Copyright (c) 2020 Gustavo Carreno <guscarreno@gmail.com>

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}
unit TPB.Tests.QueryItem;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, fpcunit
//, testutils
, testregistry

, TPB.QueryItem
;

type
{ TTestQueryItem }
  TTestQueryItem= class(TTestCase)
  private
    FQueryItem: TQueryItem;

    procedure CheckPropertiesEmpty;
    procedure CheckPropertiesFull;
    procedure CheckPropertiesError;
  protected
  public
  published
    procedure TestQueryItemCreate;
    procedure TestQueryItemCreateFromJSON;
    procedure TestQueryItemCreateFromJSONError;
  end;

implementation

const
  cJSONQueryItem =
    '{' +
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
    '}';

  cJSONQueryItemError =
    '{' +
      '"id": "0",' +
      '"name": "No results returned",' +
      '"info_hash": "0000000000000000000000000000000000000000",' +
      '"leechers": "0",' +
      '"seeders": "0",' +
      '"num_files": "0",' +
      '"size": "0",' +
      '"username": "",' +
      '"added": "0",' +
      '"status": "member",' +
      '"category": "0",' +
      '"imdb": "",' +
      '"total_found": "1"' +
    '}';

procedure TTestQueryItem.CheckPropertiesEmpty;
begin
  AssertEquals('Query Item Id 0', 0, FQueryItem.Id);
  AssertEquals('Query Item Name empty', '', FQueryItem.Name);
  AssertEquals('Query Item Info Hash empty', '', FQueryItem.InfoHash);
  AssertEquals('Query Item Leechers -1', -1, FQueryItem.Leechers);
  AssertEquals('Query Item Seeders -1', -1, FQueryItem.Seeders);
  AssertEquals('Query Item Num Files 0', 0, FQueryItem.NumFiles);
  AssertEquals('Query Item Size 0', 0, FQueryItem.Size);
  AssertEquals('Query Item Username empty', '', FQueryItem.Username);
  AssertEquals('Query Item Added 0.0', 0.0, FQueryItem.Added);
  AssertEquals('Query Item Status empty', '', FQueryItem.Status);
  AssertEquals('Query Item Category -1', -1, FQueryItem.Category);
  AssertEquals('Query Item IMDB empty', '', FQueryItem.IMDB);
  AssertEquals('Query Item Total Found -1', -1, FQueryItem.TotalFound);
end;

procedure TTestQueryItem.CheckPropertiesFull;
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
  AssertEquals('Query Item Total Found -1', -1, FQueryItem.TotalFound);
end;

procedure TTestQueryItem.CheckPropertiesError;
begin
  AssertEquals('Query Item Id 0', 0, FQueryItem.Id);
  AssertEquals('Query Item Name No results returned', 'No results returned', FQueryItem.Name);
  AssertEquals('Query Item Info Hash 0000000000000000000000000000000000000000', '0000000000000000000000000000000000000000', FQueryItem.InfoHash);
  AssertEquals('Query Item Leechers 0', 0, FQueryItem.Leechers);
  AssertEquals('Query Item Seeders 0', 0, FQueryItem.Seeders);
  AssertEquals('Query Item Num Files 0', 0, FQueryItem.NumFiles);
  AssertEquals('Query Item Size 0', 0, FQueryItem.Size);
  AssertEquals('Query Item Username empty', '', FQueryItem.Username);
  AssertEquals('Query Item Added 25569', 25569, FQueryItem.Added);
  AssertEquals('Query Item Status member', 'member', FQueryItem.Status);
  AssertEquals('Query Item Category 0', 0, FQueryItem.Category);
  AssertEquals('Query Item IMDB empty', '', FQueryItem.IMDB);
  AssertEquals('Query Item Total Found 1', 1, FQueryItem.TotalFound);
end;

procedure TTestQueryItem.TestQueryItemCreate;
begin
  FQueryItem:= TQueryItem.Create;
  CheckPropertiesEmpty;
  FQueryItem.Free;
end;

procedure TTestQueryItem.TestQueryItemCreateFromJSON;
begin
  FQueryItem:= TQueryItem.Create(cJSONQueryItem);
  CheckPropertiesFull;
  FQueryItem.Free;
end;

procedure TTestQueryItem.TestQueryItemCreateFromJSONError;
begin
  FQueryItem:= TQueryItem.Create(cJSONQueryItemError);
  CheckPropertiesError;
  FQueryItem.Free;
end;



initialization
  RegisterTest(TTestQueryItem);
end.

