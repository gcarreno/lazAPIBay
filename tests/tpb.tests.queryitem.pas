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

  TTestQueryItem= class(TTestCase)
  private
    FQueryItem: TQueryItem;
  protected
  public
  published
    procedure TestQueryItemCreate;
  end;

implementation

procedure TTestQueryItem.TestQueryItemCreate;
begin
  FQueryItem:= TQueryItem.Create;
  AssertEquals('Query Item Id 0', 0, FQueryItem.Id);
  AssertEquals('Query Item Name empty', '', FQueryItem.Name);
  FQueryItem.Free;
end;



initialization

  RegisterTest(TTestQueryItem);
end.

