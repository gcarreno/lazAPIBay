program TestlazAPIBay;

{$mode objfpc}{$H+}

uses
  Classes
, consoletestrunner

, TPB.Tests.Query
, TPB.Tests.QueryItem
;

type

  { TTestlazAPIBayRunner }

  TTestlazAPIBayRunner = class(TTestRunner)
  protected
  // override the protected methods of TTestRunner to customize its behavior
  end;

var
  Application: TTestlazAPIBayRunner;

begin
  Application := TTestlazAPIBayRunner.Create(nil);
  Application.Initialize;
  Application.Title:='testlazapibay';
  Application.Run;
  Application.Free;
end.
