{ Implements Forms.Main

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
unit Examples.Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes
, SysUtils
, Forms
, Controls
, Graphics
, Dialogs
, ExtCtrls
, StdCtrls
, ActnList
, StdActns
, PairSplitter

, TPB.Query
, TPB.QueryItem
;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    actTPBQuery: TAction;
    alMain: TActionList;
    btnTPBQuery: TButton;
    edtQuery: TEdit;
    actFileExit: TFileExit;
    lblIMDB: TLabel;
    lblCategory: TLabel;
    lblStatus: TLabel;
    lblAdded: TLabel;
    lblUsername: TLabel;
    lblSize: TLabel;
    lblSeeders: TLabel;
    lblLeechers: TLabel;
    lblHash: TLabel;
    lblName: TLabel;
    lblId: TLabel;
    lbList: TListBox;
    memLog: TMemo;
    psQuery: TPairSplitter;
    pssList: TPairSplitterSide;
    pssData: TPairSplitterSide;
    panData: TPanel;
    psMain: TPairSplitter;
    pssQueryItems: TPairSplitterSide;
    pssLog: TPairSplitterSide;
    panQuery: TPanel;
    procedure actTPBQueryExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbListSelectionChange(Sender: TObject; User: boolean);
  private
    FQuery: TQuery;
    procedure Log(const AMessage: String);
    function fmtSize(aSize: Int64):String;
    procedure UpdateList;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

uses
  LCLType
, fphttpclient
{$IF FPC_FULLVERSION > 30004}
, opensslsockets
{$ELSE}
, openssl
//, sslsockets
//, fpopenssl
{$ENDIF}
;

const
  cBaseURL = 'https://apibay.org/';
  cURLQuery = 'q.php?q=%s&cat=%d';

{ TfrmMain }
procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Log('Satrting');
  FQuery:= nil;
{$IFDEF LINUX}
  actFileExit.ShortCut := KeyToShortCut(VK_Q, [ssCtrl]);
{$ENDIF}
{$IFDEF WINDOWS}
  actFileExit.ShortCut := KeyToShortCut(VK_X, [ssAlt]);
{$ENDIF}
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FQuery) then
  begin
    FQuery.Free;
  end;
end;

procedure TfrmMain.lbListSelectionChange(Sender: TObject; User: boolean);
var
  QueryItem: TQueryItem;
begin
  QueryItem:= FQuery[lbList.ItemIndex];
  lblId.      Caption:= Format('Id: %d', [QueryItem.Id]);
  lblName.    Caption:= Format('Name: %s', [QueryItem.Name]);
  lblHash.    Caption:= Format('Hash: %s', [QueryItem.InfoHash]);
  lblLeechers.Caption:= Format('Leechers: %d', [QueryItem.Leechers]);
  lblSeeders. Caption:= Format('Seeders: %d', [QueryItem.Seeders]);
  lblSize.    Caption:= Format('Size: %s', [fmtSize(QueryItem.Size)]);
  lblUsername.Caption:= Format('Username: %s', [QueryItem.Username]);
  lblAdded.   Caption:= Format('Added: %s', [
    FormatDateTime('yyyy/mm/dd hh:nn:ss', QueryItem.Added)]);
  lblStatus.  Caption:= Format('Status: %s', [QueryItem.Status]);
  lblCategory.Caption:= Format('Category: %d', [QueryItem.Category]);
  lblIMDB.    Caption:= Format('IMDB: %s', [QueryItem.IMDB]);
end;

procedure TfrmMain.Log(const AMessage: String);
var
  slTmp: TStringList;
  sMessage: String;
  sDateTime: String;
begin
  slTmp:= TStringList.Create;
  slTmp.Text:= AMessage;
  sDateTime:= FormatDateTime('yyyy/mm/dd hh:nn:ss', Now);
  for sMessage in slTmp do
  begin
    memLog.Append(sDateTime + ' - ' + sMessage);
  end;
  slTmp.Free;
end;

function TfrmMain.fmtSize(aSize: Int64): String;
var
  dSize: Double;
begin
  Result := '';
  dSize := 0.0;
  if aSize < 1024 then
  begin
    Result := IntToStr(aSize) + ' B';
    exit;
  end;
  if aSize < (1024*1024) then
  begin
    dSize := aSize / 1024;
    Result := FormatFloat('0.##', dSize)+' KB';
    exit;
  end;
  if aSize < (1024*1024*1024) then
  begin
    dSize := aSize / 1024 / 1024;
    Result := FormatFloat('0.##', dSize)+' MB';
    exit;
  end;
  if aSize < (1024*1024*1024*1024) then
  begin
    dSize := aSize / 1024 / 1024 / 1024;
    Result := FormatFloat('0.##', dSize)+' GB';
    exit;
  end;
  if aSize < (1024*1024*1024*1024*1024) then
  begin
    dSize := aSize / 1024 / 1024 / 1024 / 1024;
    Result := FormatFloat('0.##', dSize)+' TB';
  end;
end;

procedure TfrmMain.UpdateList;
var
  QueryItem: TQueryItem;
begin
  if Assigned(FQuery) then
  begin
    lbList.Clear;
    for QueryItem in FQuery do
    begin
      lbList.Items.Add(QueryItem.Name);
    end;
    lblId.      Caption:= 'Id';
    lblName.    Caption:= 'Name:';
    lblHash.    Caption:= 'Hash:';
    lblLeechers.Caption:= 'Leechers:';
    lblSeeders. Caption:= 'Seeders:';
    lblSize.    Caption:= 'Size:';
    lblUsername.Caption:= 'Username:';
    lblAdded.   Caption:= 'Added:';
    lblStatus.  Caption:= 'Status:';
    lblCategory.Caption:= 'Category:';
    lblIMDB.    Caption:= 'IMDB:';
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:= caFree;
end;

procedure TfrmMain.actTPBQueryExecute(Sender: TObject);
var
  http: TFPHTTPClient;
  sURL: String;
  slResponse: TStringList;
begin
  actTPBQuery.Enabled:= False;
  Application.ProcessMessages;
  if Length(edtQuery.Text) > 0 then
  begin
    Log(Format('Searching for "%s"', [edtQuery.Text]));
    sURL:= Format(cBaseURL + cURLQuery, [edtQuery.Text, 200]);
    http:= TFPHTTPClient.Create(Self);
    slResponse:= TStringList.Create;
    try
      http.Get(sURL, slResponse);
      if http.ResponseStatusCode = 200 then
      begin
        if Assigned(FQuery) then
        begin
          FQuery.Free;
        end;
        FQuery:= TQuery.Create(slResponse.Text);
        Log(Format('Search returned %d results', [FQuery.Count]));
        FQuery.CompressedJSON:= False;
        Log(FQuery.FormatJSON);
        UpdateList;
      end
      else
      begin
        Log(Format('Error: %d', [http.ResponseStatusCode]));
      end;
    finally
      slResponse.Free;
      http.Free;
      actTPBQuery.Enabled:= True;
    end;
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:= True;
end;

end.

