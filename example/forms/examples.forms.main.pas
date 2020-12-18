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
    memLog: TMemo;
    psMain: TPairSplitter;
    pssQueryItems: TPairSplitterSide;
    pssLog: TPairSplitterSide;
    panQuery: TPanel;
    procedure actTPBQueryExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  private
    procedure Log(const AMessage: String);
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
, sslsockets
, fpopenssl
{$ENDIF}
;

const
  cBaseURL = 'https://apibay.org/';
  cURLQuery = 'q.php?q=%s&cat=%d';

{ TfrmMain }
procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Log('Satrting');
{$IFDEF LINUX}
  actFileExit.ShortCut := KeyToShortCut(VK_Q, [ssCtrl]);
{$ENDIF}
{$IFDEF WINDOWS}
  actFileExit.ShortCut := KeyToShortCut(VK_X, [ssAlt]);
{$ENDIF}
end;

procedure TfrmMain.Log(const AMessage: String);
begin
  memLog.Append(
    Format(
      '%s - %s',
      [FormatDateTime(
        'yyyy/mm/dd hh:nn:ss',
        Now),
      AMessage]));
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
  Query: TQuery;
begin
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
        Query:= TQuery.Create(slResponse.Text);
        Log(Format('Search returned %d results', [Query.Count]));
        Query.CompressedJSON:= False;
        Log(Query.FormatJSON);
        Query.Free;
      end
      else
      begin
        Log(Format('Error: %d', [http.ResponseStatusCode]));
      end;
    finally
      slResponse.Free;
      http.Free;
    end;
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose:= True;
end;

end.

