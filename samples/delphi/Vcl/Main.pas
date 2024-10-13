unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Datasnap.DBClient, Vcl.Grids, Vcl.DBGrids, Vcl.StdCtrls, Vcl.ExtCtrls,
  Quick.Logger.Provider.StringList, Quick.Logger.Provider.BaseDB, Quick.Logger.Provider.Dataset;

type
  TForm1 = class(TForm)
    ButtonPanel: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    LogMemo: TMemo;
    LogDBGrid: TDBGrid;
    LogDataSource: TDataSource;
    LogClientDataSet: TClientDataSet;
    ClearButton: TButton;
    AddSimpleButton: TButton;
    AddLoopButton: TButton;
    LogClientDataSetEventType: TStringField;
    LogClientDataSetMsg: TStringField;
    LogClientDataSetEnvironment: TStringField;
    LogClientDataSetPlatformInfo: TStringField;
    LogClientDataSetOSVersion: TStringField;
    LogClientDataSetAppName: TStringField;
    LogClientDataSetUserName: TStringField;
    LogClientDataSetHost: TStringField;
    LogClientDataSetThreadId: TLargeintField;
    LogClientDataSetEventDate: TStringField;
    procedure AddLoopButtonClick (Sender: TObject);
    procedure AddSimpleButtonClick (Sender: TObject);
    procedure ClearButtonClick (Sender: TObject);
    procedure FormShow (Sender: TObject);
  private
    procedure AddSimpleLog;
    procedure ClearLogs;
    { Private declarations }
  public
    procedure AddLoopLog;
    procedure InitLogProvider;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Quick.Logger, Quick.Logger.Provider.ADODB;

{$R *.dfm}

procedure TForm1.AddLoopButtonClick (Sender: TObject);
begin
  AddLoopLog;
end;

procedure TForm1.AddLoopLog;
var
  i: Integer;
begin
  for i := 1 to 20 do
  begin
    Logger.Info ('Hello world %d!', [i]);
    Logger.Error ('An error msg %d!', [i]);
    Logger.Warn ('A warning msg %d!', [i]);
    Logger.Critical ('A critical error %d!', [i]);
    Logger.Succ ('Successfully process %d', [i]);
  end;
end;

procedure TForm1.AddSimpleButtonClick (Sender: TObject);
begin
  AddSimpleLog;
end;

procedure TForm1.ClearButtonClick (Sender: TObject);
begin
  ClearLogs;
end;

procedure TForm1.FormShow (Sender: TObject);
begin
  InitLogProvider;
  ClearLogs;
  AddSimpleLog;
end;

procedure TForm1.InitLogProvider;
begin
  GlobalLogStringListProvider.LogList := LogMemo.Lines;
  GlobalLogStringListProvider.Enabled := true;
  GlobalLogStringListProvider.CustomMsgOutput := true;
  GlobalLogStringListProvider.CustomFormatOutput := '%{DATETIME} - [%{LEVEL}] : %{MESSAGE}';
  GlobalLogStringListProvider.TimePrecission := true;
  GlobalLogStringListProvider.IncludedInfo := [iiAppName, iiHost, iiUserName, iiEnvironment, iiPlatform, iiOSVersion,
    iiExceptionInfo, iiExceptionStackTrace, iiThreadId];
  GlobalLogStringListProvider.LogLevel := LOG_ALL;
  Logger.Providers.Add (GlobalLogStringListProvider);

  GlobalLogDatasetProvider.IncludedInfo :=
    [iiThreadId , iiAppName, iiHost, iiUserName, iiEnvironment, iiPlatform, iiOSVersion, iiExceptionInfo,iiExceptionStackTrace  ];
  GlobalLogDatasetProvider.Enabled := true;
  GlobalLogDatasetProvider.LogLevel := LOG_ALL;
  GlobalLogDatasetProvider.TimePrecission := False;
  GlobalLogDatasetProvider.FieldsMapping.EventDate := 'EventDate';
  GlobalLogDatasetProvider.FieldsMapping.EventType := 'EventType';
  GlobalLogDatasetProvider.FieldsMapping.Msg := 'Msg';
  GlobalLogDatasetProvider.FieldsMapping.Environment := 'Environment';
  GlobalLogDatasetProvider.FieldsMapping.PlatformInfo := 'PlatformInfo';
  GlobalLogDatasetProvider.FieldsMapping.OSVersion := 'OSVersion';
  GlobalLogDatasetProvider.FieldsMapping.AppName := 'AppName';
  GlobalLogDatasetProvider.FieldsMapping.UserName := 'UserName';
  GlobalLogDatasetProvider.FieldsMapping.Host := 'Host';
  GlobalLogDatasetProvider.FieldsMapping.ThreadId := 'ThreadId';
  GlobalLogDatasetProvider.LogDataset := LogClientDataSet;
  Logger.Providers.Add (GlobalLogDatasetProvider);
  if not LogClientDataSet.Active then
    LogClientDataSet.CreateDataSet;
end;

procedure TForm1.ClearLogs;
begin
  LogMemo.Clear;
  LogClientDataSet.Close;
  LogClientDataSet.Open;
  Log ('Quick.Logger Demo 1 [Event types]', etHeader);
end;

procedure TForm1.AddSimpleLog;
begin
  Logger.Info ('Hello world!');
  Logger.Error ('An error msg!');
  Logger.Warn ('A warning msg!');
  Logger.Critical ('A critical error!');
  Logger.Succ ('Successfully process');
end;

end.
