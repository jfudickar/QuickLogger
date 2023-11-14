unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Quick.Logger.Provider.StringList,
  Quick.Logger.Provider.Memory,
  Quick.Logger;

type
  TForm1 = class(TForm)
    ButtonPanel: TPanel;
    Panel3: TPanel;
    LogMemo: TMemo;
    ClearButton: TButton;
    AddSimpleButton: TButton;
    AddLoopButton: TButton;
    AddRecursionButton: TButton;
    IncEventLevelButton: TButton;
    DecEventLevelButton: TButton;
    procedure AddLoopButtonClick (Sender: TObject);
    procedure AddRecursionButtonClick(Sender: TObject);
    procedure AddSimpleButtonClick (Sender: TObject);
    procedure ClearButtonClick (Sender: TObject);
    procedure DecEventLevelButtonClick(Sender: TObject);
    procedure FormShow (Sender: TObject);
    procedure IncEventLevelButtonClick(Sender: TObject);
  private
    procedure AddSimpleLog;
    procedure ClearLogs;
    procedure SetLogProviderEventTypeNames(iProvider: TLogProviderBase);
    { Private declarations }
  protected
    procedure AddRecursivelLog(iLevel: Integer);
  public
    procedure AddLoopLog;
    procedure InitLogProvider;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}

CONST   EVENTTYPENAMES: TEventTypeNames = ['        ', 'INFO    ', 'SUCCESS ', 'WARNING ', 'ERROR   ', 'CRITICAL',
    'EXCEPT  ', 'DEBUG   ', 'TRACE   ', 'DONE    ', 'CUSTOM1 ', 'CUSTOM2 '];


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

procedure TForm1.AddRecursionButtonClick(Sender: TObject);
begin
  AddRecursivelLog(5);
end;

procedure TForm1.AddRecursivelLog(iLevel: Integer);
begin
  if iLevel <= 0 then
    Exit;
  Logger.StartProcedure('TForm1.AddRecursivelLog %d', [iLevel], etInfo);
  Sleep (Random(100));
  AddSimpleLog;
  AddRecursivelLog(iLevel-1);
  Logger.StopProcedure('TForm1.AddRecursivelLog %d', [iLevel], etInfo);
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
  AddRecursivelLog(3);
end;

procedure TForm1.InitLogProvider;
begin
  GlobalLogStringListProvider.LogList := LogMemo.Lines;
  GlobalLogStringListProvider.Enabled := true;
  GlobalLogStringListProvider.CustomMsgOutput := true;
  GlobalLogStringListProvider.CustomFormatOutput := '%{DATETIME} - [%{LEVEL}] : %{EVENTLEVELBLANKS}%{MESSAGE} # %{EVENTLEVEL} - %{DURATION} ';
  GlobalLogStringListProvider.TimePrecission := true;
  GlobalLogStringListProvider.IncludedInfo := [iiAppName, iiHost, iiUserName, iiEnvironment, iiPlatform, iiOSVersion,
    iiExceptionInfo, iiExceptionStackTrace, iiThreadId];
  GlobalLogStringListProvider.LogLevel := LOG_ALL;
  GlobalLogStringListProvider.SendLimits.MaxEventLevel := 8;
  SetLogProviderEventTypeNames(GlobalLogStringListProvider);
  Logger.Providers.Add (GlobalLogStringListProvider);

end;

procedure TForm1.ClearLogs;
begin
  LogMemo.Clear;
  Log ('Quick.Logger Demo 1 [Event types]', etHeader);
end;

procedure TForm1.AddSimpleLog;
begin
  Logger.StartProcedure('TForm1.AddSimpleLog', etInfo);
  Logger.Info ('Hello world!');
  sleep(random(50));
//  Logger.Error ('An error msg!');
//  sleep(random(50));
//  Logger.Warn ('A warning msg!');
//  sleep(random(50));
//  Logger.Critical ('A critical error!');
//  sleep(random(50));
  Logger.Succ ('Successfully process');
  sleep(random(50));
  Logger.StopProcedure('TForm1.AddSimpleLog', etInfo);
end;

procedure TForm1.DecEventLevelButtonClick(Sender: TObject);
begin
  Logger.DecCurrentEventLevel;
end;

procedure TForm1.IncEventLevelButtonClick(Sender: TObject);
begin
  Logger.IncCurrentEventLevel;
end;

procedure TForm1.SetLogProviderEventTypeNames(iProvider: TLogProviderBase);
var
  et: TEventType;
begin
  for et := low(TEventType) to high(TEventType) do
    iProvider.EventTypeName[et] := EVENTTYPENAMES[Integer(et)];
end;



end.
