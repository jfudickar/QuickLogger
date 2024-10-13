{ ***************************************************************************

  Copyright (c) 2016-2018 Kike Pérez

  Unit        : Quick.Logger.Provider.ADO
  Description : Log ADO DB Provider
  Author      : Kike Pérez
  Version     : 1.22
  Created     : 21/05/2018
  Modified    : 26/05/2018

  This file is part of QuickLogger: https://github.com/exilon/QuickLogger

 ***************************************************************************

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

 *************************************************************************** }
unit Quick.Logger.Provider.ADODB;

{$i QuickLib.inc}

interface

uses
  Classes,
  SysUtils,
  Data.Win.ADODB,
  Winapi.ActiveX,
  Quick.Commons,
  Quick.Logger,
  Quick.Logger.Provider.BaseDB;

type

  TDBProvider = (dbMSAccess2000, dbMSAccess2007, dbMSSQL, dbMSSQLnc10, dbMSSQLnc11, dbAS400);

const
  {$IFDEF DELPHIXE7_UP}
  ProviderName : array of string = ['Microsoft.Jet.OLEDB.4.0','Microsoft.ACE.OLEDB.12.0','SQLOLEDB.1','SQLNCLI10','SQLNCLI11','IBMDA400'];
  {$ELSE}
  ProviderName : array[0..5] of string = ('Microsoft.Jet.OLEDB.4.0','Microsoft.ACE.OLEDB.12.0','SQLOLEDB.1','SQLNCLI10','SQLNCLI11','IBMDA400');
  {$ENDIF}
type
  {$M+}
  TDBConfig = class
  private
    fDBProvider : TDBProvider;
    fServer : string;
    fDatabase : string;
    fTable : string;
    fUserName : string;
    fPassword : string;
  published
    property Provider : TDBProvider read fDBProvider write fDBProvider;
    property Server : string read fServer write fServer;
    property Database : string read fDatabase write fDatabase;
    property Table : string read fTable write fTable;
    property UserName : string read fUserName write fUserName;
    property Password : string read fPassword write fPassword;
  end;
  {$M-}

  TLogADODBProvider = class (TLogBaseDBProvider)
  private
    fDBConnection : TADOConnection;
    fDBQuery : TADOQuery;
    fConnectionString : string;
    fDBConfig : TDBConfig;
    function CreateConnectionString : string;
    function CreateTable : Boolean;
    procedure AddColumnToTable(const aColumnName, aDataType : string);
    procedure AddToQuery(var aFields, aValues : string; const aNewField, aNewValue : string); overload;
    //procedure AddToQuery(var aFields, aValues : string; const aNewField : string; aNewValue : Integer); overload;
  public
    constructor Create; override;
    destructor Destroy; override;
    property ConnectionString : string read fConnectionString write fConnectionString;
    property DBConfig : TDBConfig read fDBConfig ;
    procedure Init; override;
    procedure Restart; override;
    procedure WriteLog(cLogItem : TLogItem); override;
  end;

var
  GlobalLogADODBProvider : TLogADODBProvider;

implementation

constructor TLogADODBProvider.Create;
begin
  inherited;
  CoInitialize(nil);
  LogLevel := LOG_ALL;
  //set default db fields mapping
  FieldsMapping.EventDate := 'EventDate';
  FieldsMapping.EventType := 'EventType';
  FieldsMapping.Msg := 'Msg';
  FieldsMapping.Environment := 'Environment';
  FieldsMapping.PlatformInfo := 'PlatformInfo';
  FieldsMapping.OSVersion := 'OSVersion';
  FieldsMapping.AppName := 'AppName';
  FieldsMapping.UserName := 'UserName';
  FieldsMapping.Host := 'Host';
  FieldsMapping.ThreadId := 'ThreadId';
  //set default db config
  fDBConfig := TDBConfig.Create;
  fDBConfig.Provider := dbMSSQL;
  fDBConfig.Server := 'localhost';
  fDBConfig.Table := 'Logger';
  IncludedInfo := [iiAppName,iiHost];
end;

function TLogADODBProvider.CreateConnectionString: string;
begin
  Result := Format('Provider=%s;Persist Security Info=False;User ID=%s;Password=%s;Database=%s;Data Source=%s',[
                              ProviderName[Integer(fDBConfig.Provider)],
                              fDBConfig.UserName,
                              fDBConfig.Password,
                              fDBConfig.Database,
                              fDBConfig.Server]);
end;

procedure TLogADODBProvider.AddColumnToTable(const aColumnName, aDataType : string);
begin
  fDBQuery.SQL.Clear;
  //fDBQuery.SQL.Add(Format('IF COL_LENGTH(''%s'', ''%s'') IS NULL',[fDBConfig.Table,aColumnName]));
  fDBQuery.SQL.Add('IF COL_LENGTH(:LOGTABLE,:NEWFIELD) IS NULL');
  fDBQuery.SQL.Add('BEGIN');
  fDBQuery.SQL.Add(Format('ALTER TABLE %s',[fDBConfig.Table]));
  fDBQuery.SQL.Add(Format('ADD %s %s',[aColumnName,aDataType]));
  fDBQuery.SQL.Add('END');
  fDBQuery.Parameters.ParamByName('LOGTABLE').Value := fDBConfig.Table;
  fDBQuery.Parameters.ParamByName('NEWFIELD').Value := aColumnName;
  if fDBQuery.ExecSQL = 0 then raise Exception.Create('Error creating table fields');
end;

function TLogADODBProvider.CreateTable: Boolean;
begin
  fDBQuery.SQL.Clear;
  fDBQuery.SQL.Add('IF NOT EXISTS (SELECT name FROM sys.tables WHERE name = :LOGTABLE)');
  fDBQuery.SQL.Add(Format('CREATE TABLE %s (',[fDBConfig.Table]));
  fDBQuery.SQL.Add(Format('%s DateTime,',[FieldsMapping.EventDate]));
  fDBQuery.SQL.Add(Format('%s varchar(20),',[FieldsMapping.EventType]));
  fDBQuery.SQL.Add(Format('%s varchar(max));',[FieldsMapping.Msg]));
  fDBQuery.Parameters.ParamByName('LOGTABLE').Value := fDBConfig.Table;
  if fDBQuery.ExecSQL = 0 then raise Exception.Create('Error creating table!');

  if iiEnvironment in IncludedInfo then AddColumnToTable(FieldsMapping.Environment,'varchar(50)');
  if iiPlatform in IncludedInfo then AddColumnToTable(FieldsMapping.PlatformInfo,'varchar(50)');
  if iiOSVersion in IncludedInfo then AddColumnToTable(FieldsMapping.OSVersion,'varchar(70)');
  if iiHost in IncludedInfo then AddColumnToTable(FieldsMapping.Host,'varchar(20)');
  if iiUserName in IncludedInfo then AddColumnToTable(FieldsMapping.UserName,'varchar(50)');
  if iiAppName in IncludedInfo then AddColumnToTable(FieldsMapping.AppName,'varchar(70)');
  if iiThreadId in IncludedInfo then AddColumnToTable(FieldsMapping.ThreadId,'varchar(30)');
  Result := True;
end;

destructor TLogADODBProvider.Destroy;
begin
  if Assigned(fDBQuery) then fDBQuery.Free;
  if Assigned(fDBConnection) then fDBConnection.Free;
  fDBConfig.Free;
  CoUninitialize;
  inherited;
end;

procedure TLogADODBProvider.Init;
begin
  fDBConnection := TADOConnection.Create(nil);
  if fConnectionString <> '' then fDBConnection.ConnectionString := fConnectionString
    else fConnectionString := CreateConnectionString;
  fDBConnection.ConnectionString := fConnectionString;
  fDBConnection.LoginPrompt := False;
  fDBConnection.Connected := True;
  fDBQuery := TADOQuery.Create(nil);
  fDBQuery.Connection := fDBConnection;
  CreateTable;
  inherited;
end;

procedure TLogADODBProvider.Restart;
begin
  Stop;
  if Assigned(fDBQuery) then fDBQuery.Free;
  if Assigned(fDBConnection) then fDBConnection.Free;
  Init;
end;

procedure TLogADODBProvider.AddToQuery(var aFields, aValues : string; const aNewField, aNewValue : string);
begin
  aFields := Format('%s,%s',[aFields,aNewField]);
  aValues := Format('%s,''%s''',[aValues,aNewValue]);
end;

//procedure TLogADODBProvider.AddToQuery(var aFields, aValues : string; const aNewField : string; aNewValue : Integer);
//begin
//  aFields := Format('%s,%s',[aFields,aNewField]);
//  aValues := Format('%s,%d',[aValues,aNewValue]);
//end;

procedure TLogADODBProvider.WriteLog(cLogItem : TLogItem);
var
  fields : string;
  values : string;
begin
  //prepare fields and values for insert query
  fields := FieldsMapping.EventDate;
  values := Format('''%s''',[DateTimeToStr(cLogItem.EventDate,FormatSettings)]);
  AddToQuery(fields,values,FieldsMapping.EventType,EventTypeName[cLogItem.EventType]);
  if iiHost in IncludedInfo then AddToQuery(fields,values,FieldsMapping.Host,SystemInfo.HostName);
  if iiAppName in IncludedInfo then AddToQuery(fields,values,FieldsMapping.AppName,SystemInfo.AppName);
  if iiEnvironment in IncludedInfo then AddToQuery(fields,values,FieldsMapping.Environment,Environment);
  if iiOSVersion in IncludedInfo then AddToQuery(fields,values,FieldsMapping.OSVersion,SystemInfo.OsVersion);
  if iiPlatform in IncludedInfo then AddToQuery(fields,values,FieldsMapping.PlatformInfo,PlatformInfo);
  if iiUserName in IncludedInfo then AddToQuery(fields,values,FieldsMapping.UserName,SystemInfo.UserName);
  AddToQuery(fields,values,FieldsMapping.Msg,StringReplace(cLogItem.Msg,'''','"',[rfReplaceAll]));

  fDBQuery.SQL.Clear;
  fDBQuery.SQL.Add(Format('INSERT INTO %s (%s)',[fDBConfig.Table,fields]));
  fDBQuery.SQL.Add(Format('VALUES (%s)',[values]));
  if fDBQuery.ExecSQL = 0 then raise ELogger.Create('[TLogADODBProvider] : Error trying to write to ADODB');
end;

initialization
  GlobalLogADODBProvider := TLogADODBProvider.Create;

finalization
  if Assigned(GlobalLogADODBProvider) and (GlobalLogADODBProvider.RefCount = 0) then GlobalLogADODBProvider.Free;

end.
