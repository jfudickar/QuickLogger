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
unit Quick.Logger.Provider.Dataset;

{$i QuickLib.inc}

interface

uses
  Classes,
  SysUtils,
{$IFDEF MSWINDOWS}
  WinApi.Windows,
{$ENDIF}
{$IFDEF DELPHILINUX}
  Quick.SyncObjs.Linux.Compatibility,
{$ENDIF}
  Quick.Commons,
  Quick.Logger,
  Quick.Logger.Provider.BaseDB, Data.DB;

type

  TLogDatasetProvider = class(TLogBaseDBProvider)
  private
    FLogDataset: TDataset;
    CS: TRTLCriticalSection;
  protected
    procedure AddStringValueToDataset (iFieldName, iFieldValue: string); overload;
    procedure AddDateTimeValueToDataset (iFieldName: string; iFieldValue: TDateTime); overload;
    procedure AddIntegerValueToDataset (iFieldName: string; iFieldValue: LongInt); overload;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Init; override;
    procedure Restart; override;
    procedure WriteLog (cLogItem: TLogItem); override;
    property LogDataset: TDataset read FLogDataset write FLogDataset;
  end;

var
  GlobalLogDatasetProvider: TLogDatasetProvider;

implementation

constructor TLogDatasetProvider.Create;
begin
  inherited;
{$IF Defined(MSWINDOWS) OR Defined(DELPHILINUX)}
  InitializeCriticalSection (CS);
{$ELSE}
  InitCriticalSection (CS);
{$ENDIF}
end;

destructor TLogDatasetProvider.Destroy;
begin
{$IF Defined(MSWINDOWS) OR Defined(DELPHILINUX)}
  DeleteCriticalSection (CS);
{$ELSE}
  DoneCriticalsection (CS);
{$ENDIF}
  inherited;
end;

procedure TLogDatasetProvider.AddStringValueToDataset (iFieldName, iFieldValue: string);
var
  Field: TField;
begin
  Field := LogDataset.FindField (iFieldName);
  if Assigned (Field) then
    if Field.Size > 0 then
      Field.AsString := iFieldValue.Substring (0, Field.Size - 1)
    else
      Field.AsString := iFieldValue;
end;

procedure TLogDatasetProvider.AddDateTimeValueToDataset (iFieldName: string; iFieldValue: TDateTime);
var
  Field: TField;
begin
  Field := LogDataset.FindField (iFieldName);
  if Assigned (Field) then
    Field.AsDateTime := iFieldValue;
end;

procedure TLogDatasetProvider.AddIntegerValueToDataset (iFieldName: string; iFieldValue: LongInt);
var
  Field: TField;
begin
  Field := LogDataset.FindField (iFieldName);
  if Assigned (Field) then
    Field.AsInteger := iFieldValue;
end;

procedure TLogDatasetProvider.Init;
begin
  inherited;
end;

procedure TLogDatasetProvider.Restart;
begin
  Stop;
  Init;
end;

procedure TLogDatasetProvider.WriteLog (cLogItem: TLogItem);
begin
  EnterCriticalSection (CS);
  try
    if not Assigned (LogDataset) or not LogDataset.Active then
      Exit;
    LogDataset.Insert;
    // prepare fields and values for insert query
    AddDateTimeValueToDataset (FieldsMapping.EventDate, cLogItem.EventDate);
    AddStringValueToDataset (FieldsMapping.EventType, cLogItem.EventTypeName);
    AddStringValueToDataset (FieldsMapping.Msg, cLogItem.Msg);
    if iiHost in IncludedInfo then
      AddStringValueToDataset (FieldsMapping.Host, SystemInfo.HostName);
    if iiAppName in IncludedInfo then
      AddStringValueToDataset (FieldsMapping.AppName, SystemInfo.AppName);
    if iiEnvironment in IncludedInfo then
      AddStringValueToDataset (FieldsMapping.Environment, Environment);
    if iiOSVersion in IncludedInfo then
      AddStringValueToDataset (FieldsMapping.OSVersion, SystemInfo.OSVersion);
    if iiPlatform in IncludedInfo then
      AddStringValueToDataset (FieldsMapping.PlatformInfo, PlatformInfo);
    if iiUserName in IncludedInfo then
      AddStringValueToDataset (FieldsMapping.UserName, SystemInfo.UserName);
    if iiThreadId in IncludedInfo then
      AddIntegerValueToDataset (FieldsMapping.ThreadId, cLogItem.ThreadId);
    LogDataset.Post;
  finally
    EnterCriticalSection (CS);
  end;

end;

initialization

GlobalLogDatasetProvider := TLogDatasetProvider.Create;

finalization

if Assigned (GlobalLogDatasetProvider) and (GlobalLogDatasetProvider.RefCount = 0) then
  GlobalLogDatasetProvider.Free;

end.
