{ ***************************************************************************

  Copyright (c) 2023 Jens Fudickar

  Unit        : Quick.Logger.Provider.BaseDB
  Description : Base classes for all database log providers
  Author      : Jens Fudickar
  Version     : 1.0

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
unit Quick.Logger.Provider.BaseDB;

{$i QuickLib.inc}

interface

uses
  Classes,
  SysUtils,
  Quick.Logger;

type

  TLogDbFieldsMapping = class
  private
    fEventDate : string;
    fEventType : string;
    fMsg : string;
    fEnvironment : string;
    fPlatformInfo : string;
    fOSVersion : string;
    fAppName : string;
    fUserName : string;
    fHost : string;
    FThreadId: string;
  public
    property EventDate : string read fEventDate write fEventDate;
    property EventType : string read fEventType write fEventType;
    property Msg : string read fMsg write fMsg;
    property Environment : string read fEnvironment write fEnvironment;
    property PlatformInfo : string read fPlatformInfo write fPlatformInfo;
    property OSVersion : string read fOSVersion write fOSVersion;
    property AppName : string read fAppName write fAppName;
    property UserName : string read fUserName write fUserName;
    property Host : string read fHost write fHost;
    property ThreadId: string read FThreadId write FThreadId;
  end;


  TLogBaseDBProvider = class (TLogProviderBase)
  private
    FFieldsMapping: TLogDbFieldsMapping;
  public
    constructor Create; override;
    destructor Destroy; override;
    property FieldsMapping: TLogDbFieldsMapping read FFieldsMapping;
  end;

implementation

constructor TLogBaseDBProvider.Create;
begin
  inherited;
  //create default db fields mapping
  fFieldsMapping := TLogDbFieldsMapping.Create;
end;


destructor TLogBaseDBProvider.Destroy;
begin
  fFieldsMapping.Free;
  inherited;
end;

end.
