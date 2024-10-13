object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Log Sample Form'
  ClientHeight = 776
  ClientWidth = 1166
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 15
  object ButtonPanel: TPanel
    Left = 0
    Top = 0
    Width = 1166
    Height = 41
    Align = alTop
    TabOrder = 0
    object ClearButton: TButton
      Left = 16
      Top = 10
      Width = 75
      Height = 25
      Caption = '&Clear'
      TabOrder = 0
      OnClick = ClearButtonClick
    end
    object AddSimpleButton: TButton
      Left = 111
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Add &Simple'
      TabOrder = 1
      OnClick = AddSimpleButtonClick
    end
    object AddLoopButton: TButton
      Left = 200
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Add &Loop'
      TabOrder = 2
      OnClick = AddLoopButtonClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 413
    Width = 1166
    Height = 363
    Align = alClient
    TabOrder = 1
    object LogDBGrid: TDBGrid
      Left = 1
      Top = 1
      Width = 1164
      Height = 361
      Align = alClient
      DataSource = LogDataSource
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -12
      TitleFont.Name = 'Segoe UI'
      TitleFont.Style = []
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 41
    Width = 1166
    Height = 372
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
    object LogMemo: TMemo
      Left = 1
      Top = 1
      Width = 1164
      Height = 370
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object LogDataSource: TDataSource
    DataSet = LogClientDataSet
    Left = 552
    Top = 528
  end
  object LogClientDataSet: TClientDataSet
    Aggregates = <>
    FieldDefs = <>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 304
    Top = 536
    object LogClientDataSetEventDate: TStringField
      FieldName = 'EventDate'
      Size = 30
    end
    object LogClientDataSetEventType: TStringField
      DisplayWidth = 9
      FieldName = 'EventType'
    end
    object LogClientDataSetMsg: TStringField
      DisplayWidth = 31
      FieldName = 'Msg'
      Size = 10000
    end
    object LogClientDataSetThreadId: TLargeintField
      DisplayWidth = 15
      FieldName = 'ThreadId'
    end
    object LogClientDataSetEnvironment: TStringField
      DisplayWidth = 30
      FieldName = 'Environment'
      Size = 100
    end
    object LogClientDataSetPlatformInfo: TStringField
      DisplayWidth = 15
      FieldName = 'PlatformInfo'
      Size = 100
    end
    object LogClientDataSetOSVersion: TStringField
      DisplayWidth = 20
      FieldName = 'OSVersion'
      Size = 100
    end
    object LogClientDataSetAppName: TStringField
      DisplayWidth = 20
      FieldName = 'AppName'
      Size = 100
    end
    object LogClientDataSetUserName: TStringField
      DisplayWidth = 15
      FieldName = 'UserName'
      Size = 100
    end
    object LogClientDataSetHost: TStringField
      DisplayWidth = 7
      FieldName = 'Host'
      Size = 100
    end
  end
end
