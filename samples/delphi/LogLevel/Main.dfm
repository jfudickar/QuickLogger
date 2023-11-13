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
    object AddRecursionButton: TButton
      Left = 288
      Top = 10
      Width = 97
      Height = 25
      Caption = 'Add &Recursion'
      TabOrder = 3
      OnClick = AddRecursionButtonClick
    end
    object IncEventLevelButton: TButton
      Left = 391
      Top = 11
      Width = 97
      Height = 25
      Caption = '&Inc Event Level'
      TabOrder = 4
      OnClick = IncEventLevelButtonClick
    end
    object DecEventLevelButton: TButton
      Left = 494
      Top = 11
      Width = 97
      Height = 25
      Caption = '&Dec Event Level'
      TabOrder = 5
      OnClick = DecEventLevelButtonClick
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 41
    Width = 1166
    Height = 735
    Align = alClient
    TabOrder = 1
    object LogMemo: TMemo
      Left = 1
      Top = 1
      Width = 1164
      Height = 733
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
end
