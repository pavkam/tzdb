object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Timezone Visualizer'
  ClientHeight = 460
  ClientWidth = 611
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object gbSelectTZ: TGroupBox
    Left = 0
    Top = 0
    Width = 611
    Height = 92
    Align = alTop
    Caption = 'Select the time zone'
    TabOrder = 0
    DesignSize = (
      611
      92)
    object Label1: TLabel
      Left = 16
      Top = 67
      Width = 83
      Height = 13
      Caption = 'Specify the year:'
    end
    object rbLocal: TRadioButton
      Left = 16
      Top = 16
      Width = 122
      Height = 17
      Caption = 'Local (OS managed)'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbLocalClick
    end
    object rbSelected: TRadioButton
      Left = 16
      Top = 39
      Width = 122
      Height = 17
      Caption = 'Specify custom'
      TabOrder = 1
      OnClick = rbSelectedClick
    end
    object cbbSelectTZ: TComboBox
      Left = 144
      Top = 37
      Width = 462
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 2
      OnChange = cbbSelectTZChange
    end
    object edtYear: TEdit
      Left = 144
      Top = 64
      Width = 81
      Height = 21
      NumbersOnly = True
      TabOrder = 3
      OnChange = edtYearChange
    end
    object btGo: TButton
      Left = 448
      Top = 62
      Width = 158
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Show Timezone information'
      TabOrder = 4
      OnClick = btGoClick
    end
  end
  object pChart: TPanel
    Left = 0
    Top = 92
    Width = 611
    Height = 368
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 1
    object reInfo: TRichEdit
      Left = 1
      Top = 1
      Width = 609
      Height = 366
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      Zoom = 100
    end
  end
end
