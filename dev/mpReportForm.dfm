object ReportForm: TReportForm
  Left = 0
  Top = 0
  Caption = 'Submit report'
  ClientHeight = 350
  ClientWidth = 328
  Color = clWindow
  Constraints.MinHeight = 344
  Constraints.MinWidth = 344
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    328
    350)
  PixelsPerInch = 96
  TextHeight = 13
  object gbDetails: TGroupBox
    Left = 8
    Top = 59
    Width = 312
    Height = 252
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Entry'
    TabOrder = 0
    ExplicitWidth = 360
    ExplicitHeight = 424
    DesignSize = (
      312
      252)
    object lblRating: TLabel
      Left = 8
      Top = 20
      Width = 34
      Height = 13
      Caption = 'Rating '
    end
    object lblNotes: TLabel
      Left = 8
      Top = 45
      Width = 28
      Height = 13
      Caption = 'Notes'
    end
    object cbRating: TComboBox
      Left = 120
      Top = 17
      Width = 184
      Height = 21
      Hint = 'Rating hint'
      AutoComplete = False
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemIndex = 5
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = '4: Perfect'
      Items.Strings = (
        '-1: Blacklist'
        '0: Failure'
        '1: Dysfunctional'
        '2: Partially functional'
        '3: Tweaking required'
        '4: Perfect')
      ExplicitWidth = 232
    end
    object meNotes: TMemo
      Left = 8
      Top = 64
      Width = 296
      Height = 180
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Anchors = [akLeft, akTop, akRight, akBottom]
      Lines.Strings = (
        '<Notes>')
      TabOrder = 1
      OnChange = meNotesChange
      ExplicitWidth = 344
      ExplicitHeight = 357
    end
  end
  object pnlTitle: TPanel
    Left = 8
    Top = 8
    Width = 312
    Height = 45
    Anchors = [akLeft, akTop, akRight]
    BevelEdges = []
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    ExplicitWidth = 360
    DesignSize = (
      312
      45)
    object lblFilename: TLabel
      Left = 6
      Top = 3
      Width = 94
      Height = 19
      Anchors = [akLeft, akTop, akRight]
      Caption = '{Filename}'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Layout = tlCenter
    end
    object lblHash: TLabel
      Left = 6
      Top = 28
      Width = 100
      Height = 12
      AutoSize = False
      Caption = 'HASH: 00000000'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object lblRecords: TLabel
      Left = 106
      Top = 28
      Width = 104
      Height = 12
      Alignment = taCenter
      Anchors = [akTop]
      AutoSize = False
      Caption = 'RECORDS: 0'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      Font.Quality = fqDraft
      ParentColor = False
      ParentFont = False
    end
    object lblFlags: TLabel
      Left = 208
      Top = 28
      Width = 98
      Height = 12
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      AutoSize = False
      Caption = 'FLAGS: NAGVIF'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -9
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
    end
  end
  object btnNext: TButton
    Left = 245
    Top = 317
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Next'
    Enabled = False
    TabOrder = 2
    OnClick = btnNextClick
    ExplicitLeft = 293
    ExplicitTop = 489
  end
  object btnPrev: TButton
    Left = 8
    Top = 317
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Previous'
    Enabled = False
    TabOrder = 3
    OnClick = btnPrevClick
    ExplicitTop = 489
  end
end
