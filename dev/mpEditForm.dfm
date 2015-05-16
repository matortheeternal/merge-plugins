object EditForm: TEditForm
  Left = 0
  Top = 0
  Caption = 'Edit Merge'
  ClientHeight = 240
  ClientWidth = 334
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnShow = FormShow
  DesignSize = (
    334
    240)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 318
    Height = 193
    ActivePage = TabSheet1
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Edit Merge'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 281
      ExplicitHeight = 0
      object lblName: TLabel
        Left = 13
        Top = 13
        Width = 27
        Height = 13
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Caption = 'Name'
        Transparent = True
      end
      object lblFilename: TLabel
        Left = 13
        Top = 42
        Width = 42
        Height = 13
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Caption = 'Filename'
      end
      object lblMethod: TLabel
        Left = 13
        Top = 71
        Width = 69
        Height = 13
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Caption = 'Merge Method'
      end
      object lblRenumbering: TLabel
        Left = 13
        Top = 100
        Width = 63
        Height = 13
        Margins.Left = 8
        Margins.Top = 8
        Margins.Right = 8
        Margins.Bottom = 8
        Caption = 'Renumbering'
      end
      object edName: TEdit
        Left = 109
        Top = 8
        Width = 198
        Height = 21
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object edFilename: TEdit
        Left = 109
        Top = 39
        Width = 198
        Height = 21
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object cbMethod: TComboBox
        Left = 109
        Top = 66
        Width = 198
        Height = 21
        Align = alCustom
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 0
        TabOrder = 2
        Text = 'Overrides'
        Items.Strings = (
          'Overrides'
          'New records')
      end
      object cbRenumbering: TComboBox
        Left = 109
        Top = 97
        Width = 198
        Height = 21
        Align = alCustom
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemIndex = 0
        TabOrder = 3
        Text = 'Conflicting'
        Items.Strings = (
          'Conflicting'
          'All')
      end
    end
  end
  object btnOk: TButton
    Left = 170
    Top = 207
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 251
    Top = 207
    Width = 75
    Height = 25
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
