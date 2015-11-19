object PluginSelectionForm: TPluginSelectionForm
  Left = 0
  Top = 0
  Caption = 'Plugin Selection'
  ClientHeight = 514
  ClientWidth = 430
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lvPlugins: TListView
    Left = 8
    Top = 8
    Width = 414
    Height = 467
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Checkboxes = True
    Columns = <>
    DoubleBuffered = True
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    ParentDoubleBuffered = False
    TabOrder = 0
    ViewStyle = vsReport
  end
  object btnCancel: TButton
    Left = 347
    Top = 481
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 266
    Top = 481
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
    OnClick = btnOKClick
  end
end
