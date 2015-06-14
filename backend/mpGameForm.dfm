object GameForm: TGameForm
  Left = 0
  Top = 0
  Caption = 'Select Game'
  ClientHeight = 97
  ClientWidth = 236
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 153
    Top = 65
    Width = 75
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    TabOrder = 0
    ExplicitLeft = 118
    ExplicitTop = 63
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 220
    Height = 51
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Select game'
    TabOrder = 1
    ExplicitWidth = 185
    ExplicitHeight = 49
    object Label1: TLabel
      Left = 16
      Top = 19
      Width = 27
      Height = 13
      Align = alCustom
      Caption = 'Game'
    end
    object ComboBox1: TComboBox
      Left = 100
      Top = 16
      Width = 108
      Height = 21
      Align = alCustom
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemIndex = 0
      TabOrder = 0
      Text = 'TES5'
      Items.Strings = (
        'TES5'
        'TES4'
        'FNV'
        'FO3')
      ExplicitWidth = 73
    end
  end
end
