object ResolveForm: TResolveForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Resolve merge issues'
  ClientHeight = 417
  ClientWidth = 584
  Color = clBtnFace
  Constraints.MaxHeight = 600
  Constraints.MaxWidth = 800
  Constraints.MinHeight = 455
  Constraints.MinWidth = 600
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
  object ResolvePageControl: TPageControl
    Left = 8
    Top = 8
    Width = 568
    Height = 370
    ActivePage = tsBrokenDependencies
    TabOrder = 0
    TabWidth = 140
    object tsBrokenDependencies: TTabSheet
      Caption = 'Broken dependencies'
      object lblBrokenDependencies: TLabel
        Left = 3
        Top = 13
        Width = 550
        Height = 28
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'Your merge breaks the dependencies of other plugins in your load' +
          ' order.  You can either remove the problematic plugins from the ' +
          'merge or add dependent plugins to the merge.'
        WordWrap = True
      end
      object lvBrokenDependencies: TListView
        Left = 3
        Top = 47
        Width = 550
        Height = 292
        Align = alCustom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Plugin filename'
            Width = 250
          end
          item
            AutoSize = True
            Caption = 'Broken dependency'
          end>
        ColumnClick = False
        DoubleBuffered = True
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        PopupMenu = DependenciesPopupMenu
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tsNonContiguous: TTabSheet
      Caption = 'Non-contiguous plugins'
      ImageIndex = 1
      object lblNonContiguous: TLabel
        Left = 3
        Top = 13
        Width = 550
        Height = 44
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'Your merge has non-contiguous plugins in it.  It is highly recom' +
          'mended to only merge plugins that are contiguous to avoid issues' +
          ' in the resulting merged plugin.  You will have to adjust your l' +
          'oad order and restart Merge Plugins to resolve this.'
        WordWrap = True
      end
      object btnIgnoreContiguous: TButton
        Left = 452
        Top = 309
        Width = 100
        Height = 25
        Margins.Right = 8
        Margins.Bottom = 8
        Align = alCustom
        Anchors = [akRight, akBottom]
        Caption = 'Ignore'
        TabOrder = 0
        OnClick = btnIgnoreContiguousClick
      end
      object lvNonContiguous: TListView
        Left = 3
        Top = 63
        Width = 550
        Height = 240
        Align = alCustom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Load order'
            Width = 80
          end
          item
            AutoSize = True
            Caption = 'Plugin filename'
          end>
        ColumnClick = False
        DoubleBuffered = True
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        TabOrder = 1
        ViewStyle = vsReport
      end
    end
    object tsPluginErrors: TTabSheet
      Caption = 'Plugin errors'
      ImageIndex = 2
      object lblPluginErrors: TLabel
        Left = 3
        Top = 13
        Width = 550
        Height = 44
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'You need to check all plugins in your merge for errors before yo' +
          'u can build the merge.  Plugins with errors may not merge proper' +
          'ly - some content may be lost!  You can have merge plugins attem' +
          'pt to fix errors in plugins, or remove plugins with errors from ' +
          'your merge.  Ignore errors in plugins at your own risk!'
        WordWrap = True
      end
      object lvPluginErrors: TListView
        Left = 3
        Top = 63
        Width = 550
        Height = 276
        Align = alCustom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Errors status'
            Width = 150
          end
          item
            AutoSize = True
            Caption = 'Plugin filename'
          end>
        ColumnClick = False
        DoubleBuffered = True
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        PopupMenu = PluginErrorsPopupMenu
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
    object tsOtherIssues: TTabSheet
      Caption = 'Other issues'
      ImageIndex = 3
      object lblOtherIssues: TLabel
        Left = 3
        Top = 13
        Width = 550
        Height = 28
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = 
          'Your merge has other issues that make it so you cannot build it.' +
          '  Perhaps there are plugins in this merge that aren'#39't loaded, or' +
          ' your directories are invalid, or your merge has no plugins in i' +
          't.'
        WordWrap = True
      end
      object lvOtherIssues: TListView
        Left = 3
        Top = 47
        Width = 550
        Height = 292
        Align = alCustom
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            Caption = 'Issue'
            Width = 200
          end
          item
            AutoSize = True
            Caption = 'Details'
          end>
        ColumnClick = False
        DoubleBuffered = True
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        PopupMenu = OtherIssuesPopupMenu
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
  object btnOK: TButton
    Left = 501
    Top = 384
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object DependenciesPopupMenu: TPopupMenu
    OnPopup = DependenciesPopupMenuPopup
    Left = 72
    Top = 120
    object RemoveBreakingPluginItem: TMenuItem
      Caption = 'Remove plugin from merge'
      OnClick = RemoveBreakingPluginItemClick
    end
    object AddDependencyItem: TMenuItem
      Caption = 'Add dependency to merge'
      OnClick = AddDependencyItemClick
    end
    object IgnoreDependencyItem: TMenuItem
      Caption = 'Ignore dependency'
      OnClick = IgnoreDependencyItemClick
    end
  end
  object PluginErrorsPopupMenu: TPopupMenu
    OnPopup = PluginErrorsPopupMenuPopup
    Left = 72
    Top = 176
    object CheckPluginItem: TMenuItem
      Caption = 'Check plugin for errors'
      OnClick = CheckPluginItemClick
    end
    object FixErrorsItem: TMenuItem
      Caption = 'Fix errors in plugin'
      OnClick = FixErrorsItemClick
    end
    object RemoveErrorsPluginItem: TMenuItem
      Caption = 'Remove plugin from merge'
      OnClick = RemoveErrorsPluginItemClick
    end
    object IgnoreErrorsItem: TMenuItem
      Caption = 'Ignore errors'
      OnClick = IgnoreErrorsItemClick
    end
  end
  object OtherIssuesPopupMenu: TPopupMenu
    OnPopup = OtherIssuesPopupMenuPopup
    Left = 72
    Top = 240
    object RemoveUnloadedPluginItem: TMenuItem
      Caption = 'Remove plugin'
      OnClick = RemoveUnloadedPluginItemClick
    end
  end
end
