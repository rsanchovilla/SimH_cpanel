object FoMain: TFoMain
  Left = 260
  Top = 164
  Width = 711
  Height = 505
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'Simple User Asisted OCR'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    7BBBBB700000000007BBBBB7000000007BBBBB700000000077BBBBB700000007
    BBBBBB770000000077BBBBB700000007BBBBBBB7000000007BBBBBB770000007
    BBBBBBB770000007BBBBBBBB70000077BBBBBBBB70000007BBBBBBBB7000007B
    BBBBBBBB7000007BBBBBBBBB7000007BBBBBBB7B7000007B7BBBBBBB7000007B
    BBBBBB7B7000007B7BBBBBBB7000007B7B7B7B77700000777B7B7B7B7700707B
    7B7B7B70077777077B7B7B7B77707F7B7B7B7B7FFFFFFFFF7B7B7B7B7FF77FF7
    7B7B7B7FFFFFFFFF7B7B7B777FF77F44477B777444444444477B77744FF77F44
    4F477F44444444444F477F444FF77F444F444F44444444444F444F444FF77FFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFF77F777F777F777F777F777F777F777FF77F77
    7F777F777F777F777F777F777FF77F777F777F777F777F777F777F777FF77FFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFF77F777F777F777F777F777F777F777FF77F77
    7F777F777F777F777F777F777FF77F777F777F777F777F777F777F777FF77FFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFF7777777777777777777777777777777770000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object ImDisp: TPaintBox
    Left = 16
    Top = 16
    Width = 339
    Height = 437
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnClick = ImDispClick
    OnDblClick = ImDispDblClick
    OnMouseDown = ImDispMouseDown
    OnMouseMove = ImDispMouseMove
    OnMouseUp = ImDispMouseUp
    OnPaint = ImDispPaint
  end
  object ImSrc: TImage
    Left = 32
    Top = 32
    Width = 33
    Height = 41
    Visible = False
  end
  object LProgress: TLabel
    Left = 415
    Top = 72
    Width = 264
    Height = 21
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = 'LProgress'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object LMsg: TLabel
    Left = 412
    Top = 446
    Width = 33
    Height = 16
    Anchors = [akBottom]
    Caption = 'LMsg'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object LPage: TLabel
    Left = 415
    Top = 48
    Width = 264
    Height = 21
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = 'LPage'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object BStart: TButton
    Left = 455
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Init OCR'
    TabOrder = 1
    OnClick = BStartClick
  end
  object BLoad: TButton
    Left = 374
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load'
    TabOrder = 0
    OnClick = BLoadClick
  end
  object BDoOCR: TButton
    Left = 534
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Do OCR'
    TabOrder = 2
    OnClick = BDoOCRClick
  end
  object PageControl: TPageControl
    Left = 414
    Top = 96
    Width = 265
    Height = 346
    ActivePage = TabOptions
    Anchors = [akTop, akRight, akBottom]
    MultiLine = True
    TabOrder = 7
    object TabLog: TTabSheet
      Caption = 'Log'
      object TLog: TMemo
        Left = 0
        Top = 8
        Width = 249
        Height = 305
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object TabMatch: TTabSheet
      Caption = 'Match'
      ImageIndex = 1
      object TMatch: TMemo
        Left = 8
        Top = 8
        Width = 241
        Height = 305
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 0
      end
    end
    object TabText: TTabSheet
      Caption = 'Text'
      ImageIndex = 2
      object TOcrText: TMemo
        Left = 8
        Top = 8
        Width = 241
        Height = 305
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -19
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        OnKeyDown = TOcrTextKeyDown
        OnKeyPress = TOcrTextKeyPress
        OnMouseDown = TOcrTextMouseDown
      end
    end
    object TabOptions: TTabSheet
      Caption = 'Options'
      ImageIndex = 4
      object Label1: TLabel
        Left = 24
        Top = 144
        Width = 106
        Height = 13
        Caption = 'Start of line pattern(s): '
      end
      object Label2: TLabel
        Left = 24
        Top = 256
        Width = 188
        Height = 39
        Anchors = [akLeft, akTop, akRight]
        Caption = 
          'A = char or space,  X= mandatory char, <space> = mandatory space' +
          ', * = ignore symbols past this char'
        WordWrap = True
      end
      object BPgGoto: TButton
        Left = 24
        Top = 24
        Width = 209
        Height = 25
        Caption = 'Goto Page ...'
        TabOrder = 0
        OnClick = BPgGotoClick
      end
      object BPgReload: TButton
        Left = 24
        Top = 56
        Width = 209
        Height = 25
        Caption = 'Change Contrast and Reload'
        TabOrder = 1
        OnClick = BPgReloadClick
      end
      object CkKeepOCRed: TCheckBox
        Left = 24
        Top = 96
        Width = 225
        Height = 17
        Caption = 'OCR All keeps already OCRed Symbols'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
      object TLinPattern: TMemo
        Left = 24
        Top = 160
        Width = 209
        Height = 89
        Anchors = [akLeft, akTop, akRight]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 4
        WordWrap = False
        OnChange = TLinPatternChange
      end
      object CkAdjustPage: TCheckBox
        Left = 24
        Top = 120
        Width = 193
        Height = 17
        Caption = 'Adjust OCRed page margins'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
    end
  end
  object BDoOCRall: TButton
    Left = 615
    Top = 8
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Do OCR All'
    TabOrder = 3
    OnClick = BDoOCRallClick
  end
  object BPgUp: TButton
    Left = 375
    Top = 40
    Width = 33
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '<'
    TabOrder = 4
    OnClick = BPgUpClick
  end
  object BPgDn: TButton
    Left = 415
    Top = 40
    Width = 33
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '>'
    TabOrder = 5
    OnClick = BPgDnClick
  end
  object BStep: TButton
    Left = 24
    Top = 417
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Step'
    TabOrder = 6
    Visible = False
    OnClick = BStepClick
  end
  object Timer300ms: TTimer
    Interval = 300
    OnTimer = Timer300msTimer
    Left = 80
    Top = 32
  end
  object OpenInSrcDialog: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Title = 'Open Image to OCR'
    Left = 120
    Top = 32
  end
end
