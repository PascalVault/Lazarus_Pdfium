//2023-07-04
//FreePascal port for ahausladen/PdfiumLib (Updated to chromium/5868)
//https://github.com/ahausladen/PdfiumLib
//https://github.com/kjteng/PdfiumLib/
//Tested with: Lazarus(ver2.2.2)/Freepascal(version 3.2.2)
//With updates by domasz
//works with 32 & 64 bit Lazarus
{$A8,B-,E-,F-,G+,H+,I+,J-,K-,M-,N-,P+,Q-,R-,S-,T-,U-,V+,X+,Z1}
{$DEFINE FPC} // based on: https://github.com/ahausladen/PdfiumLib
//{$DEFINE FORM_DISABLED}  // do not use fillable pdf (pdf form)
//{$DEFINE VIEW_ONLY} // do not use TPdfDocumentVclPrinter/printer4lazarus

{$STRINGCHECKS OFF}

unit pdfiumcore;
{$IFDEF FPC}
{$MODE Delphi} //for FPC port
{$ENDIF}
interface

uses Dialogs,
{$IFDEF FPC} LCLIntf, LCLType,  {$ELSE}  WinSpool,{$ENDIF}
  Windows, Types, SysUtils, Classes, Contnrs, PdfiumLib, Graphics;
const
  // DIN A4
  PdfDefaultPageWidth = 595;
  PdfDefaultPageHeight = 842;

type
  EPdfException = class(Exception);
  EPdfUnsupportedFeatureException = class(EPdfException);
  EPdfArgumentOutOfRange = class(EPdfException);

  TPdfUnsupportedFeatureHandler = procedure(nType: Integer; const Typ: string) of object;

  TPdfDocument = class;
  TPdfPage = class;
  TPdfAttachmentList = class;
  TPdfAnnotationList = class;
  TPdfFormField = class;
  TPdfAnnotation = class;

  TPdfPoint = record
    X, Y: Double;
    procedure Offset(XOffset, YOffset: Double);
    class function Empty: TPdfPoint; static;
  end;

  TPdfRect = record
  private
    function GetHeight: Double; inline;
    function GetWidth: Double; inline;
    procedure SetHeight(const Value: Double); inline;
    procedure SetWidth(const Value: Double); inline;
  public
    property Width: Double read GetWidth write SetWidth;
    property Height: Double read GetHeight write SetHeight;
    procedure Offset(XOffset, YOffset: Double);

    class function Empty: TPdfRect; static;
  public
    case Integer of
      0: (Left, Top, Right, Bottom: Double);
      1: (TopLeft: TPdfPoint; BottomRight: TPdfPoint);
  end;

  TPdfRectArray = array of TPdfRect;

  TPdfDocumentCustomReadProc = function(Param: Pointer; Position: LongWord; Buffer: PByte; Size: LongWord): Boolean;

  TPdfPageRenderOptionType = (
    proAnnotations,            // Set if annotations are to be rendered.
    proLCDOptimized,           // Set if using text rendering optimized for LCD display.
    proNoNativeText,           // Don't use the native text output available on some platforms
    proNoCatch,                // Set if you don't want to catch exception.
    proLimitedImageCacheSize,  // Limit image cache size.
    proForceHalftone,          // Always use halftone for image stretching.
    proPrinting,               // Render for printing.
    proReverseByteOrder        // Set whether render in a reverse Byte order, this flag only enable when render to a bitmap.
  );
  TPdfPageRenderOptions = set of TPdfPageRenderOptionType;

  TPdfPageRotation = (
    prNormal             = 0,
    pr90Clockwise        = 1,
    pr180                = 2,
    pr90CounterClockwide = 3
  );

  TPdfDocumentSaveOption = (
    dsoIncremental    = 1,
    dsoNoIncremental  = 2,
    dsoRemoveSecurity = 3
  );

  TPdfDocumentLoadOption = (
    dloMemory,   // load the whole file into memory
    dloMMF,      // load the file by using a memory mapped file (file stays open)
    dloOnDemand  // load the file using the custom load function (file stays open)
  );

  TPdfDocumentPageMode = (
    dpmUnknown        = -1, // Unknown value
    dpmUseNone        = 0,  // Neither document outline nor thumbnail images visible
    dpmUseOutlines    = 1,  // Document outline visible
    dpmUseThumbs      = 2,  // Thumbnial images visible
    dpmFullScreen     = 3,  // Full-screen mode, with no menu bar, window controls, or any other window visible
    dpmUseOC          = 4,  // Optional content group panel visible
    dpmUseAttachments = 5   // Attachments panel visible
  );

  TPdfPrintMode = (
    pmEMF                          = FPDF_PRINTMODE_EMF,
    pmTextMode                     = FPDF_PRINTMODE_TEXTONLY,
    pmPostScript2                  = FPDF_PRINTMODE_POSTSCRIPT2,
    pmPostScript3                  = FPDF_PRINTMODE_POSTSCRIPT3,
    pmPostScriptPassThrough2       = FPDF_PRINTMODE_POSTSCRIPT2_PASSTHROUGH,
    pmPostScriptPassThrough3       = FPDF_PRINTMODE_POSTSCRIPT3_PASSTHROUGH,
    pmEMFImageMasks                = FPDF_PRINTMODE_EMF_IMAGE_MASKS,
    pmPostScript3Type42            = FPDF_PRINTMODE_POSTSCRIPT3_TYPE42,
    pmPostScript3Type42PassThrough = FPDF_PRINTMODE_POSTSCRIPT3_TYPE42_PASSTHROUGH
  );

  TPdfFileIdType = (
    pfiPermanent = 0,
    pfiChanging = 1
  );

  TPdfBitmapFormat = (
    bfGrays = FPDFBitmap_Gray, // Gray scale bitmap, one byte per pixel.
    bfBGR   = FPDFBitmap_BGR,  // 3 bytes per pixel, byte order: blue, green, red.
    bfBGRx  = FPDFBitmap_BGRx, // 4 bytes per pixel, byte order: blue, green, red, unused.
    bfBGRA  = FPDFBitmap_BGRA  // 4 bytes per pixel, byte order: blue, green, red, alpha.
  );

  TPdfFormFieldType = (
    fftUnknown         = FPDF_FORMFIELD_UNKNOWN,
    fftPushButton      = FPDF_FORMFIELD_PUSHBUTTON,
    fftCheckBox        = FPDF_FORMFIELD_CHECKBOX,
    fftRadioButton     = FPDF_FORMFIELD_RADIOBUTTON,
    fftComboBox        = FPDF_FORMFIELD_COMBOBOX,
    fftListBox         = FPDF_FORMFIELD_LISTBOX,
    fftTextField       = FPDF_FORMFIELD_TEXTFIELD,
    fftSignature       = FPDF_FORMFIELD_SIGNATURE,

    fftXFA             = FPDF_FORMFIELD_XFA,
    fftXFACheckBox     = FPDF_FORMFIELD_XFA_CHECKBOX,
    fftXFAComboBox     = FPDF_FORMFIELD_XFA_COMBOBOX,
    fftXFAImageField   = FPDF_FORMFIELD_XFA_IMAGEFIELD,
    fftXFAListBox      = FPDF_FORMFIELD_XFA_LISTBOX,
    fftXFAPushButton   = FPDF_FORMFIELD_XFA_PUSHBUTTON,
    fftXFASignature    = FPDF_FORMFIELD_XFA_SIGNATURE,
    fftXfaTextField    = FPDF_FORMFIELD_XFA_TEXTFIELD
  );

  TPdfFormFieldFlagsType = (
    fffReadOnly,
    fffRequired,
    fffNoExport,

    fffTextMultiLine,
    fffTextPassword,

    fffChoiceCombo,
    fffChoiceEdit,
    fffChoiceMultiSelect
  );
  TPdfFormFieldFlags = set of TPdfFormFieldFlagsType;
  TPdfObjectType = (
    otUnknown = FPDF_OBJECT_UNKNOWN,
    otBoolean = FPDF_OBJECT_BOOLEAN,
    otNumber = FPDF_OBJECT_NUMBER,
    otString = FPDF_OBJECT_STRING,
    otName = FPDF_OBJECT_NAME,
    otArray = FPDF_OBJECT_ARRAY,
    otDictinary = FPDF_OBJECT_DICTIONARY,
    otStream = FPDF_OBJECT_STREAM,
    otNullObj = FPDF_OBJECT_NULLOBJ,
    otReference = FPDF_OBJECT_REFERENCE
  );

  _TPdfBitmapHideCtor = class(TObject)
  private
    procedure Create;
  end;

  TPdfBitmap = class(_TPdfBitmapHideCtor)
  private
    FBitmap: FPDF_BITMAP;
    FOwnsBitmap: Boolean;
    FWidth: Integer;
    FHeight: Integer;
    FBytesPerScanLine: Integer;
  public
    constructor Create(ABitmap: FPDF_BITMAP; AOwnsBitmap: Boolean = False); overload;
    constructor Create(AWidth, AHeight: Integer; AAlpha: Boolean); overload;
    constructor Create(AWidth, AHeight: Integer; AFormat: TPdfBitmapFormat); overload;
    constructor Create(AWidth, AHeight: Integer; AFormat: TPdfBitmapFormat; ABuffer: Pointer; ABytesPerScanline: Integer); overload;
    destructor Destroy; override;

    procedure FillRect(ALeft, ATop, AWidth, AHeight: Integer; AColor: FPDF_DWORD);
    function GetBuffer: Pointer;

    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property BytesPerScanline: Integer read FBytesPerScanLine;
    property Bitmap: FPDF_BITMAP read FBitmap;
  end;

  PPdfFormFillHandler = ^TPdfFormFillHandler;
  TPdfFormFillHandler = record
    FormFillInfo: FPDF_FORMFILLINFO;
    Document: TPdfDocument;
  end;

  TPdfFormField = class(TObject)
  private
    FPage: TPdfPage;
    FHandle: FPDF_ANNOTATION;
    FAnnotation: TPdfAnnotation;
    function GetFlags: TPdfFormFieldFlags;
    function GetReadOnly: Boolean;
    function GetName: string;
    function GetAlternateName: string;
    function GetFieldType: TPdfFormFieldType;
    function GetValue: string;
    function GetExportValue: string;
    function GetOptionCount: Integer;
    function GetOptionLabel(Index: Integer): string;
    function GetChecked: Boolean;
    function GetControlIndex: Integer;
    function GetControlCount: Integer;

    procedure SetValue(const Value: string);
    procedure SetChecked(const Value: Boolean);
  protected
    constructor Create(AAnnotation: TPdfAnnotation);

    function BeginEditFormField: FPDF_ANNOTATION;
    procedure EndEditFormField(LastFocusedAnnot: FPDF_ANNOTATION);
  public
    destructor Destroy; override;

    function IsXFAFormField: Boolean;

    function IsOptionSelected(OptionIndex: Integer): Boolean;
    function SelectComboBoxOption(OptionIndex: Integer): Boolean;
    function SelectListBoxOption(OptionIndex: Integer; Selected: Boolean = True): Boolean;

    property Flags: TPdfFormFieldFlags read GetFlags;
    property ReadOnly: Boolean read GetReadOnly;
    property Name: string read GetName;
    property AlternateName: string read GetAlternateName;
    property FieldType: TPdfFormFieldType read GetFieldType;
    property Value: string read GetValue write SetValue;
    property ExportValue: string read GetExportValue;

    // ComboBox/ListBox
    property OptionCount: Integer read GetOptionCount;
    property OptionLabels[Index: Integer]: string read GetOptionLabel;

    // CheckBox/RadioButton
    property Checked: Boolean read GetChecked write SetChecked;
    property ControlIndex: Integer read GetControlIndex;
    property ControlCount: Integer read GetControlCount;

    property Annotation: TPdfAnnotation read FAnnotation;
    property Handle: FPDF_ANNOTATION read FHandle;
  end;

  TPdfFormFieldList = class(TObject)
  private
    FItems: TList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TPdfFormField;
  protected
    procedure DestroyingItem(Item: TPdfFormField);
  public
    constructor Create(AAnnotations: TPdfAnnotationList);
    destructor Destroy; override;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TPdfFormField read GetItem; default;
  end;

  TPdfAnnotation = class(TObject)
  private
    FPage: TPdfPage;
    FHandle: FPDF_ANNOTATION;
    FFormField: TPdfFormField;
    function GetFormField: TPdfFormField;
  protected
    constructor Create(APage: TPdfPage; AHandle: FPDF_ANNOTATION);
  public
    destructor Destroy; override;
    function IsFormField: Boolean;

    property FormField: TPdfFormField read GetFormField;

    property Handle: FPDF_ANNOTATION read FHandle;
  end;

  TPdfAnnotationList = class(TObject)
  private
    FPage: TPdfPage;
    FItems: TObjectList;
    FFormFields: TPdfFormFieldList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TPdfAnnotation;
    function GetFormFields: TPdfFormFieldList;
  protected
    procedure DestroyingItem(Item: TPdfAnnotation);
    procedure DestroyingFormField(FormField: TPdfFormField);
  public
    constructor Create(APage: TPdfPage);
    destructor Destroy; override;
    procedure CloseAnnotations;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TPdfAnnotation read GetItem; default;

    { A list of all form fields annotations }
    property FormFields: TPdfFormFieldList read GetFormFields;
  end;

  TPdfPage = class(TObject)
  private
    FDocument: TPdfDocument;
    FPage: FPDF_PAGE;
    FWidth: Single;
    FHeight: Single;
    FTransparency: Boolean;
    FRotation: TPdfPageRotation;
    FAnnotations: TPdfAnnotationList;
    FTextHandle: FPDF_TEXTPAGE;
    FSearchHandle: FPDF_SCHHANDLE;
    FLinkHandle: FPDF_PAGELINK;
    constructor Create(ADocument: TPdfDocument; APage: FPDF_PAGE);
    procedure UpdateMetrics;
    procedure Open;
    procedure SetRotation(const Value: TPdfPageRotation);
    function BeginText: Boolean;
    function BeginWebLinks: Boolean;
    class function GetDrawFlags(const Options: TPdfPageRenderOptions): Integer; static;
    procedure AfterOpen;
{$IFNDEF FORM_DISABLED}
    function IsValidForm: Boolean;
{$ENDIF}
    function GetMouseModifier(const Shift: TShiftState): Integer;
    function GetKeyModifier(KeyData: LPARAM): Integer;
    function GetHandle: FPDF_PAGE;
    function GetTextHandle: FPDF_TEXTPAGE;
    function GetFormFields: TPdfFormFieldList;
  public
    destructor Destroy; override;
    procedure Close;
    function IsLoaded: Boolean;

    procedure Draw(DC: HDC; X, Y, Width, Height: Integer; Rotate: TPdfPageRotation = prNormal;
      const Options: TPdfPageRenderOptions = []);
    procedure DrawToPdfBitmap(APdfBitmap: TPdfBitmap; X, Y, Width, Height: Integer; Rotate: TPdfPageRotation = prNormal;
      const Options: TPdfPageRenderOptions = []);
{$IFNDEF FORM_DISABLED}
    procedure DrawFormToPdfBitmap(APdfBitmap: TPdfBitmap; X, Y, Width, Height: Integer; Rotate: TPdfPageRotation = prNormal;
      const Options: TPdfPageRenderOptions = []);
{$ENDIF}
    function DeviceToPage(X, Y, Width, Height: Integer; DeviceX, DeviceY: Integer; Rotate: TPdfPageRotation = prNormal): TPdfPoint; overload;
    function PageToDevice(X, Y, Width, Height: Integer; PageX, PageY: Double; Rotate: TPdfPageRotation = prNormal): TPoint; overload;
    function DeviceToPage(X, Y, Width, Height: Integer; const R: TRect; Rotate: TPdfPageRotation = prNormal): TPdfRect; overload;
    function PageToDevice(X, Y, Width, Height: Integer; const R: TPdfRect; Rotate: TPdfPageRotation = prNormal): TRect; overload;

    procedure ApplyChanges;
    procedure Flatten(AFlatPrint: Boolean);
{$IFNDEF FORM_DISABLED}
    function FormEventFocus(const Shift: TShiftState; PageX, PageY: Double): Boolean;
    function FormEventMouseWheel(const Shift: TShiftState; WheelDelta: Integer; PageX, PageY: Double): Boolean;
    function FormEventMouseMove(const Shift: TShiftState; PageX, PageY: Double): Boolean;
    function FormEventLButtonDown(const Shift: TShiftState; PageX, PageY: Double): Boolean;
    function FormEventLButtonUp(const Shift: TShiftState; PageX, PageY: Double): Boolean;
    function FormEventRButtonDown(const Shift: TShiftState; PageX, PageY: Double): Boolean;
    function FormEventRButtonUp(const Shift: TShiftState; PageX, PageY: Double): Boolean;
    function FormEventKeyDown(KeyCode: Word; KeyData: LPARAM): Boolean;
    function FormEventKeyUp(KeyCode: Word; KeyData: LPARAM): Boolean;
    function FormEventKeyPress(Key: Word; KeyData: LPARAM): Boolean;
    function FormEventKillFocus: Boolean;
    function FormGetFocusedText: string;
    function FormGetSelectedText: string;
    //function FormReplaceSelection(const ANewText: string): Boolean;
    function FormReplaceAndKeepSelection(const ANewText: string): Boolean;
    function FormSelectAllText: Boolean;
    function FormCanUndo: Boolean;
    function FormCanRedo: Boolean;
    function FormUndo: Boolean;
    function FormRedo: Boolean;
 {$ENDIF}
    function BeginFind(const SearchString: string; MatchCase, MatchWholeWord: Boolean; FromEnd: Boolean): Boolean;

    function FindNext(var CharIndex, Count: Integer): Boolean;
    function FindPrev(var CharIndex, Count: Integer): Boolean;
    procedure EndFind;
procedure ccount;
    function GetCharCount: Integer;
    function ReadChar(CharIndex: Integer): WideChar;
    function GetCharFontSize(CharIndex: Integer): Double;
    function GetCharBox(CharIndex: Integer): TPdfRect;
    function GetCharIndexAt(PageX, PageY, ToleranceX, ToleranceY: Double): Integer;
    function ReadText(CharIndex, Count: Integer): string;
    function GetTextAt(const R: TPdfRect): string; overload;
    function GetTextAt(Left, Top, Right, Bottom: Double): string; overload;

    function GetTextRectCount(CharIndex, Count: Integer): Integer;
    function GetTextRect(RectIndex: Integer): TPdfRect;

{$IFNDEF FORM_DISABLED}
    function HasFormFieldAtPoint(X, Y: Double): TPdfFormFieldType;
{$ENDIF}
    function GetWebLinkCount: Integer;
    function GetWebLinkURL(LinkIndex: Integer): string;
    function GetWebLinkRectCount(LinkIndex: Integer): Integer;
    function GetWebLinkRect(LinkIndex, RectIndex: Integer): TPdfRect;

    property Handle: FPDF_PAGE read GetHandle;
    property TextHandle: FPDF_TEXTPAGE read GetTextHandle;

    property Width: Single read FWidth;
    property Height: Single read FHeight;
    property Transparency: Boolean read FTransparency;
    property Rotation: TPdfPageRotation read FRotation write SetRotation;
    property Annotations: TPdfAnnotationList read FAnnotations;
    property FormFields: TPdfFormFieldList read GetFormFields;
  end;

  TPdfFormInvalidateEvent = procedure(Document: TPdfDocument; Page: TPdfPage; const PageRect: TPdfRect) of object;
  TPdfFormOutputSelectedRectEvent = procedure(Document: TPdfDocument; Page: TPdfPage; const PageRect: TPdfRect) of object;
  TPdfFormGetCurrentPageEvent = procedure(Document: TPdfDocument; var CurrentPage: TPdfPage) of object;
  TPdfFormFieldFocusEvent = procedure(Document: TPdfDocument; Value: PWideChar; ValueLen: Integer; FieldFocused: Boolean) of object;

  TPdfAttachment = record
  private
    FDocument: TPdfDocument;
    FHandle: FPDF_ATTACHMENT;
    procedure CheckValid;

    function GetName: string;
    function GetKeyValue(const Key: string): string;
    procedure SetKeyValue(const Key, Value: string);
    function GetContentSize: Integer;
  public
    // SetContent/LoadFromXxx clears the Values[] dictionary.
    procedure SetContent(const ABytes: TBytes); overload;
    procedure SetContent(const ABytes: TBytes; Index: NativeInt; Count: Integer); overload;
    procedure SetContent(ABytes: PByte; Count: Integer); overload;
    procedure SetContent(const Value: RawByteString); overload;
    procedure SetContent(const Value: string; Encoding: TEncoding = nil); overload; // Default-encoding is UTF-8
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);

    procedure GetContent(var ABytes: TBytes); overload;
    procedure GetContent(Buffer: PByte); overload; // use ContentSize to allocate enough memory
    procedure GetContent(var Value: RawByteString); overload;
    procedure GetContent(var Value: string; Encoding: TEncoding = nil); overload;
    function GetContentAsBytes: TBytes;
    function GetContentAsRawByteString: RawByteString;
    function GetContentAsString(Encoding: TEncoding = nil): string; // Default-encoding is UTF-8

    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);

    function HasContent: Boolean;

    function HasKey(const Key: string): Boolean;
    function GetValueType(const Key: string): TPdfObjectType;

    property Name: string read GetName;
    property Values[const Key: string]: string read GetKeyValue write SetKeyValue;
    property ContentSize: Integer read GetContentSize;

    property Handle: FPDF_ATTACHMENT read FHandle;
  end;

  TPdfAttachmentList = class(TObject)
  private
    FDocument: TPdfDocument;
    function GetCount: Integer;
    function GetItem(Index: Integer): TPdfAttachment;
  public
    constructor Create(ADocument: TPdfDocument);

    function Add(const Name: string): TPdfAttachment;
    procedure Delete(Index: Integer);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TPdfAttachment read GetItem; default;
  end;

  TPdfDocument = class(TObject)
  private type
    PCustomLoadDataRec = ^TCustomLoadDataRec;
    TCustomLoadDataRec = record
      Param: Pointer;
      GetBlock: TPdfDocumentCustomReadProc;
      FileAccess: TFPDFFileAccess;
    end;
  private
    FDocument: FPDF_DOCUMENT;
    FPages: TObjectList;
    FAttachments: TPdfAttachmentList;
    FFileName: string;
    FFileHandle: THandle;
    FFileMapping: THandle;
    FBuffer: PByte;
    FBytes: TBytes;
    FClosing: Boolean;
    FUnsupportedFeatures: Boolean;
    FCustomLoadData: PCustomLoadDataRec;
{$IFNDEF FORM_DISABLED}
    FForm: FPDF_FORMHANDLE;
    FFormFillHandler: TPdfFormFillHandler;
    FFormFieldHighlightColor: TColor;
    FFormFieldHighlightAlpha: Integer;
    FPrintHidesFormFieldHighlight: Boolean;
    FFormModified: Boolean;
    FOnFormInvalidate: TPdfFormInvalidateEvent;
    FOnFormOutputSelectedRect: TPdfFormOutputSelectedRectEvent;
    FOnFormGetCurrentPage: TPdfFormGetCurrentPageEvent;
    FOnFormFieldFocus: TPdfFormFieldFocusEvent;
{$ENDIF}
    procedure InternLoadFromMem(Buffer: PByte; Size: NativeInt; const APassword: UTF8String);
    procedure InternLoadFromCustom(ReadFunc: TPdfDocumentCustomReadProc; ASize: LongWord;
      AParam: Pointer; const APassword: UTF8String);
    function InternImportPages(Source: TPdfDocument; PageIndices: PInteger; PageIndicesCount: Integer;
      const Range: AnsiString; Index: Integer; ImportByRange: Boolean): Boolean;
    function GetPage(Index: Integer): TPdfPage;
    function GetPageCount: Integer;
    procedure ExtractPage(APage: TPdfPage);
    function ReloadPage(APage: TPdfPage): FPDF_PAGE;
    function GetPrintScaling: Boolean;
    function GetActive: Boolean;
    procedure CheckActive;
    function GetSecurityHandlerRevision: Integer;
    function GetDocPermissions: Integer;
    function GetFileVersion: Integer;
    function GetPageSize(Index: Integer): TPdfPoint;
    function GetPageMode: TPdfDocumentPageMode;
    function GetNumCopies: Integer;
    procedure DocumentLoaded;
{$IFNDEF FORM_DISABLED}
    procedure SetFormFieldHighlightAlpha(Value: Integer);
    procedure SetFormFieldHighlightColor(const Value: TColor);
    procedure UpdateFormFieldHighlight;
{$ENDIF}
    function FindPage(Page: FPDF_PAGE): TPdfPage;

  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadFromCustom(ReadFunc: TPdfDocumentCustomReadProc; ASize: LongWord; AParam: Pointer; const APassword: UTF8String = '');
    procedure LoadFromActiveStream(Stream: TStream; const APassword: UTF8String = ''); // Stream must not be released until the document is closed
    procedure LoadFromActiveBuffer(Buffer: Pointer; Size: NativeInt; const APassword: UTF8String = ''); // Buffer must not be released until the document is closed
    procedure LoadFromBytes(const ABytes: TBytes; const APassword: UTF8String = ''); overload;
    procedure LoadFromBytes(const ABytes: TBytes; AIndex: NativeInt; ACount: NativeInt; const APassword: UTF8String = ''); overload;
    procedure LoadFromStream(AStream: TStream; const APassword: UTF8String = '');
    procedure LoadFromFile(const AFileName: string; const APassword: UTF8String = ''; ALoadOption: TPdfDocumentLoadOption = dloMMF);
    procedure Close;

    procedure SaveToFile(const AFileName: string; Option: TPdfDocumentSaveOption = dsoRemoveSecurity; FileVersion: Integer = -1);
    procedure SaveToStream(Stream: TStream; Option: TPdfDocumentSaveOption = dsoRemoveSecurity; FileVersion: Integer = -1);
    procedure SaveToBytes(var Bytes: TBytes; Option: TPdfDocumentSaveOption = dsoRemoveSecurity; FileVersion: Integer = -1);
{$IFDEF FPC}
//    procedure PgToBMP(idx: integer; bmp: TBitMap);
    procedure PgToPNG(idx: integer; png: TPortableNetworkGraphic);
{$ENDIF}
    function NewDocument: Boolean;
    class function CreateNPagesOnOnePageDocument(Source: TPdfDocument; NewPageWidth, NewPageHeight: Double; NumPagesXAxis, NumPagesYAxis: Integer): TPdfDocument; overload;
    class function CreateNPagesOnOnePageDocument(Source: TPdfDocument; NumPagesXAxis, NumPagesYAxis: Integer): TPdfDocument; overload;
    function ImportAllPages(Source: TPdfDocument; Index: Integer = -1): Boolean;
    function ImportPages(Source: TPdfDocument; const Range: string = ''; Index: Integer = -1): Boolean;
    function ImportPageRange(Source: TPdfDocument; PageIndex: Integer; Count: Integer = -1; Index: Integer = -1): Boolean;
    function ImportPagesByIndex(Source: TPdfDocument; const PageIndices: array of Integer; Index: Integer = -1): Boolean;
    procedure DeletePage(Index: Integer);
    function NewPage(Width, Height: Double; Index: Integer = -1): TPdfPage; overload;
    function NewPage(Index: Integer = -1): TPdfPage; overload;
    function ApplyViewerPreferences(Source: TPdfDocument): Boolean;
    function IsPageLoaded(PageIndex: Integer): Boolean;

    function GetFileIdentifier(IdType: TPdfFileIdType): string;
    function GetMetaText(const TagName: string): string;

    class function SetPrintMode(PrintMode: TPdfPrintMode): Boolean; static;

    property FileName: string read FFileName;
    property PageCount: Integer read GetPageCount;
    property Pages[Index: Integer]: TPdfPage read GetPage;
    property PageSizes[Index: Integer]: TPdfPoint read GetPageSize;

    property Attachments: TPdfAttachmentList read FAttachments;

    property Active: Boolean read GetActive;
    property PrintScaling: Boolean read GetPrintScaling;
    property NumCopies: Integer read GetNumCopies;
    property SecurityHandlerRevision: Integer read GetSecurityHandlerRevision;
    property DocPermissions: Integer read GetDocPermissions;
    property FileVersion: Integer read GetFileVersion;
    property PageMode: TPdfDocumentPageMode read GetPageMode;

    // if UnsupportedFeatures is True, then the document has unsupported features. It is updated
    // after accessing a page.
    property UnsupportedFeatures: Boolean read FUnsupportedFeatures;
    property Handle: FPDF_DOCUMENT read FDocument;
{$IFNDEF FORM_DISABLED}
    property FormHandle: FPDF_FORMHANDLE read FForm;
    property FormFieldHighlightColor: TColor read FFormFieldHighlightColor write SetFormFieldHighlightColor default $FFE4DD;
    property FormFieldHighlightAlpha: Integer read FFormFieldHighlightAlpha write SetFormFieldHighlightAlpha default 100;
    property PrintHidesFormFieldHighlight: Boolean read FPrintHidesFormFieldHighlight write FPrintHidesFormFieldHighlight default True;
    property FormModified: Boolean read FFormModified write FFormModified;
    property OnFormInvalidate: TPdfFormInvalidateEvent read FOnFormInvalidate write FOnFormInvalidate;
    property OnFormOutputSelectedRect: TPdfFormOutputSelectedRectEvent read FOnFormOutputSelectedRect write FOnFormOutputSelectedRect;
    property OnFormGetCurrentPage: TPdfFormGetCurrentPageEvent read FOnFormGetCurrentPage write FOnFormGetCurrentPage;
    property OnFormFieldFocus: TPdfFormFieldFocusEvent read FOnFormFieldFocus write FOnFormFieldFocus;
{$ENDIF}
  end;

{$IFNDEF VIEW_ONLY}
  TPdfDocumentPrinterStatusEvent = procedure(Sender: TObject; CurrentPageNum, PageCount: Integer) of object;

  TPdfDocumentPrinter = class(TObject)
  private
    FBeginPrintCounter: Integer;

    FPrinterDC: HDC;
    FPrintPortraitOrientation: Boolean;
    FPaperSize: TSize;
    FPrintArea: TSize;
    FMargins: TPoint;

    FFitPageToPrintArea: Boolean;
    FOnPrintStatus: TPdfDocumentPrinterStatusEvent;

    function IsPortraitOrientation(AWidth, AHeight: Integer): Boolean;
    procedure GetPrinterBounds;
  protected
    function PrinterStartDoc(const AJobTitle: string): Boolean; virtual; abstract;
    procedure PrinterEndDoc; virtual; abstract;
    procedure PrinterStartPage; virtual; abstract;
    procedure PrinterEndPage; virtual; abstract;
    function GetPrinterDC: HDC; virtual; abstract;

    procedure InternPrintPage(APage: TPdfPage; X, Y, Width, Height: Double);
  public
    constructor Create;

    { BeginPrint must be called before printing multiple documents.
      Returns false if the printer can't print. (e.g. The user aborted the PDF Printer's FileDialog) }
    function BeginPrint(const AJobTitle: string = ''): Boolean;
    { EndPrint must be called after printing multiple documents were printed. }
    procedure EndPrint;

    { Prints a range of PDF document pages (0..PageCount-1) }
    function Print(ADocument: TPdfDocument; AFromPageIndex, AToPageIndex: Integer): Boolean; overload;
    { Prints all pages of the PDF document. }
    function Print(ADocument: TPdfDocument): Boolean; overload;


    { If FitPageToPrintArea is true the page fill be scaled to fit into the printable area. }
    property FitPageToPrintArea: Boolean read FFitPageToPrintArea write FFitPageToPrintArea default True;

    { OnPrintStatus is triggered after every printed page }
    property OnPrintStatus: TPdfDocumentPrinterStatusEvent read FOnPrintStatus write FOnPrintStatus;
  end;
{$ENDIF}

function SetThreadPdfUnsupportedFeatureHandler(const Handler: TPdfUnsupportedFeatureHandler): TPdfUnsupportedFeatureHandler;
var
  PDFiumDllDir: string = '';
  PDFiumDllFileName: string = ''; // use this instead of PDFiumDllDir if you want to change the DLLs file name
  {$IF declared(FPDF_InitEmbeddedLibraries)}
  PDFiumResDir: string = '';
  {$IFEND}

implementation

resourcestring
  RsUnsupportedFeature = 'Function %s not supported';
  RsArgumentsOutOfRange = 'Function argument "%s" (%d) out of range';
  RsDocumentNotActive = 'PDF document is not open';
  RsFileTooLarge = 'PDF file "%s" is too large';

  RsPdfCannotDeleteAttachmnent = 'Cannot delete the PDF attachment %d';
  RsPdfCannotAddAttachmnent = 'Cannot add the PDF attachment "%s"';
  RsPdfCannotSetAttachmentContent = 'Cannot set the PDF attachment content';
  RsPdfAttachmentContentNotSet = 'Content must be set before accessing string PDF attachmemt values';

  RsPdfAnnotationNotAFormFieldError = 'The annotation is not a form field';
  RsPdfErrorSuccess  = 'No error';
  RsPdfErrorUnknown  = 'Unknown error';
  RsPdfErrorFile     = 'File not found or can''t be opened';
  RsPdfErrorFormat   = 'File is not a PDF document or is corrupted';
  RsPdfErrorPassword = 'Password required oder invalid password';
  RsPdfErrorSecurity = 'Security schema is not support';
  RsPdfErrorPage     = 'Page does not exist or data error';

threadvar
  ThreadPdfUnsupportedFeatureHandler: TPdfUnsupportedFeatureHandler;
  UnsupportedFeatureCurrentDocument: TPdfDocument;

type
  { We don't want to use a TBytes temporary array if we can convert directly into the destination
    buffer. }
{$IFDEF FPC}
  TEncodingAccess = class helper for TEncoding
{$ELSE}
  TEncodingAccess = class(TEncoding)
{$ENDIF}
  public
    function GetMemCharCount(Bytes: PByte; ByteCount: Integer): Integer;
    function GetMemChars(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer;
  end;

function TEncodingAccess.GetMemCharCount(Bytes: PByte; ByteCount: Integer): Integer;
begin
  Result := GetCharCount(Bytes, ByteCount);
end;

function TEncodingAccess.GetMemChars(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer;
begin
{$IFDEF FPC}
  Result := GetChars(Bytes, ByteCount, PUnicodeChar(PWideChar(chars)), CharCount);
{$ELSE}
  Result := GetChars(Bytes, ByteCount, Chars, CharCount);
{$ENDIF}
end;

function SetThreadPdfUnsupportedFeatureHandler(const Handler: TPdfUnsupportedFeatureHandler): TPdfUnsupportedFeatureHandler;
begin
  Result := ThreadPdfUnsupportedFeatureHandler;
  ThreadPdfUnsupportedFeatureHandler := Handler;
end;

{$IFDEF FPC}
function GetFileSizeEx(hFile: THandle; var lpFileSize: Int64): Bool; stdcall;
  external kernel32 name 'GetFileSizeEx';

{$ELSE}
{$IF not declared(GetFileSizeEx)}
function GetFileSizeEx(hFile: THandle; var lpFileSize: Int64): Boolean; stdcall;
  external kernel32 name 'GetFileSizeEx';
{$IFEND}
{$ENDIF}

procedure SwapInts(var X, Y: Integer);
var
  Tmp: Integer;
begin
  Tmp := X;
  X := Y;
  Y := Tmp;
end;

function GetUnsupportedFeatureName(nType: Integer): string;
begin
  case nType of
    FPDF_UNSP_DOC_XFAFORM:
      Result := 'XFA';

    FPDF_UNSP_DOC_PORTABLECOLLECTION:
      Result := 'Portfolios_Packages';

    FPDF_UNSP_DOC_ATTACHMENT,
    FPDF_UNSP_ANNOT_ATTACHMENT:
      Result := 'Attachment';

    FPDF_UNSP_DOC_SECURITY:
      Result := 'Rights_Management';

    FPDF_UNSP_DOC_SHAREDREVIEW:
      Result := 'Shared_Review';

    FPDF_UNSP_DOC_SHAREDFORM_ACROBAT,
    FPDF_UNSP_DOC_SHAREDFORM_FILESYSTEM,
    FPDF_UNSP_DOC_SHAREDFORM_EMAIL:
      Result := 'Shared_Form';

    FPDF_UNSP_ANNOT_3DANNOT:
      Result := '3D';

    FPDF_UNSP_ANNOT_MOVIE:
      Result := 'Movie';

    FPDF_UNSP_ANNOT_SOUND:
      Result := 'Sound';

    FPDF_UNSP_ANNOT_SCREEN_MEDIA,
    FPDF_UNSP_ANNOT_SCREEN_RICHMEDIA:
      Result := 'Screen';

    FPDF_UNSP_ANNOT_SIG:
      Result := 'Digital_Signature';

  else
    Result := 'Unknown';
  end;
end;

procedure UnsupportedHandler(pThis: PUNSUPPORT_INFO; nType: Integer); cdecl;
var
  Document: TPdfDocument;
begin
  Document := UnsupportedFeatureCurrentDocument;
  if Document <> nil then
    Document.FUnsupportedFeatures := True;

  if Assigned(ThreadPdfUnsupportedFeatureHandler) then
    ThreadPdfUnsupportedFeatureHandler(nType, GetUnsupportedFeatureName(nType));
  //raise EPdfUnsupportedFeatureException.CreateResFmt(@RsUnsupportedFeature, [GetUnsupportedFeatureName]);
end;

var
  PDFiumInitCritSect: TRTLCriticalSection;
  UnsupportInfo: TUnsupportInfo = (
    version: 1;
    FSDK_UnSupport_Handler: UnsupportedHandler;
  );

procedure InitLib;
{$J+}
const
  Initialized: Integer = 0;
{$J-}
begin
  if Initialized = 0 then
  begin
    EnterCriticalSection(PDFiumInitCritSect);
    try
      if Initialized = 0 then
      begin
        if PDFiumDllFileName <> '' then
          InitPDFiumEx(PDFiumDllFileName {$IF declared(FPDF_InitEmbeddedLibraries)}, PDFiumResDir{$IFEND})
        else
          InitPDFium(PDFiumDllDir {$IF declared(FPDF_InitEmbeddedLibraries)}, PDFiumResDir{$IFEND});
        FSDK_SetUnSpObjProcessHandler(@UnsupportInfo);
        Initialized := 1;
      end;
    finally
      LeaveCriticalSection(PDFiumInitCritSect);
    end;
  end;
end;

procedure RaiseLastPdfError;
begin
  case {$IFDEF FPC} longword(FPDF_GetLastError)
       {$ELSE}FPDF_GetLastError{$ENDIF} of
    FPDF_ERR_SUCCESS:
      raise EPdfException.CreateRes(@RsPdfErrorSuccess);
    FPDF_ERR_FILE:
      raise EPdfException.CreateRes(@RsPdfErrorFile);
    FPDF_ERR_FORMAT:
      raise EPdfException.CreateRes(@RsPdfErrorFormat);
    FPDF_ERR_PASSWORD:
      raise EPdfException.CreateRes(@RsPdfErrorPassword);
    FPDF_ERR_SECURITY:
      raise EPdfException.CreateRes(@RsPdfErrorSecurity);
    FPDF_ERR_PAGE:
      raise EPdfException.CreateRes(@RsPdfErrorPage);
  else
    raise EPdfException.CreateRes(@RsPdfErrorUnknown);
  end;
end;

{$IFNDEF FORM_DISABLED}
procedure FFI_Invalidate(pThis: PFPDF_FORMFILLINFO; page: FPDF_PAGE; left, top, right, bottom: Double); cdecl;
var
  Handler: PPdfFormFillHandler;
  Pg: TPdfPage;
  R: TPdfRect;
begin
  Handler := PPdfFormFillHandler(pThis);
  if Assigned(Handler.Document.OnFormInvalidate) then
  begin
    Pg := Handler.Document.FindPage(page);
    if Pg <> nil then
    begin
      R.Left := left;
      R.Top := top;
      R.Right := right;
      R.Bottom := bottom;
      Handler.Document.OnFormInvalidate(Handler.Document, Pg, R);
    end;
  end;
end;


procedure FFI_Change(pThis: PFPDF_FORMFILLINFO); cdecl;
var
  Handler: PPdfFormFillHandler;
begin
  Handler := PPdfFormFillHandler(pThis);
  Handler.Document.FormModified := True;
end;

procedure FFI_OutputSelectedRect(pThis: PFPDF_FORMFILLINFO; page: FPDF_PAGE; left, top, right, bottom: Double); cdecl;
var
  Handler: PPdfFormFillHandler;
  Pg: TPdfPage;
  R: TPdfRect;
begin
  Handler := PPdfFormFillHandler(pThis);
  if Assigned(Handler.Document.OnFormOutputSelectedRect) then
  begin
    Pg := Handler.Document.FindPage(Page);
    if Pg <> nil then
    begin
      R.Left := left;
      R.Top := top;
      R.Right := right;
      R.Bottom := bottom;
      Handler.Document.OnFormOutputSelectedRect(Handler.Document, Pg, R);
    end;
  end;
end;
{$ENDIF}
var
  FFITimers: array of record
    Id: UINT;
    Proc: TFPDFTimerCallback;
  end;
  FFITimersCritSect: TRTLCriticalSection;

procedure FormTimerProc(hwnd: HWND; uMsg: UINT; timerId: UINT; dwTime: DWORD); stdcall;
var
  I: Integer;
  Proc: TFPDFTimerCallback;
begin
  Proc := nil;
  EnterCriticalSection(FFITimersCritSect);
  try
    for I := 0 to Length(FFITimers) - 1 do
    begin
      if FFITimers[I].Id = timerId then
      begin
        Proc := FFITimers[I].Proc;
        Break;
      end;
    end;
  finally
    LeaveCriticalSection(FFITimersCritSect);
  end;

  if Assigned(Proc) then
    Proc(timerId);
end;

function FFI_SetTimer(pThis: PFPDF_FORMFILLINFO; uElapse: Integer; lpTimerFunc: TFPDFTimerCallback): Integer; cdecl;
var
  I: Integer;
  Id: UINT;
begin
  Id := SetTimer(0, 0, uElapse, @FormTimerProc);
  Result := Integer(Id);
  if Id <> 0 then
  begin
    EnterCriticalSection(FFITimersCritSect);
    try
      for I := 0 to Length(FFITimers) - 1 do
      begin
        if FFITimers[I].Id = 0 then
        begin
          FFITimers[I].Id := Id;
          FFITimers[I].Proc := lpTimerFunc;
          Exit;
        end;
      end;
      I := Length(FFITimers);
      SetLength(FFITimers, I + 1);
      FFITimers[I].Id := Id;
      FFITimers[I].Proc := lpTimerFunc;
    finally
      LeaveCriticalSection(FFITimersCritSect);
    end;
  end;
end;

procedure FFI_KillTimer(pThis: PFPDF_FORMFILLINFO; nTimerID: Integer); cdecl;
var
  I: Integer;
begin
  if nTimerID <> 0 then
  begin
    KillTimer(0, nTimerID);

    EnterCriticalSection(FFITimersCritSect);
    try
      for I := 0 to Length(FFITimers) - 1 do
      begin
        if FFITimers[I].Id = UINT(nTimerID) then
        begin
          FFITimers[I].Id := 0;
          FFITimers[I].Proc := nil;
        end;
      end;

      I := Length(FFITimers) - 1;
      while (I >= 0) and (FFITimers[I].Id = 0) do
        Dec(I);
      SetLength(FFITimers, I + 1);
    finally
      LeaveCriticalSection(FFITimersCritSect);
    end;
  end;
end;

function FFI_GetLocalTime(pThis: PFPDF_FORMFILLINFO): FPDF_SYSTEMTIME; cdecl;
begin
  GetLocalTime(PSystemTime(@Result)^);
end;

function FFI_GetPage(pThis: PFPDF_FORMFILLINFO; document: FPDF_DOCUMENT; nPageIndex: Integer): FPDF_PAGE; cdecl;
var
  Handler: PPdfFormFillHandler;
begin
  Handler := PPdfFormFillHandler(pThis);
  Result := nil;
  if (Handler.Document <> nil) and (Handler.Document.FDocument = document) then
  begin
    if (nPageIndex >= 0) and (nPageIndex < Handler.Document.PageCount) then
      Result := Handler.Document.Pages[nPageIndex].FPage;
  end;
end;

{$IFNDEF FORM_DISABLED}
function FFI_GetCurrentPage(pThis: PFPDF_FORMFILLINFO; document: FPDF_DOCUMENT): FPDF_PAGE; cdecl;
var
  Handler: PPdfFormFillHandler;
  Pg: TPdfPage;
begin
  Handler := PPdfFormFillHandler(pThis);
  Result := nil;
  if (Handler.Document <> nil) and (Handler.Document.FDocument = document)
     and Assigned(Handler.Document.OnFormGetCurrentPage) then
  begin
    Pg := nil;
    Handler.Document.OnFormGetCurrentPage(Handler.Document, Pg);
    Result := nil;
    if Pg <> nil then
      Result := Pg.FPage;
  end;
end;
{$ENDIF}
function FFI_GetRotation(pThis: PFPDF_FORMFILLINFO; page: FPDF_PAGE): Integer; cdecl;
begin
  Result := 0;
end;

procedure FFI_SetCursor(pThis: PFPDF_FORMFILLINFO; nCursorType: Integer); cdecl;
begin
  // A better solution is to use check what form field type is under the mouse cursor in the
  // MoveMove event. Chrome/Edge don't rely on SetCursor either.
end;

{$IFNDEF FORM_DISABLED}
procedure FFI_SetTextFieldFocus(pThis: PFPDF_FORMFILLINFO; value: FPDF_WIDESTRING; valueLen: FPDF_DWORD; is_focus: FPDF_BOOL); cdecl;
var
  Handler: PPdfFormFillHandler;
begin
  Handler := PPdfFormFillHandler(pThis);
  if (Handler.Document <> nil) and Assigned(Handler.Document.OnFormFieldFocus) then
    Handler.Document.OnFormFieldFocus(Handler.Document, value, valueLen, is_focus <> 0);
end;

procedure FFI_FocusChange(param: PFPDF_FORMFILLINFO; annot: FPDF_ANNOTATION; page_index: Integer); cdecl;
begin
end;
{$ENDIF}


{ TPdfRect }

procedure TPdfRect.Offset(XOffset, YOffset: Double);
begin
  Left := Left + XOffset;
  Top := Top + YOffset;
  Right := Right + XOffset;
  Bottom := Bottom + YOffset;
end;

class function TPdfRect.Empty: TPdfRect;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := 0;
  Result.Bottom := 0;
end;

function TPdfRect.GetHeight: Double;
begin
  Result := Bottom - Top;
end;

function TPdfRect.GetWidth: Double;
begin
  Result := Right - Left;
end;

procedure TPdfRect.SetHeight(const Value: Double);
begin
  Bottom := Top + Value;
end;

procedure TPdfRect.SetWidth(const Value: Double);
begin
  Right := Left + Value;
end;

{ TPdfDocument }

constructor TPdfDocument.Create;
begin
  inherited Create;
  FPages := TObjectList.Create;
  FAttachments := TPdfAttachmentList.Create(Self);
  FFileHandle := INVALID_HANDLE_VALUE;
{$IFNDEF FORM_DISABLED}
  FFormFieldHighlightColor := $FFE4DD;
  FFormFieldHighlightAlpha := 100;
  FPrintHidesFormFieldHighlight := True;
{$ENDIF}
  InitLib;
end;

destructor TPdfDocument.Destroy;
begin
  Close;
  FAttachments.Free;
  FPages.Free;
  inherited Destroy;
end;

procedure TPdfDocument.Close;
begin
  FClosing := True;
  try
    FPages.Clear;
    FUnsupportedFeatures := False;

    if FDocument <> nil then
    begin
{$IFNDEF FORM_DISABLED}
      if FForm <> nil then
      begin
        FORM_DoDocumentAAction(FForm, FPDFDOC_AACTION_WC);
        FPDFDOC_ExitFormFillEnvironment(FForm);
        FForm := nil;
      end;
{$ENDIF}
      FPDF_CloseDocument(FDocument);
      FDocument := nil;
    end;

    if FCustomLoadData <> nil then
    begin
      Dispose(FCustomLoadData);
      FCustomLoadData := nil;
    end;

    if FFileMapping <> 0 then
    begin
      if FBuffer <> nil then
      begin
        UnmapViewOfFile(FBuffer);
        FBuffer := nil;
      end;
      FileClose(FFileMapping); { *Converted from CloseHandle* }
      FFileMapping := 0;
    end
    else if FBuffer <> nil then
    begin
      FreeMem(FBuffer);
      FBuffer := nil;
    end;
    FBytes := nil;

    if FFileHandle <> INVALID_HANDLE_VALUE then
    begin
      FileClose(FFileHandle); { *Converted from CloseHandle* }
      FFileHandle := INVALID_HANDLE_VALUE;
    end;

    FFileName := '';
{$IFNDEF FORM_DISABLED}
    FFormModified := False;
{$ENDIF}
  finally
    FClosing := False;
  end;
end;

function ReadFromActiveFile(Param: Pointer; Position: LongWord; Buffer: PByte; Size: LongWord): Boolean;
var
  NumRead: DWORD;
begin
  if Buffer <> nil then
  begin
    SetFilePointer(THandle(Param), Position, nil, FILE_BEGIN);
    Result := ReadFile(THandle(Param), Buffer^, Size, NumRead, nil) and (NumRead = Size);
  end
  else
    Result := Size = 0;
end;

procedure TPdfDocument.LoadFromFile(const AFileName: string; const APassword: UTF8String; ALoadOption: TPdfDocumentLoadOption);
var
  Size: Int64;
  Offset: NativeInt;
  NumRead: DWORD;
  LastError: DWORD;
begin
  Close;
  // We don't use FPDF_LoadDocument because it is limited to ANSI file names and dloOnDemand emulates it

  FFileHandle := CreateFile(PChar(AFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if FFileHandle = INVALID_HANDLE_VALUE then
    RaiseLastOSError;
  try
    if not GetFileSizeEx(FFileHandle, Size) then
      RaiseLastOSError;
    if Size > High(Integer) then // PDFium can only handle PDFs up to 2 GB (FX_FILESIZE in core/fxcrt/fx_system.h)
    begin
      {$IFDEF CPUX64}
      // FPDF_LoadCustomDocument wasn't updated to load larger files, so we fall back to MMF.
      if ALoadOption = dloOnDemand then
        ALoadOption := dloMMF;
      {$ELSE}
      raise EPdfException.CreateResFmt(@RsFileTooLarge, [ExtractFileName(AFileName)]);
      {$ENDIF CPUX64}
    end;

    case ALoadOption of
      dloMemory:
        begin
          if Size > 0 then
          begin
            try
              GetMem(FBuffer, Size);
              Offset := 0;
              while Offset < Size do
              begin
                if ((Size - Offset) and not $FFFFFFFF) <> 0 then
                  NumRead := $40000000
                else
                  NumRead := Size - Offset;

                if not ReadFile(FFileHandle, FBuffer[Offset], NumRead, NumRead, nil) then
                begin
                  LastError := GetLastError;
                  FreeMem(FBuffer);
                  FBuffer := nil;
                  RaiseLastOSError(LastError);
                end;
                Inc(Offset, NumRead);
              end;
            finally
              FileClose(FFileHandle); { *Converted from CloseHandle* }
              FFileHandle := INVALID_HANDLE_VALUE;
            end;

            InternLoadFromMem(FBuffer, Size, APassword);
          end;
        end;

      dloMMF:
        begin
          FFileMapping := CreateFileMapping(FFileHandle, nil, PAGE_READONLY, 0, 0, nil);
          if FFileMapping = 0 then
            RaiseLastOSError;
          FBuffer := MapViewOfFile(FFileMapping, FILE_MAP_READ, 0, 0, Size);
          if FBuffer = nil then
            RaiseLastOSError;

          InternLoadFromMem(FBuffer, Size, APassword);
        end;

      dloOnDemand:
        InternLoadFromCustom(ReadFromActiveFile, Size, Pointer(FFileHandle), APassword);
    end;
  except
    Close;
    raise;
  end;
  FFileName := AFileName;
end;

procedure TPdfDocument.LoadFromStream(AStream: TStream; const APassword: UTF8String);
var
  Size: NativeInt;
begin
  Close;
  Size := AStream.Size;
  if Size > 0 then
  begin
    GetMem(FBuffer, Size);
    try
      AStream.ReadBuffer(FBuffer^, Size);
      InternLoadFromMem(FBuffer, Size, APassword);
    except
      Close;
      raise;
    end;
  end;
end;

procedure TPdfDocument.LoadFromActiveBuffer(Buffer: Pointer; Size: NativeInt; const APassword: UTF8String);
begin
  Close;
  InternLoadFromMem(Buffer, Size, APassword);
end;

procedure TPdfDocument.LoadFromBytes(const ABytes: TBytes; const APassword: UTF8String);
begin
  LoadFromBytes(ABytes, 0, Length(ABytes), APassword);
end;

procedure TPdfDocument.LoadFromBytes(const ABytes: TBytes; AIndex, ACount: NativeInt;
  const APassword: UTF8String);
var
  Len: NativeInt;
begin
  Close;

  Len := Length(ABytes);
  if AIndex >= Len then
    raise EPdfArgumentOutOfRange.CreateResFmt(@RsArgumentsOutOfRange, ['Index', AIndex]);
  if AIndex + ACount > Len then
    raise EPdfArgumentOutOfRange.CreateResFmt(@RsArgumentsOutOfRange, ['Count', ACount]);

  FBytes := ABytes; // keep alive after return
  InternLoadFromMem(@ABytes[AIndex], ACount, APassword);
end;

function ReadFromActiveStream(Param: Pointer; Position: LongWord; Buffer: PByte; Size: LongWord): Boolean;
begin
  if Buffer <> nil then
  begin
    TStream(Param).Seek(Position, TSeekOrigin.soBeginning);
    Result := TStream(Param).Read(Buffer^, Size) = Integer(Size);
  end
  else
    Result := Size = 0;
end;

procedure TPdfDocument.LoadFromActiveStream(Stream: TStream; const APassword: UTF8String);
begin
  if Stream = nil then
    Close
  else
    LoadFromCustom(ReadFromActiveStream, Stream.Size, Stream, APassword);
end;

procedure TPdfDocument.LoadFromCustom(ReadFunc: TPdfDocumentCustomReadProc; ASize: LongWord;
  AParam: Pointer; const APassword: UTF8String);
begin
  Close;
  InternLoadFromCustom(ReadFunc, ASize, AParam, APassword);
end;

function GetLoadFromCustomBlock(Param: Pointer; Position: LongWord; Buffer: PByte; Size: LongWord): Integer; cdecl;
var
  Data: TPdfDocument.PCustomLoadDataRec;
begin
  Data := TPdfDocument(param).FCustomLoadData;
  Result := Ord(Data.GetBlock(Data.Param, Position, Buffer, Size));
end;

procedure TPdfDocument.InternLoadFromCustom(ReadFunc: TPdfDocumentCustomReadProc; ASize: LongWord;
  AParam: Pointer; const APassword: UTF8String);
var
  OldCurDoc: TPdfDocument;
begin
  if Assigned(ReadFunc) then
  begin
    New(FCustomLoadData);
    FCustomLoadData.Param := AParam;
    FCustomLoadData.GetBlock := ReadFunc;
    FCustomLoadData.FileAccess.m_FileLen := ASize;
    FCustomLoadData.FileAccess.m_GetBlock := GetLoadFromCustomBlock;
    FCustomLoadData.FileAccess.m_Param := Self;

    OldCurDoc := UnsupportedFeatureCurrentDocument;
    try
      UnsupportedFeatureCurrentDocument := Self;
      FDocument := FPDF_LoadCustomDocument(@FCustomLoadData.FileAccess, PAnsiChar(Pointer(APassword)));
      DocumentLoaded;
    finally
      UnsupportedFeatureCurrentDocument := OldCurDoc;
    end;
  end;
end;

procedure TPdfDocument.InternLoadFromMem(Buffer: PByte; Size: NativeInt; const APassword: UTF8String);
var
  OldCurDoc: TPdfDocument;
begin
  if Size > 0 then
  begin
    OldCurDoc := UnsupportedFeatureCurrentDocument;
    try
      UnsupportedFeatureCurrentDocument := Self;
      FDocument := FPDF_LoadMemDocument64(Buffer, Size, PAnsiChar(Pointer(APassword)));
    finally
      UnsupportedFeatureCurrentDocument := OldCurDoc;
    end;
    DocumentLoaded;
  end;
end;

procedure TPdfDocument.DocumentLoaded;
begin
{$IFNDEF FORM_DISABLED} FFormModified := False;{$ENDIF}
  if FDocument = nil then
    RaiseLastPdfError;

  FPages.Count := FPDF_GetPageCount(FDocument);
{$IFNDEF FORM_DISABLED}
  FillChar(FFormFillHandler, SizeOf(TPdfFormFillHandler), 0);
  FFormFillHandler.Document := Self;
  FFormFillHandler.FormFillInfo.version := 1; // will be set to 2 if we use an XFA-enabled DLL
  FFormFillHandler.FormFillInfo.FFI_Invalidate := FFI_Invalidate;
  FFormFillHandler.FormFillInfo.FFI_OnChange := FFI_Change;
  FFormFillHandler.FormFillInfo.FFI_OutputSelectedRect := FFI_OutputSelectedRect;
  FFormFillHandler.FormFillInfo.FFI_SetTimer := FFI_SetTimer;
  FFormFillHandler.FormFillInfo.FFI_KillTimer := FFI_KillTimer;
  FFormFillHandler.FormFillInfo.FFI_GetLocalTime := FFI_GetLocalTime;
  FFormFillHandler.FormFillInfo.FFI_GetPage := FFI_GetPage;
  FFormFillHandler.FormFillInfo.FFI_GetCurrentPage := FFI_GetCurrentPage;
  FFormFillHandler.FormFillInfo.FFI_GetRotation := FFI_GetRotation;
  FFormFillHandler.FormFillInfo.FFI_SetCursor := FFI_SetCursor;
  FFormFillHandler.FormFillInfo.FFI_SetTextFieldFocus := FFI_SetTextFieldFocus;
  FFormFillHandler.FormFillInfo.FFI_OnFocusChange := FFI_FocusChange;

  if PDF_USE_XFA then
  begin
    FFormFillHandler.FormFillInfo.version := 2;
    FFormFillHandler.FormFillInfo.xfa_disabled := 1; // Disable XFA support for now
  end;

  FForm := FPDFDOC_InitFormFillEnvironment(FDocument, @FFormFillHandler.FormFillInfo);
  if FForm <> nil then
  begin
    UpdateFormFieldHighlight;

    FORM_DoDocumentJSAction(FForm);
    FORM_DoDocumentOpenAction(FForm);
  end;
  {$ENDIF}
end;

{$IFNDEF FORM_DISABLED}
procedure TPdfDocument.UpdateFormFieldHighlight;
begin
  FPDF_SetFormFieldHighlightColor(FForm, 0, {ColorToRGB}(FFormFieldHighlightColor));
  FPDF_SetFormFieldHighlightAlpha(FForm, FFormFieldHighlightAlpha);
end;
{$ENDIF}

function TPdfDocument.IsPageLoaded(PageIndex: Integer): Boolean;
var
  Page: TPdfPage;
begin
  Page := TPdfPage(FPages[PageIndex]);
  Result := (Page <> nil) and Page.IsLoaded;
end;

function TPdfDocument.GetPage(Index: Integer): TPdfPage;
var
  LPage: FPDF_PAGE;
begin
  Result := TPdfPage(FPages[Index]);
  if Result = nil then
  begin
    LPage := FPDF_LoadPage(FDocument, Index);
    if LPage = nil then
      RaiseLastPdfError;
    Result := TPdfPage.Create(Self, LPage);
    FPages[Index] := Result;
  end
end;

function TPdfDocument.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TPdfDocument.ExtractPage(APage: TPdfPage);
begin
  if not FClosing then
    FPages.Extract(APage);
end;

function TPdfDocument.ReloadPage(APage: TPdfPage): FPDF_PAGE;
var
  Index: Integer;
begin
  CheckActive;
  Index := FPages.IndexOf(APage);
  Result := FPDF_LoadPage(FDocument, Index);
  if Result = nil then
    RaiseLastPdfError;
end;

function TPdfDocument.GetPrintScaling: Boolean;
begin
  CheckActive;
  Result := FPDF_VIEWERREF_GetPrintScaling(FDocument) <> 0;
end;

function TPdfDocument.GetActive: Boolean;
begin
  Result := FDocument <> nil;
end;

procedure TPdfDocument.CheckActive;
begin
  if not Active then
    raise EPdfException.CreateRes(@RsDocumentNotActive);
end;

class function TPdfDocument.CreateNPagesOnOnePageDocument(Source: TPdfDocument;
  NumPagesXAxis, NumPagesYAxis: Integer): TPdfDocument;
begin
  if Source.PageCount > 0 then
    Result := CreateNPagesOnOnePageDocument(Source, Source.PageSizes[0].X, Source.PageSizes[0].Y, NumPagesXAxis, NumPagesYAxis)
  else
    Result := CreateNPagesOnOnePageDocument(Source, PdfDefaultPageWidth, PdfDefaultPageHeight, NumPagesXAxis, NumPagesYAxis); // DIN A4 page
end;

class function TPdfDocument.CreateNPagesOnOnePageDocument(Source: TPdfDocument;
  NewPageWidth, NewPageHeight: Double; NumPagesXAxis, NumPagesYAxis: Integer): TPdfDocument;
begin
  Result := TPdfDocument.Create;
  try
    if (Source = nil) or not Source.Active then
      Result.NewDocument
    else
    begin
      Result.FDocument := FPDF_ImportNPagesToOne(Source.FDocument, NewPageWidth, NewPageHeight, NumPagesXAxis, NumPagesYAxis);
      if Result.FDocument <> nil then
        Result.DocumentLoaded
      else
        Result.NewDocument;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TPdfDocument.InternImportPages(Source: TPdfDocument; PageIndices: PInteger; PageIndicesCount: Integer;
  const Range: AnsiString; Index: Integer; ImportByRange: Boolean): Boolean;
var
  I, NewCount, OldCount, InsertCount: Integer;
begin
  CheckActive;
  Source.CheckActive;

  OldCount := FPDF_GetPageCount(FDocument);
  if Index < 0 then
    Index := OldCount;

  if ImportByRange then // Range = '' => Import all pages
    Result := FPDF_ImportPages(FDocument, Source.FDocument, PAnsiChar(Pointer(Range)), Index) <> 0
  else
    Result := FPDF_ImportPagesByIndex(FDocument, Source.FDocument, PageIndices, PageIndicesCount, Index) <> 0;

  NewCount := FPDF_GetPageCount(FDocument);
  InsertCount := NewCount - OldCount;
  if InsertCount > 0 then
  begin
    FPages.Count := NewCount;
    if Index < OldCount then
    begin
      Move(FPages.List[Index], FPages.List[Index + InsertCount], (OldCount - Index) * SizeOf(TObject));
      for I := Index to Index + InsertCount - 1 do
        FPages.List[Index] := nil;
    end;
  end;
end;

function TPdfDocument.ImportAllPages(Source: TPdfDocument; Index: Integer): Boolean;
begin
  Result := InternImportPages(Source, nil, 0, '', Index, False);
end;

function TPdfDocument.ImportPages(Source: TPdfDocument; const Range: string; Index: Integer): Boolean;
begin
  Result := InternImportPages(Source, nil, 0, AnsiString(Range), Index, True)
end;

function TPdfDocument.ImportPageRange(Source: TPdfDocument; PageIndex, Count, Index: Integer): Boolean;
begin
  Result := False;
  if (Source <> nil) and (PageIndex >= 0) then
  begin
    if Count = -1 then
      Count := Source.PageCount - PageIndex
    else if Count < 0 then
      Exit;

    if Count > 0 then
    begin
      if PageIndex + Count > Source.PageCount then
      begin
        Count := Source.PageCount - PageIndex;
        if Count = 0 then
          Exit;
      end;
      if (PageIndex = 0) and (Count = Source.PageCount) then
        Result := ImportAllPages(Source, Index)
      else
        Result := ImportPages(Source, Format('%d-%d', [PageIndex, PageIndex + Count - 1]));
    end;
  end;
end;

function TPdfDocument.ImportPagesByIndex(Source: TPdfDocument; const PageIndices: array of Integer; Index: Integer = -1): Boolean;
begin
  if Length(PageIndices) > 0 then
    Result := InternImportPages(Source, @PageIndices[0], Length(PageIndices), '', Index, False)
  else
    Result := ImportAllPages(Source, Index);
end;

procedure TPdfDocument.SaveToFile(const AFileName: string; Option: TPdfDocumentSaveOption; FileVersion: Integer);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate or fmShareDenyWrite);
  try
    SaveToStream(Stream, Option, FileVersion);
  finally
    Stream.Free;
  end;
end;

type
  PFPDFFileWriteEx = ^TFPDFFileWriteEx;
  TFPDFFileWriteEx = record
    Inner: TFPDFFileWrite; // emulate object inheritance
    Stream: TStream;
  end;

function WriteBlockToStream(pThis: PFPDF_FILEWRITE; pData: Pointer; size: LongWord): Integer; cdecl;
begin
  Result := Ord(LongWord(PFPDFFileWriteEx(pThis).Stream.Write(pData^, size)) = size);
end;

procedure TPdfDocument.SaveToStream(Stream: TStream; Option: TPdfDocumentSaveOption; FileVersion: Integer);
var
  FileWriteInfo: TFPDFFileWriteEx;
begin
  CheckActive;

  FileWriteInfo.Inner.version := 1;
  FileWriteInfo.Inner.WriteBlock := @WriteBlockToStream;
  FileWriteInfo.Stream := Stream;
{$IFNDEF FORM_DISABLED}
  if FForm <> nil then
  begin
    FORM_ForceToKillFocus(FForm); // also save the form field data that is currently focused
    FORM_DoDocumentAAction(FForm, FPDFDOC_AACTION_WS); // BeforeSave
  end;
{$ENDIF}
  if FileVersion <> -1 then
    FPDF_SaveWithVersion(FDocument, @FileWriteInfo, Ord(Option), FileVersion)
  else
    FPDF_SaveAsCopy(FDocument, @FileWriteInfo, Ord(Option));
{$IFNDEF FORM_DISABLED}
  if FForm <> nil then
    FORM_DoDocumentAAction(FForm, FPDFDOC_AACTION_DS); // AfterSave
{$ENDIF}
end;

procedure TPdfDocument.SaveToBytes(var Bytes: TBytes; Option: TPdfDocumentSaveOption; FileVersion: Integer);
var
  Stream: TBytesStream;
  Size: NativeInt;
begin
  CheckActive;

  Stream := TBytesStream.Create(nil);
  try
    SaveToStream(Stream, Option, FileVersion);
    Size := Stream.Size;
    Bytes := Stream.Bytes;
  finally
    Stream.Free;
  end;
  // Trim the byte array from the stream's capacity to the actual size
  if Length(Bytes) <> Size then
    SetLength(Bytes, Size);
end;

{$IFDEF FPC}
{
procedure TPdfDocument.PgToBMP(idx: integer; bmp: TBitMap);
var  pg: FPDF_PAGE; dib: FPDF_BITMAP;  bmpData: Pointer;
     ww, hh, bmpStride, yy: Integer;
begin
  pg := FPDF_LoadPage(FDocument, idx);
  ww := Trunc(FPDF_GetPageWidth(pg));
  hh := Trunc(FPDF_GetPageHeight(pg));
  dib := FPDFBitmap_Create(ww, hh, 1);
  FPDFBitmap_FillRect(dib, 0, 0, ww, hh, $FFFFFFFF);
  FPDF_RenderPageBitmap(dib, pg, 0, 0, ww, hh, 0, 0);  //, FPDF_REVERSE_BYTE_ORDER);
  bmpData := FPDFBitmap_GetBuffer(dib);
  bmp.SetSize(ww, hh);
  bmpStride := FPDFBitmap_GetStride(dib);
  if bmpStride / ww =3 then
    bmp.PixelFormat := pf24bit
  else
    bmp.PixelFormat := pf32bit;
  for yy := 0 to hh - 1 do
    Move(Pointer(PtrUInt(bmpData) + PtrUInt(yy * bmpStride))^,
              bmp.ScanLine[yy]^, bmpStride);
  pg := nil;
end;
}

procedure TPdfDocument.PgToPNG(idx: integer; png: TPortableNetworkGraphic);
var  pg: FPDF_PAGE; dib: FPDF_BITMAP;  bmpData: Pointer;
     ww, hh, bmpStride, yy: Integer;
begin
  pg := FPDF_LoadPage(FDocument, idx);
  ww := Trunc(FPDF_GetPageWidth(pg));
  hh := Trunc(FPDF_GetPageHeight(pg));
  dib := FPDFBitmap_Create(ww, hh, 1);
  FPDFBitmap_FillRect(dib, 0, 0, ww, hh, $FFFFFFFF);
  FPDF_RenderPageBitmap(dib, pg, 0, 0, ww, hh, 0, 0);  //, FPDF_REVERSE_BYTE_ORDER);
  bmpData := FPDFBitmap_GetBuffer(dib);
  png.SetSize(ww, hh);
  bmpStride := FPDFBitmap_GetStride(dib);
  if bmpStride / ww =3 then
    png.PixelFormat := pf24bit
  else
    png.PixelFormat := pf32bit;
  for yy := 0 to hh - 1 do
    Move(Pointer(PtrUInt(bmpData) + PtrUInt(yy * bmpStride))^,
              png.ScanLine[yy]^, bmpStride);
  pg := nil;

end;

{$ENDIF}

function TPdfDocument.NewDocument: Boolean;
begin
  Close;
  FDocument := FPDF_CreateNewDocument;
  Result := FDocument <> nil;
{$IFNDEF FORM_DISABLED} FFormModified := False; {$ENDIF}
end;

procedure TPdfDocument.DeletePage(Index: Integer);
begin
  CheckActive;
  FPages.Delete(Index);
  FPDFPage_Delete(FDocument, Index);
end;

function TPdfDocument.NewPage(Width, Height: Double; Index: Integer): TPdfPage;
var
  LPage: FPDF_PAGE;
begin
  CheckActive;
  if Index < 0 then
    Index := FPages.Count; // append new page
  LPage := FPDFPage_New(FDocument, Index, Width, Height);
  if LPage <> nil then
  begin
    Result := TPdfPage.Create(Self, LPage);
    FPages.Insert(Index, Result);
  end
  else
    Result := nil;
end;

function TPdfDocument.NewPage(Index: Integer = -1): TPdfPage;
begin
  Result := NewPage(PdfDefaultPageWidth, PdfDefaultPageHeight, Index);
end;

function TPdfDocument.ApplyViewerPreferences(Source: TPdfDocument): Boolean;
begin
  CheckActive;
  Source.CheckActive;
  Result := FPDF_CopyViewerPreferences(FDocument, Source.FDocument) <> 0;
end;

function TPdfDocument.GetFileIdentifier(IdType: TPdfFileIdType): string;
var
  Len: Integer;
  A: AnsiString;
begin
  CheckActive;
  Len := FPDF_GetFileIdentifier(FDocument, FPDF_FILEIDTYPE(IdType), nil, 0) div SizeOf(AnsiChar) - 1;
  if Len > 0 then
  begin
    SetLength(A, Len);
    FPDF_GetFileIdentifier(FDocument, FPDF_FILEIDTYPE(IdType), PAnsiChar(A), (Len + 1) * SizeOf(AnsiChar));
    Result := string(A);
  end
  else
    Result := '';
end;

function TPdfDocument.GetMetaText(const TagName: string): string;
var
  Len: Integer;
  A: AnsiString;
begin
  CheckActive;
  A := AnsiString(TagName);
  Len := FPDF_GetMetaText(FDocument, PAnsiChar(A), nil, 0) div SizeOf(WideChar) - 1;
  if Len > 0 then
  begin
    SetLength(Result, Len);
    FPDF_GetMetaText(FDocument, PAnsiChar(A), PChar(Result), (Len + 1) * SizeOf(WideChar));
  end
  else
    Result := '';
end;

function TPdfDocument.GetSecurityHandlerRevision: Integer;
begin
  CheckActive;
  Result := FPDF_GetSecurityHandlerRevision(FDocument);
end;

function TPdfDocument.GetDocPermissions: Integer;
begin
  CheckActive;
  Result := Integer(FPDF_GetDocPermissions(FDocument));
end;

function TPdfDocument.GetFileVersion: Integer;
begin
  CheckActive;
  if FPDF_GetFileVersion(FDocument, Result) = 0 then
    Result := 0;
end;

function TPdfDocument.GetPageSize(Index: Integer): TPdfPoint;
var
  SizeF: TFSSizeF;
begin
  CheckActive;
  Result.X := 0;
  Result.Y := 0;
  if FPDF_GetPageSizeByIndexF(FDocument, Index, @SizeF) <> 0 then
  begin
    Result.X := SizeF.width;
    Result.Y := SizeF.height;
  end;
end;

function TPdfDocument.GetPageMode: TPdfDocumentPageMode;
begin
  CheckActive;
  Result := TPdfDocumentPageMode(FPDFDoc_GetPageMode(FDocument));
end;

function TPdfDocument.GetNumCopies: Integer;
begin
  CheckActive;
  Result := FPDF_VIEWERREF_GetNumCopies(FDocument);
end;

class function TPdfDocument.SetPrintMode(PrintMode: TPdfPrintMode): Boolean;
begin
  InitLib;
  Result := FPDF_SetPrintMode(Ord(PrintMode)) <> 0;
end;

{$IFNDEF FORM_DISABLED}
procedure TPdfDocument.SetFormFieldHighlightAlpha(Value: Integer);
begin
  if Value < 0 then
    Value := 0;
  if Value > 255 then
    Value := 255;
  if Value <> FFormFieldHighlightAlpha then
  begin
    FFormFieldHighlightAlpha := Value;
    if Active then
      FPDF_SetFormFieldHighlightAlpha(FForm, FFormFieldHighlightAlpha);
  end;
end;

procedure TPdfDocument.SetFormFieldHighlightColor(const Value: TColor);
begin
  if Value <> FFormFieldHighlightColor then
  begin
    FFormFieldHighlightColor := Value;
    if Active then
      FPDF_SetFormFieldHighlightColor(FForm, 0, {ColorToRGB}(FFormFieldHighlightColor));
  end;
end;
{$ENDIF}

function TPdfDocument.FindPage(Page: FPDF_PAGE): TPdfPage;
var
  I: Integer;
begin
  // The page must be already loaded
  for I := 0 to PageCount - 1 do
  begin
    Result := TPdfPage(FPages[I]);
    if (Result <> nil) and (Result.FPage = Page) then
      Exit;
  end;
  Result := nil;
end;
                              {
procedure TPdfDocument.Pg2BMP(idx: integer; bmp: TBitMap);
var  dib: FPDF_BITMAP;  bmpData: Pointer;
     ww, hh, bmpStride, yy: Integer;
     pg: FPDF_PAGE;
begin
  pg := FPDF_LoadPage(FDocument, idx);
  ww := Trunc(FPDF_GetPageWidth(pg));
  hh := Trunc(FPDF_GetPageHeight(pg));
//  ww := Trunc(Self.FWidth);
//  hh := Trunc(Self.Height);
  dib := FPDFBitmap_Create(ww, hh, 1);
  FPDFBitmap_FillRect(dib, 0, 0, ww, hh, $FFFFFFFF);
  FPDF_RenderPageBitmap(dib, pg, 0, 0, ww, hh, 0, 0);  //, FPDF_REVERSE_BYTE_ORDER);
  bmpData := FPDFBitmap_GetBuffer(dib);
  bmp.SetSize(ww, hh);
  bmpStride := FPDFBitmap_GetStride(dib);
  if bmpStride / ww =3 then
    bmp.PixelFormat := pf24bit
  else
    bmp.PixelFormat := pf32bit;
  for yy := 0 to hh - 1 do
    Move(Pointer(PtrUInt(bmpData) + PtrUInt(yy * bmpStride))^,
              bm.ScanLine[yy]^, bmpStride);
end;
                             }

{ TPdfPage }
procedure TPdfPage.ccount;
begin
  showmessage( FPDFPage_CountObjects(FPage).ToString)
end;

constructor TPdfPage.Create(ADocument: TPdfDocument; APage: FPDF_PAGE);
begin
  inherited Create;
  FDocument := ADocument;
  FPage := APage;
  FAnnotations := TPdfAnnotationList.Create(Self);

  AfterOpen;
end;

destructor TPdfPage.Destroy;
begin
  Close;
  FDocument.ExtractPage(Self);
  FreeAndNil(FAnnotations);
  inherited Destroy;
end;


{$IFNDEF FORM_DISABLED}
function TPdfPage.IsValidForm: Boolean;
begin
  Result := (FDocument <> nil) and (FDocument.FForm <> nil) and (FPage <> nil);
end;
{$ENDIF}

procedure TPdfPage.AfterOpen;
var
  OldCurDoc: TPdfDocument;
begin

{$IFNDEF FORM_DISABLED}
  if IsValidForm then
  begin
    OldCurDoc := UnsupportedFeatureCurrentDocument;
    try
      UnsupportedFeatureCurrentDocument := FDocument;
      FORM_OnAfterLoadPage(FPage, FDocument.FForm);
      FORM_DoPageAAction(FPage, FDocument.FForm, FPDFPAGE_AACTION_OPEN);
    finally
      UnsupportedFeatureCurrentDocument := OldCurDoc;
    end;
  end;
{$ENDIF}
  UpdateMetrics;
end;


procedure TPdfPage.Close;
begin
  FAnnotations.CloseAnnotations;
{$IFNDEF FORM_DISABLED}
  if IsValidForm then
  begin
    FORM_DoPageAAction(FPage, FDocument.FForm, FPDFPAGE_AACTION_CLOSE);
    FORM_OnBeforeClosePage(FPage, FDocument.FForm);
  end;
{$ENDIF}
  if FLinkHandle <> nil then
  begin
    FPDFLink_CloseWebLinks(FLinkHandle);
    FLinkHandle := nil;
  end;
  if FSearchHandle <> nil then
  begin
    FPDFText_FindClose(FSearchHandle);
    FSearchHandle := nil;
  end;
  if FTextHandle <> nil then
  begin
    FPDFText_ClosePage(FTextHandle);
    FTextHandle := nil;
  end;
  if FPage <> nil then
  begin
    FPDF_ClosePage(FPage);
    FPage := nil;
  end;
end;

procedure TPdfPage.Open;
begin
  if FPage = nil then
  begin
    FPage := FDocument.ReloadPage(Self);
    AfterOpen;
  end;
end;

class function TPdfPage.GetDrawFlags(const Options: TPdfPageRenderOptions): Integer;
begin
  Result := 0;
  if proAnnotations in Options then
    Result := Result or FPDF_ANNOT;
  if proLCDOptimized in Options then
    Result := Result or FPDF_LCD_TEXT;
  if proNoNativeText in Options then
    Result := Result or FPDF_NO_NATIVETEXT;
  if proNoCatch in Options then
    Result := Result or FPDF_NO_CATCH;
  if proLimitedImageCacheSize in Options then
    Result := Result or FPDF_RENDER_LIMITEDIMAGECACHE;
  if proForceHalftone in Options then
    Result := Result or FPDF_RENDER_FORCEHALFTONE;
  if proPrinting in Options then
    Result := Result or FPDF_PRINTING;
  if proReverseByteOrder in Options then
    Result := Result or FPDF_REVERSE_BYTE_ORDER;
end;

procedure TPdfPage.Draw(DC: HDC; X, Y, Width, Height: Integer; Rotate: TPdfPageRotation; const Options: TPdfPageRenderOptions);
var
  BitmapInfo: TBitmapInfo;
  Bmp, OldBmp: HBITMAP;
  BmpBits: Pointer;
  PdfBmp: TPdfBitmap;
  BmpDC: HDC;
begin
  Open;

  if proPrinting in Options then
  begin
{$IFNDEF FORM_DISABLED}
    if IsValidForm and (FPDFPage_GetAnnotCount(FPage) > 0) then
    begin
      // Form content isn't printed unless it was flattend and the page was reloaded.
      ApplyChanges;
      Flatten(True);
      Close;
      Open;
    end;
{$ENDIF}
    FPDF_RenderPage(DC, FPage, X, Y, Width, Height, Ord(Rotate), GetDrawFlags(Options));
    Exit;
  end;


  FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);
  BitmapInfo.bmiHeader.biSize := SizeOf(BitmapInfo);
  BitmapInfo.bmiHeader.biWidth := Width;
  BitmapInfo.bmiHeader.biHeight := -Height;
  BitmapInfo.bmiHeader.biPlanes := 1;
  BitmapInfo.bmiHeader.biBitCount := 32;
  BitmapInfo.bmiHeader.biCompression := BI_RGB;
  BmpBits := nil;
  Bmp := CreateDIBSection(DC, BitmapInfo, DIB_RGB_COLORS, BmpBits, 0, 0);
  if Bmp <> 0 then
  begin
    try
      PdfBmp := TPdfBitmap.Create(Width, Height, bfBGRA, BmpBits, Width * 4);
      try
        if Transparency then
          PdfBmp.FillRect(0, 0, Width, Height, $00FFFFFF)
        else
          PdfBmp.FillRect(0, 0, Width, Height, $FFFFFFFF);
        DrawToPdfBitmap(PdfBmp, 0, 0, Width, Height, Rotate, Options);
{$IFNDEF FORM_DISABLED}
        DrawFormToPdfBitmap(PdfBmp, 0, 0, Width, Height, Rotate, Options);{$ENDIF}
      finally
        PdfBmp.Free;
      end;

      BmpDC := CreateCompatibleDC(DC);
      OldBmp := SelectObject(BmpDC, Bmp);
      BitBlt(DC, X, Y, Width, Height, BmpDC, 0, 0, SRCCOPY);
      SelectObject(BmpDC, OldBmp);
      DeleteDC(BmpDC);
    finally
      DeleteObject(Bmp);
    end;
  end;
end;

procedure TPdfPage.DrawToPdfBitmap(APdfBitmap: TPdfBitmap; X, Y, Width, Height: Integer;
  Rotate: TPdfPageRotation; const Options: TPdfPageRenderOptions);
begin
  Open;
  FPDF_RenderPageBitmap(APdfBitmap.FBitmap, FPage, X, Y, Width, Height, Ord(Rotate), GetDrawFlags(Options));
end;

{$IFNDEF FORM_DISABLED}
procedure TPdfPage.DrawFormToPdfBitmap(APdfBitmap: TPdfBitmap; X, Y, Width, Height: Integer;
  Rotate: TPdfPageRotation; const Options: TPdfPageRenderOptions);
begin
  Open;
  if IsValidForm then
  begin
    if proPrinting in Options then
    begin
      if FDocument.PrintHidesFormFieldHighlight then
        FPDF_RemoveFormFieldHighlight(FDocument.FForm);
        //FPDF_SetFormFieldHighlightAlpha(FDocument.FForm, 0); // hide the highlight
      FormEventKillFocus;
    end;
    try
      FPDF_FFLDraw(FDocument.FForm, APdfBitmap.FBitmap, FPage, X, Y, Width, Height, Ord(Rotate), GetDrawFlags(Options));
    finally
      if (proPrinting in Options) and FDocument.PrintHidesFormFieldHighlight then
        FDocument.UpdateFormFieldHighlight;
    end;
  end;
end;
{$ENDIF}

procedure TPdfPage.UpdateMetrics;
begin
  FWidth := FPDF_GetPageWidthF(FPage);
  FHeight := FPDF_GetPageHeightF(FPage);
  FTransparency := FPDFPage_HasTransparency(FPage) <> 0;
  FRotation := TPdfPageRotation(FPDFPage_GetRotation(FPage));
end;

function TPdfPage.DeviceToPage(X, Y, Width, Height: Integer; DeviceX, DeviceY: Integer; Rotate: TPdfPageRotation): TPdfPoint;
begin
  Open;
  FPDF_DeviceToPage(FPage, X, Y, Width, Height, Ord(Rotate), DeviceX, DeviceY, Result.X, Result.Y);
end;

function TPdfPage.PageToDevice(X, Y, Width, Height: Integer; PageX, PageY: Double;
  Rotate: TPdfPageRotation): TPoint;
begin
  Open;
  FPDF_PageToDevice(FPage, X, Y, Width, Height, Ord(Rotate), PageX, PageY, Result.X, Result.Y);
end;

function TPdfPage.DeviceToPage(X, Y, Width, Height: Integer; const R: TRect; Rotate: TPdfPageRotation): TPdfRect;
begin
  Result.TopLeft := DeviceToPage(X, Y, Width, Height, R.Left, R.Top, Rotate);
  Result.BottomRight := DeviceToPage(X, Y, Width, Height, R.Right, R.Bottom, Rotate);
end;

function TPdfPage.PageToDevice(X, Y, Width, Height: Integer; const R: TPdfRect; Rotate: TPdfPageRotation): TRect;
var
  T: Integer;
begin
  Result.TopLeft := PageToDevice(X, Y, Width, Height, R.Left, R.Top, Rotate);
  Result.BottomRight := PageToDevice(X, Y, Width, Height, R.Right, R.Bottom, Rotate);
  if Result.Top > Result.Bottom then
  begin
    T := Result.Top;
    Result.Top := Result.Bottom;
    Result.Bottom := T;
  end;
end;

procedure TPdfPage.SetRotation(const Value: TPdfPageRotation);
begin
  Open;
  FPDFPage_SetRotation(FPage, Ord(Value));
  FRotation := TPdfPageRotation(FPDFPage_GetRotation(FPage));
end;

procedure TPdfPage.ApplyChanges;
begin
  if FPage <> nil then
    FPDFPage_GenerateContent(FPage);
end;

procedure TPdfPage.Flatten(AFlatPrint: Boolean);
const
  Flags: array[Boolean] of Integer = (FLAT_NORMALDISPLAY, FLAT_PRINT);
begin
  if FPage <> nil then
    FPDFPage_Flatten(FPage, Flags[AFlatPrint]);
end;


function TPdfPage.BeginText: Boolean;
begin
  if FTextHandle = nil then
  begin
    Open;
    FTextHandle := FPDFText_LoadPage(FPage);
  end;
  Result := FTextHandle <> nil;
end;

function TPdfPage.BeginWebLinks: Boolean;
begin
  if (FLinkHandle = nil) and BeginText then
    FLinkHandle := FPDFLink_LoadWebLinks(FTextHandle);
  Result := FLinkHandle <> nil;
end;

function TPdfPage.BeginFind(const SearchString: string; MatchCase, MatchWholeWord,
  FromEnd: Boolean): Boolean;
var {$IFDEF FPC}pwc: PWideChar; {$ENDIF}
  Flags, StartIndex: Integer;
begin
  EndFind;
  if BeginText then
  begin
    Flags := 0;
    if MatchCase then
      Flags := Flags or FPDF_MATCHCASE;
    if MatchWholeWord then
      Flags := Flags or FPDF_MATCHWHOLEWORD;
    if FromEnd then
      StartIndex := -1
    else
      StartIndex := 0;
{$IFDEF FPC}
    GetMem(pwc, (Length(SearchString)+1)*Sizeof(WideChar));
    try
      StrPCopy(pwc, unicodeString(SearchString));
      FSearchHandle := FPDFText_FindStart(FTextHandle,
                       pwc, Flags, StartIndex);
    finally
      Freemem(pwc);
    end;
{$ELSE}
    FSearchHandle := FPDFText_FindStart(FTextHandle, PChar(SearchString), Flags, StartIndex);
{$ENDIF}
  end;
  Result := FSearchHandle <> nil;
end;

procedure TPdfPage.EndFind;
begin
  if FSearchHandle <> nil then
  begin
    FPDFText_FindClose(FSearchHandle);
    FSearchHandle := nil;
  end;
end;

function TPdfPage.FindNext(var CharIndex, Count: Integer): Boolean;
begin
  CharIndex := 0;
  Count := 0;
  if FSearchHandle <> nil then
  begin
    Result := FPDFText_FindNext(FSearchHandle) <> 0;
    if Result then
    begin
      CharIndex := FPDFText_GetSchResultIndex(FSearchHandle);
      Count := FPDFText_GetSchCount(FSearchHandle);
    end;
  end
  else
    Result := False;
end;

function TPdfPage.FindPrev(var CharIndex, Count: Integer): Boolean;
begin
  CharIndex := 0;
  Count := 0;
  if FSearchHandle <> nil then
  begin
    Result := FPDFText_FindPrev(FSearchHandle) <> 0;
    if Result then
    begin
      CharIndex := FPDFText_GetSchResultIndex(FSearchHandle);
      Count := FPDFText_GetSchCount(FSearchHandle);
    end;
  end
  else
    Result := False;
end;

function TPdfPage.GetCharCount: Integer;
begin
  if BeginText then
    Result := FPDFText_CountChars(FTextHandle)
  else
    Result := 0;
end;

function TPdfPage.ReadChar(CharIndex: Integer): WideChar;
begin
  if BeginText then
    Result := FPDFText_GetUnicode(FTextHandle, CharIndex)
  else
    Result := #0;
end;

function TPdfPage.GetCharFontSize(CharIndex: Integer): Double;
begin
  if BeginText then
    Result := FPDFText_GetFontSize(FTextHandle, CharIndex)
  else
    Result := 0;
end;

function TPdfPage.GetCharBox(CharIndex: Integer): TPdfRect;
begin
  if BeginText then
    FPDFText_GetCharBox(FTextHandle, CharIndex, Result.Left, Result.Right, Result.Bottom, Result.Top)
  else
    Result := TPdfRect.Empty;
end;

function TPdfPage.GetCharIndexAt(PageX, PageY, ToleranceX, ToleranceY: Double): Integer;
begin
  if BeginText then
    Result := FPDFText_GetCharIndexAtPos(FTextHandle, PageX, PageY, ToleranceX, ToleranceY)
  else
    Result := 0;
end;

function TPdfPage.ReadText(CharIndex, Count: Integer): string;// {$IFDEF FPC}WideString{$ELSE}string{$ENDIF};
var
  Len: Integer; {$IFDEF FPC}pwc: PWideChar;{$ENDIF}
begin
  if (Count > 0) and BeginText then
    begin
{$IFDEF FPC}
      Getmem(pwc, (Count+1)*Sizeof(WideChar));
      try
        Len := FPDFText_GetText(FTextHandle, CharIndex, Count, pwc) - 1; // returned length includes the #0
        if Len <= 0 then
          Result := ''
        else
          result := UnicodeString(pwc); //WideCharToString(pwc);
      finally
        Freemem(pwc)
      end
{$ELSE}
      SetLength(Result, Count); // we let GetText overwrite our #0 terminator with its #0
      Len := FPDFText_GetText(FTextHandle, CharIndex, Count, PChar(Result)) - 1; // returned length includes the #0
      if Len <= 0 then
        Result := ''
      else if Len < Count then
        SetLength(Result, Len);
{$ENDIF}
    end
  else
    Result := '';
end;

function TPdfPage.GetTextAt(Left, Top, Right, Bottom: Double): string;
var   {$IFDEF FPC}pwc: PWideChar;{$ENDIF}
  Len: Integer;
begin
  if BeginText then
  begin
    Len := FPDFText_GetBoundedText(FTextHandle, Left, Top, Right, Bottom, nil, 0); // excluding #0 terminator
{$IFDEF FPC}
    pwc := PWideChar((AllocMem((Len+1)*Sizeof(PWideChar) )));
    try
      if Len > 0 then
        FPDFText_GetBoundedText(FTextHandle, Left, Top, Right, Bottom,
                  pwc, Len);
       result := Strpas(pwc)
    finally
      FreeMem(pwc);
    end;
{$ELSE}
    SetLength(Result, Len);
    if Len > 0 then
      FPDFText_GetBoundedText(FTextHandle, Left, Top, Right, Bottom,
               PChar(Result), Len);
{$ENDIF}
  end
  else
    Result := '';
end;

function TPdfPage.GetTextAt(const R: TPdfRect): string;
begin
  Result := GetTextAt(R.Left, R.Top, R.Right, R.Bottom);
end;

function TPdfPage.GetTextRectCount(CharIndex, Count: Integer): Integer;
begin
  if BeginText then
    Result := FPDFText_CountRects(FTextHandle, CharIndex, Count)
  else
    Result := 0;
end;

function TPdfPage.GetTextRect(RectIndex: Integer): TPdfRect;
begin
  if BeginText then
    FPDFText_GetRect(FTextHandle, RectIndex, Result.Left, Result.Top, Result.Right, Result.Bottom)
  else
    Result := TPdfRect.Empty;
end;

function TPdfPage.GetWebLinkCount: Integer;
begin
  if BeginWebLinks then
  begin
    Result := FPDFLink_CountWebLinks(FLinkHandle);
    if Result < 0 then
      Result := 0;
  end
  else
    Result := 0;
end;

function TPdfPage.GetWebLinkURL(LinkIndex: Integer): string;
var
  Len: Integer;  {$IFDEF FPC} pwc: PWideChar; {$ENDIF}
begin
  Result := '';
  if BeginWebLinks then
  begin
    Len := FPDFLink_GetURL(FLinkHandle, LinkIndex, nil, 0) - 1; // including #0 terminator
    if Len > 0 then
    begin
{$IFDEF FPC}
      Getmem(pwc, (len + 1) * SizeOf(WideChar));
      try
        FPDFLink_GetURL(FLinkHandle, LinkIndex, pwc, Len + 1); // including #0 terminator
        Result := WideCharToString(PWC); //WideCharToString(pwc);
      finally
        Freemem(pwc);
      end;
{$ELSE}
      SetLength(Result, Len);
      FPDFLink_GetURL(FLinkHandle, LinkIndex, (Result), Len + 1); // including #0 terminator
{$ENDIF}
    end;
  end;
end;

function TPdfPage.GetWebLinkRectCount(LinkIndex: Integer): Integer;
begin
  if BeginWebLinks then
    Result := FPDFLink_CountRects(FLinkHandle, LinkIndex)
  else
    Result := 0;
end;

function TPdfPage.GetWebLinkRect(LinkIndex, RectIndex: Integer): TPdfRect;
begin
  if BeginWebLinks then
    FPDFLink_GetRect(FLinkHandle, LinkIndex, RectIndex, Result.Left, Result.Top, Result.Right, Result.Bottom)
  else
    Result := TPdfRect.Empty;
end;

function TPdfPage.GetMouseModifier(const Shift: TShiftState): Integer;
begin
  Result := 0;
  if ssShift in Shift then
    Result := Result or FWL_EVENTFLAG_ShiftKey;
  if ssCtrl in Shift then
    Result := Result or FWL_EVENTFLAG_ControlKey;
  if ssAlt in Shift then
    Result := Result or FWL_EVENTFLAG_AltKey;
  if ssLeft in Shift then
    Result := Result or FWL_EVENTFLAG_LeftButtonDown;
  if ssMiddle in Shift then
    Result := Result or FWL_EVENTFLAG_MiddleButtonDown;
  if ssRight in Shift then
    Result := Result or FWL_EVENTFLAG_RightButtonDown;
end;

function TPdfPage.GetKeyModifier(KeyData: LPARAM): Integer;
const
  AltMask = $20000000;
begin
  Result := 0;
  if GetKeyState(VK_SHIFT) < 0 then
    Result := Result or FWL_EVENTFLAG_ShiftKey;
  if GetKeyState(VK_CONTROL) < 0 then
    Result := Result or FWL_EVENTFLAG_ControlKey;
  if KeyData and AltMask <> 0 then
    Result := Result or FWL_EVENTFLAG_AltKey;
end;

{$IFNDEF FORM_DISABLED}
function TPdfPage.FormEventFocus(const Shift: TShiftState; PageX, PageY: Double): Boolean;
begin
  if IsValidForm then
    Result := FORM_OnFocus(FDocument.FForm, FPage, GetMouseModifier(Shift), PageX, PageY) <> 0
  else
    Result := False;
end;

function TPdfPage.FormEventMouseWheel(const Shift: TShiftState; WheelDelta: Integer; PageX, PageY: Double): Boolean;
var
  Pt: TFSPointF;
  WheelX, WheelY: Integer;
begin
  if IsValidForm then
  begin
    Pt.X := PageX;
    Pt.Y := PageY;
    WheelX := 0;
    WheelY := 0;
    if ssShift in Shift then
      WheelX := WheelDelta
    else
      WheelY := WheelDelta;
    Result := FORM_OnMouseWheel(FDocument.FForm, FPage, GetMouseModifier(Shift), @Pt, WheelX, WheelY) <> 0;
  end
  else
    Result := False;
end;

function TPdfPage.FormEventMouseMove(const Shift: TShiftState; PageX, PageY: Double): Boolean;
begin
  if IsValidForm then
    Result := FORM_OnMouseMove(FDocument.FForm, FPage, GetMouseModifier(Shift), PageX, PageY) <> 0
  else
    Result := False;
end;

function TPdfPage.FormEventLButtonDown(const Shift: TShiftState; PageX, PageY: Double): Boolean;
begin
  if IsValidForm then
    Result := FORM_OnLButtonDown(FDocument.FForm, FPage, GetMouseModifier(Shift), PageX, PageY) <> 0
  else
    Result := False;
end;

function TPdfPage.FormEventLButtonUp(const Shift: TShiftState; PageX, PageY: Double): Boolean;
begin
  if IsValidForm then
    Result := FORM_OnLButtonUp(FDocument.FForm, FPage, GetMouseModifier(Shift), PageX, PageY) <> 0
  else
    Result := False;
end;

function TPdfPage.FormEventRButtonDown(const Shift: TShiftState; PageX, PageY: Double): Boolean;
begin
  if IsValidForm then
    Result := FORM_OnRButtonDown(FDocument.FForm, FPage, GetMouseModifier(Shift), PageX, PageY) <> 0
  else
    Result := False;
end;

function TPdfPage.FormEventRButtonUp(const Shift: TShiftState; PageX, PageY: Double): Boolean;
begin
  if IsValidForm then
    Result := FORM_OnRButtonUp(FDocument.FForm, FPage, GetMouseModifier(Shift), PageX, PageY) <> 0
  else
    Result := False;
end;

function TPdfPage.FormEventKeyDown(KeyCode: Word; KeyData: LPARAM): Boolean;
begin
  if IsValidForm then
    Result := FORM_OnKeyDown(FDocument.FForm, FPage, KeyCode, GetKeyModifier(KeyData)) <> 0
  else
    Result := False;
end;

function TPdfPage.FormEventKeyUp(KeyCode: Word; KeyData: LPARAM): Boolean;
begin
  if IsValidForm then
    Result := FORM_OnKeyUp(FDocument.FForm, FPage, KeyCode, GetKeyModifier(KeyData)) <> 0
  else
    Result := False;
end;

function TPdfPage.FormEventKeyPress(Key: Word; KeyData: LPARAM): Boolean;
begin
  if IsValidForm then
    Result := FORM_OnChar(FDocument.FForm, FPage, Key, GetKeyModifier(KeyData)) <> 0
  else
    Result := False;
end;

function TPdfPage.FormEventKillFocus: Boolean;
begin
  if IsValidForm then
    Result := FORM_ForceToKillFocus(FDocument.FForm) <> 0
  else
    Result := False;
end;

function TPdfPage.FormGetFocusedText: string;
var
  ByteLen: LongWord; {$IfDEF FPC}pwc: PWideChar; ii: LongWord; {$ENDIF}
begin
  if IsValidForm then
    begin
      ByteLen := FORM_GetFocusedText(FDocument.FForm, FPage, nil, 0); // UTF 16 including #0 terminator in byte size
      if ByteLen <= 2 then // WideChar(#0) => empty string
        Result := ''
      else
        begin
{$IfDEF FPC}
          ii := ByteLen + SizeOf(WideChar);
          pwc := PWideChar(AllocMem(ii));
          try
            FORM_GetFocusedText(FDocument.FForm, FPage, pwc, ii);
            result := StrPas(pwc);
          finally
            freeMem(pwc)
          end;
{$ELSE}
          SetLength(Result, ByteLen div SizeOf(WideChar) - 1);
          FORM_GetFocusedText(FDocument.FForm, FPage, PWideChar(Result), ByteLen);
{$ENDIF}
        end;
    end
  else
    Result := '';
end;

function TPdfPage.FormGetSelectedText: string;
{$IFDEF FPC}var pwc: PWideChar; {$ENDIF}
var
  ByteLen: LongWord;
begin
  if IsValidForm then
    begin
      ByteLen := FORM_GetSelectedText(FDocument.FForm, FPage, nil, 0); // UTF 16 including #0 terminator in byte size
      if ByteLen <= 2 then // WideChar(#0) => empty string
        Result := ''
      else
        begin
{$IFDEF FPC}
          Getmem(pwc, ByteLen);
          try
            FORM_GetSelectedText(FDocument.FForm, FPage, PWC, ByteLen);
            result := StrPas(pwc);
          finally
            FreeMem(pwc);
          end;
{$ELSE}
          SetLength(Result, ByteLen div SizeOf(WideChar) - 1);
          FORM_GetSelectedText(FDocument.FForm, FPage, PWideChar(Result), ByteLen);
{$ENDIF}
        end;
    end
  else
    Result := '';
end;

function TPdfPage.FormReplaceAndKeepSelection(const ANewText: string): Boolean;
{$IFDEF FPC}var pwc: PWideChar; {$ENDIF}
begin
  if IsValidForm then
    begin
{$IFDEF FPC}
      Getmem(pwc, (Length(ANewText)+1)*Sizeof(WideChar));
      try
        StrPCopy(pwc, ANewText);
        FORM_ReplaceSelection(FDocument.FForm, FPage, pwc);
      finally
        FreeMem(pwc);
      end;
{$ELSE}
      FORM_ReplaceSelection(FDocument.FForm, FPage, PWideChar(ANewText));
{$ENDIF}
      Result := True;
    end
  else
    Result := False;
end;

function TPdfPage.FormSelectAllText: Boolean;
begin
  if IsValidForm then
    Result := FORM_SelectAllText(FDocument.FForm, FPage) <> 0
  else
    Result := False;
end;

function TPdfPage.FormCanUndo: Boolean;
begin
  if IsValidForm then
    Result := FORM_CanUndo(FDocument.FForm, FPage) <> 0
  else
    Result := False;
end;

function TPdfPage.FormCanRedo: Boolean;
begin
  if IsValidForm then
    Result := FORM_CanRedo(FDocument.FForm, FPage) <> 0
  else
    Result := False;
end;

function TPdfPage.FormUndo: Boolean;
begin
  if IsValidForm then
    Result := FORM_Undo(FDocument.FForm, FPage) <> 0
  else
    Result := False;
end;

function TPdfPage.FormRedo: Boolean;
begin
  if IsValidForm then
    Result := FORM_Redo(FDocument.FForm, FPage) <> 0
  else
    Result := False;
end;

function TPdfPage.HasFormFieldAtPoint(X, Y: Double): TPdfFormFieldType;
begin
  case FPDFPage_HasFormFieldAtPoint(FDocument.FForm, FPage, X, Y) of
    FPDF_FORMFIELD_PUSHBUTTON:
      Result := fftPushButton;
    FPDF_FORMFIELD_CHECKBOX:
      Result := fftCheckBox;
    FPDF_FORMFIELD_RADIOBUTTON:
      Result := fftRadioButton;
    FPDF_FORMFIELD_COMBOBOX:
      Result := fftComboBox;
    FPDF_FORMFIELD_LISTBOX:
      Result := fftListBox;
    FPDF_FORMFIELD_TEXTFIELD:
      Result := fftTextField;
    FPDF_FORMFIELD_SIGNATURE:
      Result := fftSignature;
  else
    Result := fftUnknown;
  end;
end;
{$ENDIF}

function TPdfPage.GetHandle: FPDF_PAGE;
begin
  Open;
  Result := FPage;
end;

function TPdfPage.IsLoaded: Boolean;
begin
  Result := FPage <> nil;
end;

function TPdfPage.GetTextHandle: FPDF_TEXTPAGE;
begin
  if BeginText then
    Result := FTextHandle
  else
    Result := nil;
end;

function TPdfPage.GetFormFields: TPdfFormFieldList;
begin
  Result := Annotations.FormFields;
end;
{ _TPdfBitmapHideCtor }

procedure _TPdfBitmapHideCtor.Create;
begin
  inherited Create;
end;

{ TPdfBitmap }

constructor TPdfBitmap.Create(ABitmap: FPDF_BITMAP; AOwnsBitmap: Boolean);
begin
  inherited Create;
  FBitmap := ABitmap;
  FOwnsBitmap := AOwnsBitmap;
  if FBitmap <> nil then
  begin
    FWidth := FPDFBitmap_GetWidth(FBitmap);
    FHeight := FPDFBitmap_GetHeight(FBitmap);
    FBytesPerScanLine := FPDFBitmap_GetStride(FBitmap);
  end;
end;

constructor TPdfBitmap.Create(AWidth, AHeight: Integer; AAlpha: Boolean);
begin
  Create(FPDFBitmap_Create(AWidth, AHeight, Ord(AAlpha)), True);
end;

constructor TPdfBitmap.Create(AWidth, AHeight: Integer; AFormat: TPdfBitmapFormat);
begin
  Create(FPDFBitmap_CreateEx(AWidth, AHeight, Ord(AFormat), nil, 0), True);
end;

constructor TPdfBitmap.Create(AWidth, AHeight: Integer; AFormat: TPdfBitmapFormat; ABuffer: Pointer;
  ABytesPerScanLine: Integer);
begin
  Create(FPDFBitmap_CreateEx(AWidth, AHeight, Ord(AFormat), ABuffer, ABytesPerScanline), True);
end;

destructor TPdfBitmap.Destroy;
begin
  if FOwnsBitmap and (FBitmap <> nil) then
    FPDFBitmap_Destroy(FBitmap);
  inherited Destroy;
end;

function TPdfBitmap.GetBuffer: Pointer;
begin
  if FBitmap <> nil then
    Result := FPDFBitmap_GetBuffer(FBitmap)
  else
    Result := nil;
end;

procedure TPdfBitmap.FillRect(ALeft, ATop, AWidth, AHeight: Integer; AColor: FPDF_DWORD);
begin
  if FBitmap <> nil then
    FPDFBitmap_FillRect(FBitmap, ALeft, ATop, AWidth, AHeight, AColor);
end;

{ TPdfPoint }

procedure TPdfPoint.Offset(XOffset, YOffset: Double);
begin
  X := X + XOffset;
  Y := Y + YOffset;
end;

class function TPdfPoint.Empty: TPdfPoint;
begin
  Result.X := 0;
  Result.Y := 0;
end;

{ TPdfAttachmentList }

constructor TPdfAttachmentList.Create(ADocument: TPdfDocument);
begin
  inherited Create;
  FDocument := ADocument;
end;

function TPdfAttachmentList.GetCount: Integer;
begin
  FDocument.CheckActive;
  Result := FPDFDoc_GetAttachmentCount(FDocument.Handle);
end;

function TPdfAttachmentList.GetItem(Index: Integer): TPdfAttachment;
var
  Attachment: FPDF_ATTACHMENT;
begin
  FDocument.CheckActive;
  Attachment := FPDFDoc_GetAttachment(FDocument.Handle, Index);
  if Attachment = nil then
    raise EPdfArgumentOutOfRange.CreateResFmt(@RsArgumentsOutOfRange, ['Index']);
  Result.FDocument := FDocument;
  Result.FHandle := Attachment;
end;

procedure TPdfAttachmentList.Delete(Index: Integer);
begin
  FDocument.CheckActive;
  if FPDFDoc_DeleteAttachment(FDocument.Handle, Index) = 0 then
    raise EPdfException.CreateResFmt(@RsPdfCannotDeleteAttachmnent, [Index]);
end;

function TPdfAttachmentList.Add(const Name: string): TPdfAttachment;
begin
  FDocument.CheckActive;
  Result.FDocument := FDocument;
{$IFDEF FPC}
  Result.FHandle := FPDFDoc_AddAttachment(FDocument.Handle, PWideChar(utf8decode(Name)));
{$ELSE}
  Result.FHandle := FPDFDoc_AddAttachment(FDocument.Handle, PWideChar(Name));

{$ENDIF}
  if Result.FHandle = nil then
    raise EPdfException.CreateResFmt(@RsPdfCannotAddAttachmnent, [Name]);
end;

{ TPdfAttachment }

function TPdfAttachment.GetName: string;
var
  ByteLen: LongWord; {$IFDEF FPC} pwc: PWideChar; {$ENDIF}
begin
  CheckValid;
  ByteLen := FPDFAttachment_GetName(Handle, nil, 0); // UTF 16 including #0 terminator in byte size
  if ByteLen <= 2 then
    Result := ''
  else
  begin
{$IFDEF FPC}
    GetMem(pwc, ByteLen); // div SizeOf(WideChar) - 1);
    try
      FPDFAttachment_GetName(FHandle, pwc, ByteLen);
      result := StrPas(pwc);
    finally
      Freemem(pwc);
    end;
{$ELSE}
    SetLength(Result, ByteLen div SizeOf(WideChar) - 1);
    FPDFAttachment_GetName(FHandle, PWideChar(Result), ByteLen);
{$ENDIF}
  end;
end;

procedure TPdfAttachment.CheckValid;
begin
  if FDocument <> nil then
    FDocument.CheckActive;
end;

procedure TPdfAttachment.SetContent(ABytes: PByte; Count: Integer);
begin
  CheckValid;
  if FPDFAttachment_SetFile(FHandle, FDocument.Handle, ABytes, Count) = 0 then
    raise EPdfException.CreateResFmt(@RsPdfCannotSetAttachmentContent, [Name]);
end;

procedure TPdfAttachment.SetContent(const Value: RawByteString);
begin
  if Value = '' then
    SetContent(nil, 0)
  else
    SetContent(PByte(PAnsiChar(Value)), Length(Value) * SizeOf(AnsiChar));
end;

procedure TPdfAttachment.SetContent(const Value: string; Encoding: TEncoding = nil);
begin
  CheckValid;
  if Value = '' then
    SetContent(nil, 0)
  else if (Encoding = nil) or (Encoding = TEncoding.UTF8) then
    SetContent(UTF8Encode(Value))
  else
    SetContent(Encoding.GetBytes(Value));
end;

procedure TPdfAttachment.SetContent(const ABytes: TBytes; Index: NativeInt; Count: Integer);
var
  Len: NativeInt;
begin
  CheckValid;

  Len := Length(ABytes);
  if Index >= Len then
    raise EPdfArgumentOutOfRange.CreateResFmt(@RsArgumentsOutOfRange, ['Index', Index]);
  if Index + Count > Len then
    raise EPdfArgumentOutOfRange.CreateResFmt(@RsArgumentsOutOfRange, ['Count', Count]);

  if Count = 0 then
    SetContent(nil, 0)
  else
    SetContent(@ABytes[Index], Count);
end;

procedure TPdfAttachment.SetContent(const ABytes: TBytes);
begin
  SetContent(ABytes, 0, Length(ABytes));
end;

procedure TPdfAttachment.LoadFromStream(Stream: TStream);
var
  StreamPos, StreamSize: Int64;
  Buf: PByte;
  Count: Integer;
begin
  CheckValid;

  StreamPos := Stream.Position;
  StreamSize := Stream.Size;
  Count := StreamSize - StreamPos;
  if Count = 0 then
    SetContent(nil, 0)
  else
  begin
    if Stream is TCustomMemoryStream then // direct access to the memory
    begin
      SetContent(PByte(TCustomMemoryStream(Stream).Memory) + StreamPos, Count);
      Stream.Position := StreamSize; // simulate the ReadBuffer call
    end
    else
    begin
      if Count = 0 then
        SetContent(nil, 0)
      else
      begin
        GetMem(Buf, Count);
        try
          Stream.ReadBuffer(Buf^, Count);
          SetContent(Buf, Count);
        finally
          FreeMem(Buf);
        end;
      end;
    end;
  end;
end;

procedure TPdfAttachment.LoadFromFile(const FileName: string);
var
  Stream: TFileStream;
begin
  CheckValid;

  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TPdfAttachment.HasKey(const Key: string): Boolean;
begin
  CheckValid;
  Result := FPDFAttachment_HasKey(FHandle, PAnsiChar(Key)) <> 0;
//  Result := FPDFAttachment_HasKey(FHandle, PAnsiChar(UTF8Encode(Key))) <> 0;
end;

function TPdfAttachment.GetValueType(const Key: string): TPdfObjectType;
var ansi: AnsiString;
begin
  CheckValid;
  ansi := UTF8Encode(Key);
  Result := TPdfObjectType(FPDFAttachment_GetValueType(FHandle, PAnsiChar(ansi)));
//  Result := TPdfObjectType(FPDFAttachment_GetValueType(FHandle, PAnsiChar(UTF8Encode(Key))));
end;

procedure TPdfAttachment.SetKeyValue(const Key, Value: string);
{$IFDEF FPC}var pwc: PWideChar; {$ENDIF}
begin
  CheckValid;
{$IFDEF FPC}
  pwc := PWideChar(AllocMem((Length(Value) + 1)*Sizeof(WideChar) ));
  StrPCopy(pwc, Value);
  if FPDFAttachment_SetStringValue(FHandle, PAnsiChar(UTF8Encode(Key)), pwc) = 0 then
    raise EPdfException.CreateRes(@RsPdfAttachmentContentNotSet);
  Freemem(pwc);
{$ELSE}
  if FPDFAttachment_SetStringValue(FHandle, PAnsiChar(UTF8Encode(Key)), PWideChar(Value)) = 0 then
    raise EPdfException.CreateRes(@RsPdfAttachmentContentNotSet);
{$ENDIF}
end;

function TPdfAttachment.GetKeyValue(const Key: string): string;
var
  ByteLen: LongWord;
  Utf8Key: UTF8String;  {$IFDEF FPC} pwc: PWideChar; {$ENDIF}
begin
  CheckValid;
  Utf8Key := UTF8Encode(Key);
  ByteLen := FPDFAttachment_GetStringValue(FHandle, PAnsiChar(Utf8Key), nil, 0);
  if ByteLen = 0 then
    raise EPdfException.CreateRes(@RsPdfAttachmentContentNotSet);
  if ByteLen <= 2 then
    Result := ''
  else
  begin
 {$IFDEF FPC}
    pwc := PWideChar(AllocMem(ByteLen));
    try
      FPDFAttachment_GetStringValue(FHandle, PAnsiChar(UTF8Encode(Key)), pwc, ByteLen);
      result := StrPas(pwc);
    finally
      Freemem(pwc);
    end;
 {$ELSE}
    SetLength(Result, (ByteLen div SizeOf(WideChar) - 1));
    FPDFAttachment_GetStringValue(FHandle, PAnsiChar(Utf8Key), @PWideChar(Result), ByteLen);
 {$ENDIF}
  end;
end;

function TPdfAttachment.GetContentSize: Integer;
var
  OutBufLen: LongWord;
begin
  CheckValid;
  if FPDFAttachment_GetFile(FHandle, nil, 0, OutBufLen) = 0 then
    Result := 0
  else
    Result := Integer(OutBufLen);
end;

function TPdfAttachment.HasContent: Boolean;
var
  OutBufLen: LongWord;
begin
  CheckValid;
  Result := FPDFAttachment_GetFile(FHandle, nil, 0, OutBufLen) <> 0;
end;

procedure TPdfAttachment.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  CheckValid;

  Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TPdfAttachment.SaveToStream(Stream: TStream);
var
  Size: Integer;
  OutBufLen: LongWord;
  StreamPos: Int64;
  Buf: PByte;
begin
  Size := ContentSize;

  if Size > 0 then
  begin
    if Stream is TCustomMemoryStream then // direct access to the memory
    begin
      StreamPos := Stream.Position;
      if StreamPos + Size > Stream.Size then
        Stream.Size := StreamPos + Size; // allocate enough memory
      Stream.Position := StreamPos;

      FPDFAttachment_GetFile(FHandle, PByte(TCustomMemoryStream(Stream).Memory) + StreamPos, Size, OutBufLen);
      Stream.Position := StreamPos + Size; // simulate Stream.WriteBuffer
    end
    else
    begin
      GetMem(Buf, Size);
      try
        FPDFAttachment_GetFile(FHandle, Buf, Size, OutBufLen);
        Stream.WriteBuffer(Buf^, Size);
      finally
        FreeMem(Buf);
      end;
    end;
  end;
end;

procedure TPdfAttachment.GetContent(var Value: string; Encoding: TEncoding);
var
  ii, Size: Integer;
  OutBufLen: LongWord;
  Buf: PByte;  {$IFDEF FPC} pwc: PWideChar;{$ENDIF}
begin
  Size := ContentSize;
  if Size <= 0 then
    Value := ''

  else if Encoding = TEncoding.Unicode then // no conversion needed
  begin
{$IFDEF FPC}
 // this part may not be necessay as the next block can also handle unicode type
    pwc := PWideChar(AllocMem( Size)); //+2));
    try
      FPDFAttachment_GetFile(FHandle, pwc, Size, OutBufLen);
      Value := StrPas(pwc);
    finally
      Freemem(pwc);
    end;
{$ELSE}
    SetLength(Value, Size div SizeOf(WideChar));
    FPDFAttachment_GetFile(FHandle, PWideChar(Value), Size, OutBufLen);}
{$ENDIf}
  end
    else
      begin
        if Encoding = nil then
          Encoding := TEncoding.UTF8;
{$IFDEF FPC}
        GetMem(Buf, Size);
        try
          FPDFAttachment_GetFile(FHandle, Buf, Size, OutBufLen);
          ii := Encoding.GetMemCharCount(Buf, Size)+1;
          pwc := PWideChar(AllocMem( ii*Sizeof(WideChar)));
          try
            if ii >1 then
              begin
                Encoding.GetMemChars(Buf, Size, PChar(pwc), ii);
                Value := StrPas(pwc);
              end;
          finally
            Freemem(pwc);
          end;
{$ELSE}
        SetLength(Value, TEncodingAccess(Encoding).GetMemCharCount(Buf, Size));
          if Value <> '' then
            TEncodingAccess(Encoding).GetMemChars(Buf, Size,
                           PWideChar(Value), Length(Value));
{$ENDIF}
        finally
          FreeMem(Buf);
        end;
      end;
end;

procedure TPdfAttachment.GetContent(var Value: RawByteString);
var
  Size: Integer;
  OutBufLen: LongWord;
begin
  Size := ContentSize;

  if Size <= 0 then
    Value := ''
  else
  begin
    SetLength(Value, Size);
    FPDFAttachment_GetFile(FHandle, PAnsiChar(Value), Size, OutBufLen);
  end;
end;

procedure TPdfAttachment.GetContent(Buffer: PByte);
var
  OutBufLen: LongWord;
begin
  FPDFAttachment_GetFile(FHandle, Buffer, ContentSize, OutBufLen);
end;

procedure TPdfAttachment.GetContent(var ABytes: TBytes);
var
  Size: Integer;
  OutBufLen: LongWord;
begin
  Size := ContentSize;

  if Size <= 0 then
    ABytes := nil
  else
  begin
    SetLength(ABytes, Size);
    FPDFAttachment_GetFile(FHandle, @ABytes[0], Size, OutBufLen);
  end;
end;

function TPdfAttachment.GetContentAsBytes: TBytes;
begin
  GetContent(Result);
end;

function TPdfAttachment.GetContentAsRawByteString: RawByteString;
begin
  GetContent(Result);
end;

function TPdfAttachment.GetContentAsString(Encoding: TEncoding): string;
begin
  GetContent(Result, Encoding);
end;

{ TPdfAnnotationList }

constructor TPdfAnnotationList.Create(APage: TPdfPage);
begin
  inherited Create;
  FPage := APage;
  FItems := TObjectList.Create;
end;

destructor TPdfAnnotationList.Destroy;
begin
  FreeAndNil(FFormFields);
  FreeAndNil(FItems); // closes all annotations
  inherited Destroy;
end;

procedure TPdfAnnotationList.CloseAnnotations;
begin
  FreeAndNil(FFormFields);
  FreeAndNil(FItems); // closes all annotations
  FItems := TObjectList.Create;
end;

function TPdfAnnotationList.GetCount: Integer;
begin
  Result := FPDFPage_GetAnnotCount(FPage.Handle);
end;

function TPdfAnnotationList.GetItem(Index: Integer): TPdfAnnotation;
var
  Annot: FPDF_ANNOTATION;
begin
  FPage.FDocument.CheckActive;

  if (Index < 0) or (Index >= FItems.Count) or (FItems[Index] = nil) then
  begin
    Annot := FPDFPage_GetAnnot(FPage.Handle, Index);
    if Annot = nil then
      raise EPdfArgumentOutOfRange.CreateResFmt(@RsArgumentsOutOfRange, ['Index']);

    while FItems.Count <= Index do
      FItems.Add(nil);
    FItems[Index] := TPdfAnnotation.Create(FPage, Annot);
  end;
  Result := FItems[Index] as TPdfAnnotation;
end;

procedure TPdfAnnotationList.DestroyingItem(Item: TPdfAnnotation);
var
  Index: Integer;
begin
  if (Item <> nil) and (FItems <> nil) then
  begin
    Index := FItems.IndexOf(Item);
    if Index <> -1 then
      FItems.List[Index] := nil; // Bypass the Items[] setter to not destroy the Item twice
  end;
end;

procedure TPdfAnnotationList.DestroyingFormField(FormField: TPdfFormField);
begin
  if FFormFields <> nil then
    FFormFields.DestroyingItem(FormField);
end;

function TPdfAnnotationList.GetFormFields: TPdfFormFieldList;
begin
  if FFormFields = nil then
    FFormFields := TPdfFormFieldList.Create(Self);
  Result := FFormFields;
end;

{ TPdfFormFieldList }

constructor TPdfFormFieldList.Create(AAnnotations: TPdfAnnotationList);
var
  I: Integer;
begin
  inherited Create;
  FItems := TList.Create;

  for I := 0 to AAnnotations.Count - 1 do
    if AAnnotations[I].IsFormField then
      FItems.Add(AAnnotations[I].FormField);
end;

destructor TPdfFormFieldList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TPdfFormFieldList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TPdfFormFieldList.GetItem(Index: Integer): TPdfFormField;
begin
  Result := TObject(FItems[Index]) as TPdfFormField;
end;

procedure TPdfFormFieldList.DestroyingItem(Item: TPdfFormField);
begin
  if (Item <> nil) and (FItems <> nil) then
    FItems.Extract(Item);
end;


{ TPdfAnnotation }

constructor TPdfAnnotation.Create(APage: TPdfPage; AHandle: FPDF_ANNOTATION);
begin
  inherited Create;
  FPage := APage;
  FHandle := AHandle;
end;

destructor TPdfAnnotation.Destroy;
begin
  FreeAndNil(FFormField);
  if FHandle <> nil then
  begin
    FPDFPage_CloseAnnot(FHandle);
    FHandle := nil;
  end;
  if FPage.FAnnotations <> nil then
    FPage.FAnnotations.DestroyingItem(Self);
  inherited Destroy;
end;

function TPdfAnnotation.IsFormField: Boolean;
begin
  case FPDFAnnot_GetSubtype(Handle) of
    FPDF_ANNOT_WIDGET,
    FPDF_ANNOT_XFAWIDGET:
      Result := True;
  else
    Result := False;
  end;
end;

function TPdfAnnotation.GetFormField: TPdfFormField;
begin
  if FFormField = nil then
  begin
    if not IsFormField then
      raise EPdfException.CreateRes(@RsPdfAnnotationNotAFormFieldError);
    FFormField := TPdfFormField.Create(Self);
  end;
  Result := FFormField;
end;

{ TPdfFormField }

constructor TPdfFormField.Create(AAnnotation: TPdfAnnotation);
begin
  inherited Create;
  FAnnotation := AAnnotation;
  FPage := FAnnotation.FPage;
  FHandle := FAnnotation.Handle;
end;

destructor TPdfFormField.Destroy;
begin
  FAnnotation.FFormField := nil;
  FAnnotation.FPage.Annotations.DestroyingFormField(Self);
  inherited Destroy;
end;

function TPdfFormField.IsXFAFormField: Boolean;
begin
  Result := IS_XFA_FORMFIELD(FPDFAnnot_GetFormFieldType(FPage.FDocument.FormHandle, Handle));
end;

function TPdfFormField.GetReadOnly: Boolean;
begin
  Result := fffReadOnly in Flags;
end;

function TPdfFormField.GetFlags: TPdfFormFieldFlags;
var
  FormFlags: Integer;
begin
  FormFlags := FPDFAnnot_GetFormFieldFlags(FPage.FDocument.FormHandle, Handle);

  Result := [];
  if FormFlags <> FPDF_FORMFLAG_NONE then
  begin
    if FormFlags and FPDF_FORMFLAG_READONLY <> 0 then
      Include(Result, fffReadOnly);
    if FormFlags and FPDF_FORMFLAG_REQUIRED <> 0 then
      Include(Result, fffRequired);
    if FormFlags and FPDF_FORMFLAG_NOEXPORT <> 0 then
      Include(Result, fffNoExport);

    if FormFlags and FPDF_FORMFLAG_TEXT_MULTILINE <> 0 then
      Include(Result, fffTextMultiLine);
    if FormFlags and FPDF_FORMFLAG_TEXT_PASSWORD <> 0 then
      Include(Result, fffTextPassword);

    if FormFlags and FPDF_FORMFLAG_CHOICE_COMBO <> 0 then
      Include(Result, fffChoiceCombo);
    if FormFlags and FPDF_FORMFLAG_CHOICE_EDIT <> 0 then
      Include(Result, fffChoiceEdit);
    if FormFlags and FPDF_FORMFLAG_CHOICE_MULTI_SELECT <> 0 then
      Include(Result, fffChoiceMultiSelect);
  end;
end;

function TPdfFormField.GetName: string;
var
  Len: Integer;
begin
  Len := FPDFAnnot_GetFormFieldName(FPage.FDocument.FormHandle, Handle, nil, 0) div SizeOf(WideChar) - 1;
  if Len > 0 then
  begin
    SetLength(Result, Len);
    FPDFAnnot_GetFormFieldName(FPage.FDocument.FormHandle, Handle, PWideChar(Result), (Len + 1) * SizeOf(WideChar));
  end
  else
    Result := '';
end;

function TPdfFormField.GetAlternateName: string;
var
  Len: Integer;
begin
  Len := FPDFAnnot_GetFormFieldAlternateName(FPage.FDocument.FormHandle, Handle, nil, 0) div SizeOf(WideChar) - 1;
  if Len > 0 then
  begin
    SetLength(Result, Len);
    FPDFAnnot_GetFormFieldAlternateName(FPage.FDocument.FormHandle, Handle, PWideChar(Result), (Len + 1) * SizeOf(WideChar));
  end
  else
    Result := '';
end;

function TPdfFormField.GetFieldType: TPdfFormFieldType;
begin
  Result := TPdfFormFieldType(FPDFAnnot_GetFormFieldType(FPage.FDocument.FormHandle, Handle));
  if (Result < Low(TPdfFormFieldType)) or (Result > High(TPdfFormFieldType)) then
    Result := fftUnknown;
end;

function TPdfFormField.GetValue: string;
var
  Len: Integer;
begin
  Len := FPDFAnnot_GetFormFieldValue(FPage.FDocument.FormHandle, Handle, nil, 0) div SizeOf(WideChar) - 1;
  if Len > 0 then
  begin
    SetLength(Result, Len);
    FPDFAnnot_GetFormFieldValue(FPage.FDocument.FormHandle, Handle, PWideChar(Result), (Len + 1) * SizeOf(WideChar));
  end
  else
    Result := '';
end;

function TPdfFormField.GetExportValue: string;
var
  Len: Integer;
begin
  Len := FPDFAnnot_GetFormFieldExportValue(FPage.FDocument.FormHandle, Handle, nil, 0) div SizeOf(WideChar) - 1;
  if Len > 0 then
  begin
    SetLength(Result, Len);
    FPDFAnnot_GetFormFieldExportValue(FPage.FDocument.FormHandle, Handle, PWideChar(Result), (Len + 1) * SizeOf(WideChar));
  end
  else
    Result := '';
end;

function TPdfFormField.GetOptionCount: Integer;
begin
  Result := FPDFAnnot_GetOptionCount(FPage.FDocument.FormHandle, Handle);
  if Result < 0 then // annotation types that don't support options will return -1
    Result := 0;
end;

function TPdfFormField.GetOptionLabel(Index: Integer): string;
var
  Len: Integer;
begin
  Len := FPDFAnnot_GetOptionLabel(FPage.FDocument.FormHandle, Handle, Index, nil, 0) div SizeOf(WideChar) - 1;
  if Len > 0 then
  begin
    SetLength(Result, Len);
    FPDFAnnot_GetOptionLabel(FPage.FDocument.FormHandle, Handle, Index, PWideChar(Result), (Len + 1) * SizeOf(WideChar));
  end
  else
    Result := '';
end;

function TPdfFormField.IsOptionSelected(OptionIndex: Integer): Boolean;
begin
  Result := FPDFAnnot_IsOptionSelected(FPage.FDocument.FormHandle, Handle, OptionIndex) <> 0;
end;

function TPdfFormField.GetChecked: Boolean;
begin
  Result := FPDFAnnot_IsChecked(FPage.FDocument.FormHandle, Handle) <> 0;
end;

function TPdfFormField.GetControlCount: Integer;
begin
  Result := FPDFAnnot_GetFormControlCount(FPage.FDocument.FormHandle, Handle);
end;

function TPdfFormField.GetControlIndex: Integer;
begin
  Result := FPDFAnnot_GetFormControlIndex(FPage.FDocument.FormHandle, Handle);
end;

function TPdfFormField.BeginEditFormField: FPDF_ANNOTATION;
var
  AnnotPageIndex: Integer;
begin
  FPage.FDocument.CheckActive;

  // Obtain the currently focused form field/annotation so that we can restore the focus after
  // editing our form field.
  if FORM_GetFocusedAnnot(FPage.FDocument.FormHandle, AnnotPageIndex, Result) = 0 then
    Result := nil;
end;

procedure TPdfFormField.EndEditFormField(LastFocusedAnnot: FPDF_ANNOTATION);
begin
  // Restore the focus to the form field/annotation that had the focus before changing our form field.
  // If no previous form field was focused, kill the focus.
  if LastFocusedAnnot <> nil then
  begin
    if FORM_SetFocusedAnnot(FPage.FDocument.FormHandle, Handle) = 0 then
      FORM_ForceToKillFocus(FPage.FDocument.FormHandle);
    FPDFPage_CloseAnnot(LastFocusedAnnot);
  end
  else
    FORM_ForceToKillFocus(FPage.FDocument.FormHandle);
end;

procedure TPdfFormField.SetValue(const Value: string);
var
  LastFocusedAnnot: FPDF_ANNOTATION;
begin
  FPage.FDocument.CheckActive;

  if not ReadOnly then
  begin
    LastFocusedAnnot := BeginEditFormField();
    try
      if FORM_SetFocusedAnnot(FPage.FDocument.FormHandle, Handle) <> 0 then
      begin
        FORM_SelectAllText(FPage.FDocument.FormHandle, FPage.Handle);
        FORM_ReplaceSelection(FPage.FDocument.FormHandle, FPage.Handle, PWideChar(Value));
      end;
    finally
      EndEditFormField(LastFocusedAnnot);
    end;
  end;
end;

function TPdfFormField.SelectComboBoxOption(OptionIndex: Integer): Boolean;
begin
  Result := SelectListBoxOption(OptionIndex, True);
end;

function TPdfFormField.SelectListBoxOption(OptionIndex: Integer; Selected: Boolean): Boolean;
var
  LastFocusedAnnot: FPDF_ANNOTATION;
begin
  FPage.FDocument.CheckActive;

  Result := False;
  if not ReadOnly then
  begin
    LastFocusedAnnot := BeginEditFormField();
    try
      if FORM_SetFocusedAnnot(FPage.FDocument.FormHandle, Handle) <> 0 then
        Result := FORM_SetIndexSelected(FPage.FDocument.FormHandle, FPage.Handle, OptionIndex, Ord(Selected <> False)) <> 0;
    finally
      EndEditFormField(LastFocusedAnnot);
    end;
  end;
end;

procedure TPdfFormField.SetChecked(const Value: Boolean);
var
  LastFocusedAnnot: FPDF_ANNOTATION;
begin
  FPage.FDocument.CheckActive;

  if not ReadOnly and (FieldType in [fftCheckBox, fftRadioButton, fftXFACheckBox]) then
  begin
    if Value <> Checked then
    begin
      LastFocusedAnnot := BeginEditFormField();
      try
        if FORM_SetFocusedAnnot(FPage.FDocument.FormHandle, Handle) <> 0 then
        begin
          // Toggle the RadioButton/Checkbox by emulating "pressing the space bar".
          FORM_OnKeyDown(FPage.FDocument.FormHandle, FPage.Handle, Ord(' '), 0);
          FORM_OnChar(FPage.FDocument.FormHandle, FPage.Handle, Ord(' '), 0);
          FORM_OnKeyUp(FPage.FDocument.FormHandle, FPage.Handle, Ord(' '), 0);
        end;
      finally
        EndEditFormField(LastFocusedAnnot);
      end;
    end;
  end;
end;

{$IFNDEF VIEW_ONLY}
{ TPdfDocumentPrinter }

constructor TPdfDocumentPrinter.Create;
begin
  inherited Create;
  FFitPageToPrintArea := True;
end;

function TPdfDocumentPrinter.IsPortraitOrientation(AWidth, AHeight: Integer): Boolean;
begin
  Result := AHeight > AWidth;
end;

procedure TPdfDocumentPrinter.GetPrinterBounds;
begin
  FPaperSize.cx := GetDeviceCaps(FPrinterDC, PHYSICALWIDTH);
  FPaperSize.cy := GetDeviceCaps(FPrinterDC, PHYSICALHEIGHT);

  FPrintArea.cx := GetDeviceCaps(FPrinterDC, HORZRES);
  FPrintArea.cy := GetDeviceCaps(FPrinterDC, VERTRES);

  FMargins.X := GetDeviceCaps(FPrinterDC, PHYSICALOFFSETX);
  FMargins.Y := GetDeviceCaps(FPrinterDC, PHYSICALOFFSETY);
end;

function TPdfDocumentPrinter.BeginPrint(const AJobTitle: string): Boolean;
begin
  Inc(FBeginPrintCounter);
  if FBeginPrintCounter = 1 then
  begin
    Result := PrinterStartDoc(AJobTitle);
    if Result then
    begin
      FPrinterDC := GetPrinterDC;

      GetPrinterBounds;
      FPrintPortraitOrientation := IsPortraitOrientation(FPaperSize.cx, FPaperSize.cy);
    end
    else
    begin
      FPrinterDC := 0;
      Dec(FBeginPrintCounter);
    end;
  end
  else
    Result := True;
end;

procedure TPdfDocumentPrinter.EndPrint;
begin
  Dec(FBeginPrintCounter);
  if FBeginPrintCounter = 0 then
  begin
    if FPrinterDC <> 0 then
    begin
      FPrinterDC := 0;
      PrinterEndDoc;
    end;
  end;
end;

function TPdfDocumentPrinter.Print(ADocument: TPdfDocument): Boolean;
begin
  if ADocument <> nil then
    Result := Print(ADocument, 0, ADocument.PageCount - 1)
  else
    Result := False;
end;

function TPdfDocumentPrinter.Print(ADocument: TPdfDocument; AFromPageIndex, AToPageIndex: Integer): Boolean;
var
  PageIndex: Integer;
  WasPageLoaded: Boolean;
  PdfPage: TPdfPage;
  PagePortraitOrientation: Boolean;
  X, Y, W, H: Integer;
  PrintedPageNum, PrintPageCount: Integer;
begin
  Result := False;
  if ADocument = nil then
    Exit;

  if AFromPageIndex < 0 then
    raise EPdfArgumentOutOfRange.CreateResFmt(@RsArgumentsOutOfRange, ['FromPage', AFromPageIndex]);
  if (AToPageIndex < AFromPageIndex) or (AToPageIndex >= ADocument.PageCount) then
    raise EPdfArgumentOutOfRange.CreateResFmt(@RsArgumentsOutOfRange, ['ToPage', AToPageIndex]);

  PrintedPageNum := 0;
  PrintPageCount := AToPageIndex - AFromPageIndex + 1;

  if BeginPrint then
  begin
    try
{$IFNDEF FORM_DISABLED}
      if ADocument.FForm <> nil then
        FORM_DoDocumentAAction(ADocument.FForm, FPDFDOC_AACTION_WP); // BeforePrint
{$ENDIF}
      for PageIndex := AFromPageIndex to AToPageIndex do
      begin
        PdfPage := nil;
        WasPageLoaded := ADocument.IsPageLoaded(PageIndex);
        try
          PdfPage := ADocument.Pages[PageIndex];
          PagePortraitOrientation := IsPortraitOrientation(Trunc(PdfPage.Width), Trunc(PdfPage.Height));

          if FitPageToPrintArea then
          begin
            X := 0;
            Y := 0;
            W := FPrintArea.cx;
            H := FPrintArea.cy;
          end
          else
          begin
            X := -FMargins.X;
            Y := -FMargins.Y;
            W := FPaperSize.cx;
            H := FPaperSize.cy;
          end;

          if PagePortraitOrientation <> FPrintPortraitOrientation then
          begin
            SwapInts(X, Y);
            SwapInts(W, H);
          end;

          // Print page
          PrinterStartPage;
          try
            if (W > 0) and (H > 0) then
              InternPrintPage(PdfPage, X, Y, W, H);
          finally
            PrinterEndPage;
          end;
          Inc(PrintedPageNum);
          if Assigned(OnPrintStatus) then
            OnPrintStatus(Self, PrintedPageNum, PrintPageCount);
        finally
          if not WasPageLoaded and (PdfPage <> nil) then
            PdfPage.Close; // release memory
        end;
{$IFNDEF FORM_DISABLED}
        if ADocument.FForm <> nil then
          FORM_DoDocumentAAction(ADocument.FForm, FPDFDOC_AACTION_DP); // AfterPrint
{$ENDIF}
      end;
    finally
      EndPrint;
    end;
    Result := True;
  end;
end;


procedure TPdfDocumentPrinter.InternPrintPage(APage: TPdfPage; X, Y, Width, Height: Double);

  function RoundToInt(Value: Double): Integer;
  var
    F: Double;
  begin
    Result := Trunc(Value);
    F := Frac(Value);
    if F < 0 then
    begin
      if F <= -0.5 then
        Result := Result - 1;
    end
    else if F >= 0.5 then
      Result := Result + 1;
  end;

var
  PageWidth, PageHeight: Double;
  PageScale, PrintScale: Double;
  ScaledWidth, ScaledHeight: Double;
begin
  PageWidth := APage.Width;
  PageHeight := APage.Height;

  PageScale := PageHeight / PageWidth;
  PrintScale := Height / Width;

  ScaledWidth := Width;
  ScaledHeight := Height;
  if PageScale > PrintScale then
    ScaledWidth := Width * (PrintScale / PageScale)
  else
    ScaledHeight := Height * (PageScale / PrintScale);

  X := X + (Width - ScaledWidth) / 2;
  Y := Y + (Height - ScaledHeight) / 2;

  APage.Draw(
    FPrinterDC,
    RoundToInt(X), RoundToInt(Y), RoundToInt(ScaledWidth), RoundToInt(ScaledHeight),
    prNormal, [proPrinting, proAnnotations]
  );
end;
{$ENDIF}

initialization
  InitializeCriticalSectionAndSpinCount(PDFiumInitCritSect, 4000);
  InitializeCriticalSectionAndSpinCount(FFITimersCritSect, 4000);

finalization
  //{$IFDEF FPC}Windows.{$ENDIF}
  DeleteCriticalSection(FFITimersCritSect);
  //{$IFDEF FPC}Windows.{$ENDIF}
  DeleteCriticalSection(PDFiumInitCritSect);

end.

(*
procedure TPdfPage.ToBMP(bmp: TBitmap);
var  dib: FPDF_BITMAP;  bmpData: Pointer;
     ww, hh, bmpStride, yy: Integer;
begin
  ww := Trunc(Self.FWidth);
  hh := Trunc(Self.Height);
  dib := FPDFBitmap_Create(ww, hh, 1);
  FPDFBitmap_FillRect(dib, 0, 0, ww, hh, $FFFFFFFF);
  FPDF_RenderPageBitmap(dib, Self.FPage, 0, 0, ww, hh, 0, 0);  //, FPDF_REVERSE_BYTE_ORDER);
  bmpData := FPDFBitmap_GetBuffer(dib);
  bmp.SetSize(ww, hh);
  bmpStride := FPDFBitmap_GetStride(dib);
  if bmpStride / ww =3 then
    bmp.PixelFormat := pf24bit
  else
    bmp.PixelFormat := pf32bit;
  for yy := 0 to hh - 1 do
    Move(Pointer(PtrUInt(bmpData) + PtrUInt(yy * bmpStride))^,
              bmp.ScanLine[yy]^, bmpStride);
end;
*)
