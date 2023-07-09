program OCR;



uses
  Forms,
  FMain in 'FMain.pas' {FoMain},
  OCRMain in 'OCRMain.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFoMain, FoMain);
  Application.Run;
end.
