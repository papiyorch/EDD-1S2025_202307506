unit filesTools;

{$mode objfpc}{$H+}

interface
    uses
        SysUtils;

    procedure generarReportes(const nombreReporte, nombreFolder, DotCode: string);

implementation
    uses
        Process, Classes;

    procedure generarReportes(const nombreReporte, nombreFolder, DotCode: string);
        var
            dotFilePath, imagenFilePath: string;
            folder : Boolean;
            dotFile: Text;
            cmdOutPut: AnsiString;
            SL: TStringList;
            i: Integer;
        begin

            if not DirectoryExists(nombreFolder) then
            begin
                folder := CreateDir(nombreFolder);
                if not folder then
                begin
                    WriteLn('Error al crear el directorio: ', nombreFolder);
                    Exit;
                end;
            end;

            dotFilePath := nombreFolder + PathDelim + nombreReporte + '.dot';
            imagenFilePath := nombreFolder + PathDelim + nombreReporte + '.png';

            //WriteLn('Ruta archivo DOT: ', dotFilePath);
            //WriteLn('Ruta imagen PNG: ', imagenFilePath);
            //WriteLn('Contenido DOT generado:');
            WriteLn(DotCode);

            SL := TStringList.Create;
            SL.Text := DotCode;
            Assign(dotFile, dotFilePath);
            Rewrite(dotFile);

            for i := 0 to SL.Count - 1 do
                WriteLn(dotFile, SL[i]);

            Close(dotFile);
            SL.Free;

            if RunCommand('dot', ['-Tpng', dotFilePath, '-o', imagenFilePath], cmdOutPut) then
                WriteLn('Reporte generado: ', imagenFilePath)
            else
            begin
                WriteLn('Error al generar el reporte: ');
                WriteLn('Detalles: ', cmdOutPut);
            end;
        end;
end.
