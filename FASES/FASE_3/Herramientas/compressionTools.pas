unit compressionTools;

{$MODE DELPHI}

interface
    function LZWCompress(const input: string): string;
    procedure GuardarArchivo(const fileName, content: string);

implementation
    uses 
        SysUtils, Generics.Collections, Classes;

    function LZWCompress(const input: string): string;
    var
        dict: TDictionary<string, Integer>;
        resultList: TList<Integer>;
        current, next: string;
        i, code: Integer;
        Output: string;
    begin
        dict := TDictionary<string, Integer>.Create;
        resultList := TList<Integer>.Create;
        try
            code := 256;
            for i := 0 to 255 do
                dict.Add(Char(i), i);

            current := '';
            for i := 1 to Length(input) do
            begin
                next := current + input[i];
                if dict.ContainsKey(next) then
                    current := next
                else
                begin
                    resultList.Add(dict[current]);
                    dict.Add(next, code);
                    Inc(code);
                    current := input[i];
                end;
            end;
        
            if current <> '' then
                resultList.Add(dict[current]);

            Output := '';
            for i := 0 to resultList.Count - 1 do
                Output := Output + IntToStr(resultList[i]) + ' ';

            Result := Output;
        finally
            dict.Free;
            resultList.Free;
        end;
    end;

    procedure GuardarArchivo(const fileName, content: string);
    var
        archivo: TextFile;
    begin
        AssignFile(archivo, fileName);
        Rewrite(archivo);
        try
            Write(archivo, content);
        finally
            CloseFile(archivo);
        end;
    end;
end.