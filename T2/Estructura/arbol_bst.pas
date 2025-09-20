{$mode objfpc}
program ArbolBST;
uses
    SysUtils, fpjson, jsonparser, Classes, Process;

type
    PNode = ^TNode;
    TNode = record
        data: Integer;
        left, right: PNode;
    end;

var
    root: PNode;


function crearNodo(valor: Integer): PNode;
begin
    New(Result);
    Result^.data := valor;
    Result^.left := nil;
    Result^.right := nil;
end;

procedure insertar(var node: PNode; valor: Integer);
begin
    if node = nil then
    begin
        node := crearNodo(valor);
        Exit;
    end;
    
    if valor < node^.data then
        insertar(node^.left, valor)
    else if valor > node^.data then
        insertar(node^.right, valor);
end;

procedure cargarDesdeJSON(const filename: string);
var
    jsonData: TJSONData;
    jsonArray: TJSONArray;
    i: Integer;
    jsonStr : TStringList;
    obj: TJSONObject;
begin
    jsonStr := TStringList.Create;
    try
        jsonStr.LoadFromFile(filename);
        jsonData := GetJSON(jsonStr.Text);
        if jsonData.JSONType = jtArray then
            begin
                jsonArray := TJSONArray(jsonData);
                for i := 0 to jsonArray.Count - 1 do
                begin
                    obj := TJSONObject(jsonArray.Items[i]);
                    insertar(root, obj.Get('id'));
                end;
            end
        else
            WriteLn('El archivo JSON no contiene un arreglo.');
    finally
        jsonStr.Free;
    end;
end;

procedure RecorrerDOT(node: PNode; var fileHandle: Text);
begin
    if node = nil then Exit;

    if node^.left <> nil then
    begin
        WriteLn(fileHandle, '    ', node^.data, ' -> ', node^.left^.data, ';');
        RecorrerDOT(node^.left, fileHandle);
    end;

    if node^.right <> nil then
    begin
        WriteLn(fileHandle, '    ', node^.data, ' -> ', node^.right^.data, ';');
        RecorrerDOT(node^.right, fileHandle);
    end;
end;

procedure generarDOT(const filename: string);
var
    files: Text;
begin
    Assign(files, filename);
    Rewrite(files);

    WriteLn(files, 'digraph BST {');
    WriteLn(files, '    node [shape=circle];');
    RecorrerDOT(root, files);
    WriteLn(files, '}');
    Close(files);
end;

procedure generarImagenDOT(const dotFile, outputFile: string);
var
    cmd: string;
    exitCode: Boolean;
    output: AnsiString;
begin
    cmd := Format('dot -Tpng "%s" -o "%s"', [dotFile, outputFile]);
    {$IFDEF WINDOWS}
    exitCode := RunCommand('cmd', ['/C', cmd], output);
    {$ELSE}
    exitCode := RunCommand('/bin/sh', ['-c', cmd], output);
    {$ENDIF}

    if exitCode then
        WriteLn('Imagen generada correctamente: ', outputFile)
    else
    begin
        WriteLn('Error al generar la imagen.');
        WriteLn('Salida: ', output);
    end;
end;

// Programa principal
begin
    root := nil;
    cargarDesdeJSON('Inputs/datos.json');
    generarDOT('arbol_bst.dot');
    generarImagenDOT('arbol_bst.dot', 'arbol_bst.png');

end.