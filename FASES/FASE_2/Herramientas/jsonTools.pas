unit jsonTools;
{$mode objfpc}{$H+}


interface
    uses 
        ListaSimple;

    function cargaUsuariosJson(const filePath: String): Boolean;
    function agregarUsuarioJson(const filePath: String; const id: Integer; const nombre, usuario, email, telefono, password: String): Boolean;

implementation
    uses
        SysUtils, fpjson, jsonparser, Classes;

    function cargaUsuariosJson(const filePath: String): Boolean;
    var
        jsonData: TJSONData;
        jsonObject: TJSONObject;
        usersArray: TJSONArray;
        userItem: TJSONObject;
        i: Integer;
        fileStream: TFileStream;
    begin
        cargaUsuariosJson := False;

        if not FileExists(filePath) then
        begin
            WriteLn('El archivo no existe', filePath);
            Exit;
        end;

        try
            fileStream := TFileStream.Create(filePath, fmOpenRead);
            try
                jsonData := GetJSON(fileStream);
            finally
                fileStream.Free;
            end;

            jsonObject := TJSONObject(jsonData);
            usersArray := jsonObject.Arrays['usuarios'];

            for i := 0 to usersArray.Count - 1 do
            begin
                userItem := usersArray.Objects[i];

                Insertar(
                    IntToStr(userItem.Integers['id']),
                    userItem.Strings['nombre'],
                    userItem.Strings['usuario'],
                    userItem.Strings['email'],
                    userItem.Strings['telefono'],
                    userItem.Strings['password']
                );
            end;

            jsonData.Free;
            cargaUsuariosJson := True;
        except
            on E: Exception do
            begin
                WriteLn('Error al cargar usuarios desde JSON: ', E.Message);
                cargaUsuariosJson := False;
            end;
        end;
    end;

    function agregarUsuarioJson(const filePath: String; const id: Integer; const nombre, usuario, email, telefono, password: String): Boolean;
    var
        jsonData: TJSONData = nil;
        jsonObject: TJSONObject = nil;
        usersArray: TJSONArray = nil;
        usuarioNuevo: TJSONObject;
        fileStream: TFileStream;
        jsonString: TStringList;
    begin
        agregarUsuarioJson := False;

        if FileExists(filePath) then
        begin
            try 
                fileStream := TFileStream.Create(filePath, fmOpenRead);
                try
                    jsonData := GetJSON(fileStream);
                finally
                    fileStream.Free;
                end;

                jsonObject := TJSONObject(jsonData);
                usersArray := jsonObject.Arrays['usuarios'];
            except
                on E: Exception do
                begin
                    WriteLn('Error al leer el archivo JSON: ', E.Message);
                    Exit;
                end;
            end;
        end
        else
        begin
            jsonObject := TJSONObject.Create;
            usersArray := TJSONArray.Create;
            jsonObject.Add('usuarios', usersArray);
        end;

        usuarioNuevo := TJSONObject.Create;
        usuarioNuevo.Add('id', id);
        usuarioNuevo.Add('nombre', nombre);
        usuarioNuevo.Add('usuario', usuario);
        usuarioNuevo.Add('email', email);
        usuarioNuevo.Add('telefono', telefono);
        usuarioNuevo.Add('password', password);

        usersArray.Add(usuarioNuevo);

        jsonString := TStringList.Create;
        try
            jsonString.Text := jsonObject.FormatJSON();
            jsonString.SaveToFile(filePath);

            agregarUsuarioJson := True;

        finally
            jsonString.Free;

            if Assigned(jsonData) then
                jsonData.Free
            else
                jsonObject.Free;
        end;
    end;
end.    