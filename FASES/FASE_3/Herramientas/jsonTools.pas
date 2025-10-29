unit jsonTools;
{$mode objfpc}{$H+}


interface
    uses 
        ListaSimple, ListaDoble, ListaCircular, blockchainCorreos;

    var
        contadorCorreos : Integer = 0;
        contadorContactos : Integer = 0;

    function cargaUsuariosJson(const filePath: String): Boolean;
    function agregarUsuarioJson(const filePath: String; const id: Integer; const nombre, usuario, email, telefono, password: String): Boolean;
    function cargarCorreosJson(const filePath: String): Boolean;
    function agregarCorreoJson(const filePath: String; id: Integer; remitente, destinatario, estado, asunto, mensaje: String; programado: Boolean; fecha: TDateTime): Boolean;
    function cargarContactosJson(const filePath: String): Boolean;
    function agregarContactoJson(const filePath: String; const propietario, contacto: String): Boolean;

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

            //Inicializar Blockchain si no existe
            if BlockchainCorreosGlobal = nil then
                BlockchainCorreosGlobal := TBlockchain.Create;
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

    function cargarCorreosJson(const filePath: String): Boolean;
    var
        jsonData: TJSONData;
        jsonObject: TJSONObject;
        correosArray: TJSONArray;
        correoItem: TJSONObject;
        i: Integer;
        fileStream: TFileStream;
        usuarioDestino: PNodo;
        idCorreo: Integer;
    begin
        cargarCorreosJson := False;

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
            correosArray := jsonObject.Arrays['correos'];

            for i := 0 to correosArray.Count - 1 do
            begin
                correoItem := correosArray.Objects[i];

                idCorreo := correoItem.Integers['id'];

                usuarioDestino := buscarUsuarioPorEmail(listaUsuarios, correoItem.Strings['destinatario']);
                if usuarioDestino <> nil then
                begin
                    insertarDoble(usuarioDestino^.correos,
                                IntToStr(idCorreo),
                                correoItem.Strings['remitente'],
                                correoItem.Strings['estado'],
                                correoItem.Booleans['programado'],
                                correoItem.Strings['asunto'],
                                Now, // Usar la fecha actual o parsear si se agrega al JSON
                                correoItem.Strings['mensaje']);
                    
                    //Blockchain
                    if usuarioDestino^.correos <> nil then
                        BlockchainCorreosGlobal.AddBlock(usuarioDestino^.correos);

                    if idCorreo > contadorCorreos then
                        contadorCorreos := idCorreo;
                end
                else
                    WriteLn('Destinatario no encontrado: ', correoItem.Strings['destinatario']);
            end;

            jsonData.Free;
            cargarCorreosJson := True;
        except
            on E: Exception do
            begin
                WriteLn('Error al cargar correos desde JSON: ', E.Message);
                cargarCorreosJson := False;
            end;
        end;

    end;

    function agregarCorreoJson(const filePath: String; id: Integer; remitente, destinatario, estado, asunto, mensaje: String; programado: Boolean; fecha: TDateTime): Boolean;
    var
        jsonData: TJSONData = nil;
        jsonObject: TJSONObject = nil;
        correosArray: TJSONArray = nil;
        correoNuevo: TJSONObject;
        fileStream: TFileStream;
        jsonString: TStringList;
        usuarioDestino: PNodo;
    begin
        agregarCorreoJson := False;

        // Leer o crear archivo
        if FileExists(filePath) then
        begin
            jsonData := GetJSON(filePath);
            jsonObject := TJSONObject(jsonData);
            correosArray := jsonObject.Arrays['correos'];
        end
        else
        begin
            jsonObject := TJSONObject.Create;
            correosArray := TJSONArray.Create;
            jsonObject.Add('correos', correosArray);
        end;

        // Crear objeto JSON
        correoNuevo := TJSONObject.Create;
        correoNuevo.Add('id', id);
        correoNuevo.Add('remitente', remitente);
        correoNuevo.Add('destinatario', destinatario);
        correoNuevo.Add('estado', estado);
        correoNuevo.Add('asunto', asunto);
        correoNuevo.Add('mensaje', mensaje);

        correosArray.Add(correoNuevo);

        // Guardar JSON
        jsonString := TStringList.Create;
        try
            jsonString.Text := jsonObject.FormatJSON();
            jsonString.SaveToFile(filePath);
        finally
            jsonString.Free;
            if Assigned(jsonData) then jsonData.Free else jsonObject.Free;
        end;

        // Insertar en lista doble del destinatario
        usuarioDestino := buscarUsuarioPorEmail(listaUsuarios, destinatario);
        if usuarioDestino <> nil then
        begin
            insertarDoble(usuarioDestino^.correos,
                        IntToStr(id),
                        remitente,
                        estado,
                        programado,
                        asunto,
                        fecha,
                        mensaje);

            if id > contadorCorreos then
                contadorCorreos := id;

            agregarCorreoJson := True;
        end
        else
            WriteLn('Destinatario no encontrado: ', destinatario);
    end;

    function buscarUsuarioPorUsuario(listaUsuarios: PNodo; usuario: String): PNodo;
    var
        actual: PNodo;
    begin
        actual := listaUsuarios;
        while (actual <> nil) do
        begin
            if actual^.usuario = usuario then
            Exit(actual);
            actual := actual^.siguiente;
        end;
        Result := nil;
    end;

    function cargarContactosJson(const filePath: String): Boolean;
    var
        jsonData: TJSONData;
        jsonObject: TJSONObject;
        contactosArray: TJSONArray;
        contactoItem: TJSONObject;
        contactosList: TJSONArray;
        i, j: Integer;
        fileStream: TFileStream;
        usuarioPropietario, usuarioContacto: PNodo;
        nombreContacto: String;
    begin
        cargarContactosJson := False;

        if not FileExists(filePath) then
        begin
            WriteLn('El archivo no existe: ', filePath);
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
            contactosArray := jsonObject.Arrays['Contactos'];

            for i := 0 to contactosArray.Count - 1 do
            begin
            contactoItem := contactosArray.Objects[i];
            usuarioPropietario := buscarUsuarioPorUsuario(listaUsuarios, contactoItem.Strings['usuario']);

            if usuarioPropietario = nil then
            begin
                WriteLn('Usuario propietario no encontrado: ', contactoItem.Strings['usuario']);
                Continue;
            end;

            contactosList := contactoItem.Arrays['contactos'];

            for j := 0 to contactosList.Count - 1 do
            begin
                nombreContacto := contactosList.Strings[j];
                usuarioContacto := buscarUsuarioPorUsuario(listaUsuarios, nombreContacto);

                if usuarioContacto = nil then
                begin
                WriteLn('Usuario contacto no encontrado: ', nombreContacto);
                Continue;
                end;

                if not existeContacto(usuarioPropietario^.contactos, usuarioContacto^.usuario) then
                begin
                InsertarCircular(
                    usuarioPropietario^.contactos,
                    IntToStr(contadorContactos + 1),
                    usuarioContacto^.nombre,
                    usuarioContacto^.usuario,
                    usuarioContacto^.email,
                    usuarioContacto^.telefono
                );
                Inc(contadorContactos);
                end;
            end;
            end;

            jsonData.Free;
            cargarContactosJson := True;
        except
            on E: Exception do
            WriteLn('Error al cargar contactos desde JSON: ', E.Message);
        end;
    end;

    function agregarContactoJson(const filePath: String; const propietario, contacto: String): Boolean;
    var
        jsonData: TJSONData = nil;
        jsonObject: TJSONObject = nil;
        contactosArray: TJSONArray = nil;
        propietarioItem: TJSONObject = nil;
        contactosList: TJSONArray = nil;
        usuarioPropietario, usuarioContacto: PNodo;
        i: Integer;
        jsonString: TStringList;
    begin
        Result := False;

        usuarioPropietario := buscarUsuarioPorUsuario(listaUsuarios, propietario);
        usuarioContacto := buscarUsuarioPorUsuario(listaUsuarios, contacto);

        if (usuarioPropietario = nil) or (usuarioContacto = nil) then
        begin
            WriteLn('Error: Propietario o contacto no existe en la lista de usuarios.');
            Exit;
        end;

        if FileExists(filePath) then
        begin
            jsonData := GetJSON(filePath);
            jsonObject := TJSONObject(jsonData);
            contactosArray := jsonObject.Arrays['Contactos'];
        end
        else
        begin
            jsonObject := TJSONObject.Create;
            contactosArray := TJSONArray.Create;
            jsonObject.Add('Contactos', contactosArray);
        end;

        propietarioItem := nil;
        for i := 0 to contactosArray.Count - 1 do
        begin
            if contactosArray.Objects[i].Strings['usuario'] = propietario then
            begin
            propietarioItem := contactosArray.Objects[i];
            Break;
            end;
        end;

        if propietarioItem = nil then
        begin
            propietarioItem := TJSONObject.Create;
            propietarioItem.Add('usuario', propietario);
            contactosList := TJSONArray.Create;
            propietarioItem.Add('contactos', contactosList);
            contactosArray.Add(propietarioItem);
        end
        else
            contactosList := propietarioItem.Arrays['contactos'];

        for i := 0 to contactosList.Count - 1 do
            if contactosList.Strings[i] = contacto then
            Exit;

        contactosList.Add(contacto);

        jsonString := TStringList.Create;
        try
            jsonString.Text := jsonObject.FormatJSON();
            jsonString.SaveToFile(filePath);
            Result := True;
        finally
            jsonString.Free;
            if Assigned(jsonData) then
            jsonData.Free
            else
            jsonObject.Free;
        end;

        if not existeContacto(usuarioPropietario^.contactos, usuarioContacto^.usuario) then
        begin
            InsertarCircular(
            usuarioPropietario^.contactos,
            IntToStr(contadorContactos + 1),
            usuarioContacto^.nombre,
            usuarioContacto^.usuario,
            usuarioContacto^.email,
            usuarioContacto^.telefono
            );
            Inc(contadorContactos);
        end;
    end;

end.