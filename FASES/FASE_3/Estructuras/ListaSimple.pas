
unit ListaSimple;

{$MODE DELPHI}

interface 
    uses
        SysUtils, Classes, InterfaceTools, ArbolB, ArbolAVL, ArbolMerkle, fpjson, jsonparser, ListaDoble;
    type

        //Lista para el control de logueo
        PLogueo = ^TLogueo;
        TLogueo = record
            usuario: String;
            horaEntrada: TDateTime;
            horaSalida: TDateTime;
            siguiente: PLogueo;
        end;

        //Lista circular de contactos
        PContacto = ^TContacto;
        TContacto = record
            id: String;
            nombre: String;
            usuario: String;
            email: String;
            telefono: String;
            siguiente: PContacto;
        end;

        //Pila de correos
        PPila = ^TPila;
        TPila = record
            idCorreo: String;
            remitente: String;
            estado: String;
            programado: Boolean;
            asunto: String;
            fechaEnvio: TDateTime;
            mensaje: String;
            siguiente: PPila;
        end;

        //Cola de correos programados
        PProgramados = ^TProgramados;
        TProgramados = record
            idCorreo: String;
            remitente: String;
            estado: String;
            programado: Boolean;
            asunto: String;
            fechaEnvio: TDateTime;
            mensaje: String;
            destinatario: String;
            siguiente: PProgramados;
        end;
        PPProgramados = ^PProgramados; 

        TDatos = record
            id: String;
            nombre: String;
            usuario: String;
            password: String;
            email: String;
            telefono: String;
        end;
        
        PNodo = ^TNodo;
        TNodo = record
            id: String;
            nombre: String;
            usuario: String;
            password: String;
            email: String;
            telefono: String;

            //Estructuras para cada usuario
            contactos: PContacto;
            correos: PCorreo;
            papelera: PPila;
            correosProgramados: PProgramados;
            favoritos: PNodoB;
            siguiente: PNodo;
            borradores: PNodoAVL;
            privados: TMrklePrivados;
        end;

    var
        listaUsuarios: PNodo = nil;
        cabezaLogueo: PLogueo = nil;

    procedure Insertar(id, nombre, usuario, email, telefono, password: String);
    procedure imprimir;
    function generarDotLS: string;
    function validarCredenciales(email, password: string): boolean;
    function buscarPorEmail(const email: string): TDatos;
    function obtenerNodoPorEmail(const email: string): PNodo;
    procedure RegistrarEntrada(const usuario: String);
    procedure RegistrarSalida(const usuario: String);
    procedure generarJsonLogueo(const filename: string);

implementation
    var
        cabeza: PNodo = nil;
    function EscapeDotString(const S: string): string;
    var
        Res: string;
        i: Integer;
    begin
        Res := '';
        for i := 1 to Length(S) do
        begin
            case S[i] of
                '"': Res := Res + '\"';
                '\': Res := Res + '\\';
                '|': Res := Res + '\|';
                '{': Res := Res + '\{';
                '}': Res := Res + '\}';
                #10: Res := Res + '\n';
                #13: Res := Res + '\n';
            else
                Res := Res + S[i];
            end;
        end;
        Result := Res;
    end;

    procedure Insertar(id, nombre, usuario, email, telefono, password : String);
    var
        nuevoNodo: PNodo;
        actualNodo : PNodo;
    begin
        //Verifica si el ID ya existe
        actualNodo := cabeza;
        while actualNodo <> nil do
        begin
            if LowerCase(Trim(actualNodo^.id)) = LowerCase(Trim(id)) then
            begin
                mostrarMensajeError(nil, 'Error al insertar', 'El ID ya existe. No se puede insertar el usuario.');
                Exit;
            end;
            actualNodo := actualNodo^.siguiente;
        end;
        //Si no existe crea al usuario
        New(nuevoNodo);
        nuevoNodo^.id := Trim(id);
        nuevoNodo^.nombre := Trim(nombre);
        nuevoNodo^.usuario := Trim(usuario);
        nuevoNodo^.email := Trim(email);
        nuevoNodo^.telefono := Trim(telefono);
        nuevoNodo^.password := Trim(password);

        //Inicializa las estructuras del usuario
        nuevoNodo^.contactos := nil;
        nuevoNodo^.correos := nil;
        nuevoNodo^.papelera := nil;
        nuevoNodo^.correosProgramados := nil;

        nuevoNodo^.siguiente := nil;

        if cabeza = nil then
            cabeza := nuevoNodo
        else
        begin
            actualNodo := cabeza;
            while actualNodo^.siguiente <> nil do
                actualNodo := actualNodo^.siguiente;
            actualNodo^.siguiente := nuevoNodo;
        end;
        listaUsuarios := cabeza;
    end;

    procedure imprimir;
    var
        actualNodo: PNodo;
    begin
        if cabeza = nil then
        begin
            WriteLn('La lista está vacía.');
            Exit;
        end;

        actualNodo := cabeza;
        while actualNodo <> nil do
        begin
            WriteLn('ID: ', actualNodo^.id);
            WriteLn('Nombre: ', actualNodo^.nombre);
            WriteLn('Usuario: ', actualNodo^.usuario);
            WriteLn('Email: ', actualNodo^.email);
            WriteLn('Telefono: ', actualNodo^.telefono);
            WriteLn('Password: ', actualNodo^.password);
            WriteLn('-------------------');
            actualNodo := actualNodo^.siguiente;
        end;
    end;

    function generarDotLS: string;
    var
        SL: TStringList;
        actualNodo: PNodo;
        Counter: Integer;
        nombreNodo, NextName: string;
        ResultText: string;
    begin
        SL := TStringList.Create;

        SL.Add('digraph ListaEnlazada {');
        SL.Add('  rankdir=LR;');
        SL.Add('  nodesep=0.5;');
        SL.Add('');
        SL.Add('  subgraph cluster_0 {');
        SL.Add('    label="Lista simple enlazada";');
        SL.Add('    fontsize=14;');
        SL.Add('    color=black;');
        SL.Add('    style=filled;');
        SL.Add('    fillcolor=white;');
        SL.Add('    node [shape=record, style=filled, fillcolor=lightblue];');
        SL.Add('');

        if cabeza = nil then
            SL.Add('    null [label="Lista vacía", shape=plaintext];')
        else
        begin
            Counter := 0;
            actualNodo := cabeza;
            while actualNodo <> nil do
            begin
                nombreNodo := Format('nodo%d', [Counter]);
                SL.Add(Format('    %s [label="{%s \n %s \n %s \n %s \n %s \n %s}"];',
                    [nombreNodo,
                    EscapeDotString(actualNodo^.id),
                    EscapeDotString(actualNodo^.nombre),
                    EscapeDotString(actualNodo^.usuario),
                    EscapeDotString(actualNodo^.email),
                    EscapeDotString(actualNodo^.telefono),
                    EscapeDotString(actualNodo^.password)]));

                if actualNodo^.siguiente <> nil then
                begin
                    NextName := Format('nodo%d', [Counter + 1]);
                    SL.Add(Format('    %s -> %s;', [nombreNodo, NextName]));
                end;

                Inc(Counter);
                actualNodo := actualNodo^.siguiente;
            end;
        end;

        SL.Add('  }');
        SL.Add('}');

        ResultText := SL.Text;
        SL.Free;

        Result := ResultText;
    end;

    function validarCredenciales(email, password: string): boolean;
    var
        actualNodo: PNodo;
    begin
        validarCredenciales := False;
        if cabeza = nil then
            Exit;
        
        actualNodo := cabeza;
        while actualNodo <> nil do
        begin
            if (actualNodo^.email = Trim(email)) and (actualNodo^.password = Trim(password)) then
            begin
                validarCredenciales := True;
                RegistrarEntrada(actualNodo^.usuario);
                Break;
            end;
            actualNodo := actualNodo^.siguiente;
        end;
    end;

    function buscarPorEmail(const email: string): TDatos;
    var
        actualNodo: PNodo;
    begin
        buscarPorEmail := Default(TDatos);
        buscarPorEmail.id := '';
        buscarPorEmail.nombre := '';
        buscarPorEmail.usuario := '';
        buscarPorEmail.password := '';
        buscarPorEmail.email := '';
        buscarPorEmail.telefono := '';

        if cabeza = nil then
            Exit;

        actualNodo := cabeza;
        while actualNodo <> nil do
        begin
            if actualNodo^.email = Trim(email) then
            begin
                buscarPorEmail.id := actualNodo^.id;
                buscarPorEmail.nombre := actualNodo^.nombre;
                buscarPorEmail.usuario := actualNodo^.usuario;
                buscarPorEmail.password := actualNodo^.password;
                buscarPorEmail.email := actualNodo^.email;
                buscarPorEmail.telefono := actualNodo^.telefono;
                Exit;
            end;
            actualNodo := actualNodo^.siguiente;
        end;
    end;

    function obtenerNodoPorEmail(const email: string): PNodo;
    var
        actualNodo: PNodo;
    begin
        Result := nil;
        actualNodo := cabeza;
        while actualNodo <> nil do
        begin
            if actualNodo^.email = Trim(email) then
            begin
                Result := actualNodo;
                Exit;
            end;
            actualNodo := actualNodo^.siguiente;
        end;
    end;

    procedure RegistrarEntrada(const usuario: String);
    var
        nuevoLogueo: PLogueo;
        temp: PLogueo;
    begin
        New(nuevoLogueo);
        nuevoLogueo^.usuario := Trim(usuario);
        nuevoLogueo^.horaEntrada := Now;
        nuevoLogueo^.horaSalida := 0; // Indica que aún no ha salido
        nuevoLogueo^.siguiente := nil;

        if cabezaLogueo = nil then
            cabezaLogueo := nuevoLogueo
        else
        begin
            temp := cabezaLogueo;
            while temp^.siguiente <> nil do
                temp := temp^.siguiente;
            temp^.siguiente := nuevoLogueo;
        end;
    end;

    procedure RegistrarSalida(const usuario: String);
    var
        temp: PLogueo;
    begin
        temp := cabezaLogueo;
        while temp <> nil do
        begin
            if (temp^.usuario = Trim(usuario)) and (temp^.horaSalida = 0) then
            begin
                temp^.horaSalida := Now;
                Exit;
            end;
            temp := temp^.siguiente;
        end;
    end;

    procedure generarJsonLogueo(const filename: string);
    var
        jsonArray: TJSONArray;
        temp: PLogueo;
        jsonObject: TJSONObject;
        jsonData: TStringList;
    begin
        jsonArray := TJSONArray.Create;
        temp := cabezaLogueo;
        while temp <> nil do
        begin
            jsonObject := TJSONObject.Create;
            jsonObject.Add('usuario', temp^.usuario);
            jsonObject.Add('horaEntrada', DateTimeToStr(temp^.horaEntrada));
            if temp^.horaSalida <> 0 then
                jsonObject.Add('horaSalida', DateTimeToStr(temp^.horaSalida))
            else
                jsonObject.Add('horaSalida', 'N/A');
            jsonArray.Add(jsonObject);
            temp := temp^.siguiente;
        end;
        jsonData := TStringList.Create;
        jsonData.Text := jsonArray.FormatJSON;
        jsonData.SaveToFile(filename);
        jsonData.Free;
        jsonArray.Free;
    end;
end.
