unit ListaCircular;

{$MODE Delphi}

interface
    uses
        SysUtils, Classes, InterfaceTools, ListaSimple, ListaDoble, enviarCorreo;

    procedure InsertarCircular(var lista: PContacto; id, nombre, email, usuarioContacto, telefono: string);
    function EscapeDotString(const S: string): string;
    function generarDotLC (lista: PContacto): string;
    function obtenerContactoPorID(lista: PContacto; id: string): TDatos;
    function existeContacto(lista: PContacto; email: string): Boolean;
    function buscarUsuarioPorEmail(listaUsuarios: PNodo; email: String): PNodo;

implementation

    procedure InsertarCircular(var lista: PContacto; id, nombre, email, usuarioContacto, telefono: string);
    var
        nuevoNodo, temp: PContacto;
    begin
        New(nuevoNodo);
        nuevoNodo^.id := Trim(id);
        nuevoNodo^.nombre := Trim(nombre);
        nuevoNodo^.email := Trim(email);
        nuevoNodo^.usuario := Trim(usuarioContacto);  //  Usuario del contacto
        nuevoNodo^.telefono := Trim(telefono);

        nuevoNodo^.siguiente := nil;

        if lista = nil then
        begin
            lista := nuevoNodo;
            nuevoNodo^.siguiente := lista;
        end
        else
        begin
            temp := lista;
            while temp^.siguiente <> lista do
                temp := temp^.siguiente;
            temp^.siguiente := nuevoNodo;
            nuevoNodo^.siguiente := lista;
        end;
    end;

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

    function generarDotLC (lista: PContacto): string;
    var
        SL: TStringList;
        actualNodo: PContacto;
        Counter: Integer;
        nombreNodo, NextName: string;
        ResultText: string;
        firstRun: Boolean;
    begin
        SL := TStringList.Create;

        SL.Add('digraph ListaCircular {');
        SL.Add('  rankdir=LR;');
        SL.Add('  nodesep=0.5;');
        SL.Add('');
        SL.Add('  subgraph cluster_0 {');
        SL.Add('    label="Lista circular";');
        SL.Add('    fontsize=14;');
        SL.Add('    color=black;');
        SL.Add('    style=filled;');
        SL.Add('    fillcolor=white;');
        SL.Add('    node [shape=record, style=filled, fillcolor=lightblue];');
        SL.Add('');

        if lista = nil then
            SL.Add('    null [label="Lista vacía", shape=plaintext];')
        else
        begin
            Counter := 0;
            actualNodo := lista;
            firstRun := True;
            repeat
            nombreNodo := Format('nodo%d', [Counter]);
            SL.Add(Format('    %s [label="{%s \n %s \n %s \n %s \n %s}"];',
                [nombreNodo,
                EscapeDotString(actualNodo^.id),
                EscapeDotString(actualNodo^.nombre),
                EscapeDotString(actualNodo^.usuario),
                EscapeDotString(actualNodo^.email),
                EscapeDotString(actualNodo^.telefono)]));

            NextName := Format('nodo%d', [Counter + 1]);
            if actualNodo^.siguiente <> lista then
                SL.Add(Format('    %s -> %s;', [nombreNodo, NextName]))
            else
                SL.Add(Format('    %s -> nodo0;', [nombreNodo])); // Apunta al primero

            Inc(Counter);
            actualNodo := actualNodo^.siguiente;
            firstRun := False;
        until (actualNodo = lista) and (not firstRun);
        end;

        SL.Add('  }');
        SL.Add('}');

        ResultText := SL.Text;
        SL.Free;

        Result := ResultText;
    end;

    function obtenerContactoPorID(lista: PContacto; id: string): TDatos;
    var
        actualNodo: PContacto;
        contacto: TDatos;
        firstRun: Boolean;
    begin
        contacto := Default(TDatos);

        if lista = nil then
            Exit(contacto);

        actualNodo := lista;
        firstRun := True;
        repeat
            if actualNodo^.id = id then
            begin
                contacto.id := actualNodo^.id;
                contacto.nombre := actualNodo^.nombre;
                contacto.email := actualNodo^.email;
                contacto.usuario := actualNodo^.usuario;
                contacto.telefono := actualNodo^.telefono;
                Break;
            end;
            actualNodo := actualNodo^.siguiente;
            firstRun := False;
        until (actualNodo = lista) and (not firstRun);
        Result := contacto;
    end;

    function buscarUsuarioPorEmail(listaUsuarios: PNodo; email: String): PNodo;
    var
        actual: PNodo;
    begin
        actual := listaUsuarios;
        while (actual <> nil) do
        begin
            if actual^.email = email then
                Exit(actual);
            actual := actual^.siguiente;
        end;
    Result := nil; // no encontrado
end;

    function existeContacto(lista: PContacto; email: string): Boolean;
    var
        temp: PContacto;
        encontrado: Boolean;
        firstRun: Boolean;
    begin
        encontrado := False;
        if lista = nil then
            Exit(False);
        temp := lista;
        firstRun := True;
        repeat
            if temp^.email = email then
            begin
                encontrado := True;
                Break;
            end;
            temp := temp^.siguiente;
        until temp = lista;
        Result := encontrado;
    end;
end.