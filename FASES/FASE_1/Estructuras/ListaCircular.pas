unit ListaCircular;

{$MODE Delphi}

interface
    type
        TUserData = record
            id: string;
            nombre: string;
            email: string;
            usuario: string;
            telefono: string;
        end;

        PNode = ^TNode;
        TNode = record
            id: string;
            nombre: string;
            email: string;
            usuario: string;
            telefono: string;
            next: PNode;
            propietario: string;
        end;

    var
        head: PNode;
        destinatario: String;
        asunto: String;
        mensaje: String;

    procedure InsertarCircular(id, nombre, email, usuarioContacto, telefono, propietario: string);
    function EscapeDotString(const S: string): string;
    function generarDotLC: string;
    function obtenerContactoPorID(id: string): TUserData;
    function existeContacto(email: string): Boolean;

implementation

    uses
        SysUtils, Classes, InterfaceTools, ListaDoble, enviarCorreo;

    procedure InsertarCircular(id, nombre, email, usuarioContacto, telefono, propietario: string);
    var
        nuevoNodo, temp: PNode;
    begin
        New(nuevoNodo);
        nuevoNodo^.id := Trim(id);
        nuevoNodo^.nombre := Trim(nombre);
        nuevoNodo^.email := Trim(email);
        nuevoNodo^.usuario := Trim(usuarioContacto);  // ← Usuario del contacto
        nuevoNodo^.telefono := Trim(telefono);
        nuevoNodo^.propietario := Trim(propietario);  // ← Usuario logueado que guarda el contacto

        if head = nil then
        begin
            head := nuevoNodo;
            nuevoNodo^.next := head;
        end
        else
        begin
            temp := head;
            while temp^.next <> head do
                temp := temp^.next;
            temp^.next := nuevoNodo;
            nuevoNodo^.next := head;
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

    function generarDotLC: string;
    var
        SL: TStringList;
        actualNodo: PNode;
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

        if head = nil then
            SL.Add('    null [label="Lista vacía", shape=plaintext];')
        else
        begin
            Counter := 0;
            actualNodo := head;
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
            if actualNodo^.next <> head then
                SL.Add(Format('    %s -> %s;', [nombreNodo, NextName]))
            else
                SL.Add(Format('    %s -> nodo0;', [nombreNodo])); // Apunta al primero

            Inc(Counter);
            actualNodo := actualNodo^.next;
            firstRun := False;
        until (actualNodo = head) and (not firstRun);
        end;

        SL.Add('  }');
        SL.Add('}');

        ResultText := SL.Text;
        SL.Free;

        Result := ResultText;
    end;

    function obtenerContactoPorID(id: string): TUserData;
    var
        actualNodo: PNode;
        contacto: TUserData;
    begin
        contacto.id := '';
        contacto.nombre := '';
        contacto.email := '';
        contacto.usuario := '';
        contacto.telefono := '';

        actualNodo := head;

        while actualNodo <> nil do
        begin

            if actualNodo^.id = id then
            begin
                contacto.id := actualNodo^.id;
                contacto.nombre := actualNodo^.nombre;
                contacto.email := actualNodo^.email;
                contacto.usuario := actualNodo^.usuario;
                contacto.telefono := actualNodo^.telefono;
                Break;
            end;
            actualNodo := actualNodo^.next;
        end;

        Result := contacto;
    end;

    function existeContacto(email: string): Boolean;
    var
        temp: PNode;
        encontrado: Boolean;
    begin
        encontrado := False;
        if head = nil then
            Exit(False);
        temp := head;
        repeat
            if temp^.email = email then
            begin
                encontrado := True;
                Break;
            end;
            temp := temp^.next;
        until temp = head;
        Result := encontrado;
    end;
end.