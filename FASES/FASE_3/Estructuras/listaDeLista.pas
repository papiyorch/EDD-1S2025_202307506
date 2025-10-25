unit listaDeLista;

{$MODE DELPHI}
interface
    uses
        SysUtils, Classes, InterfaceTools, listaSimpleMensajes;

    type

        //Nodo para usuarios
        PUsuarioComunidad = ^TUsuarioComunidad;
        TUsuarioComunidad = record
            email: String;
            siguiente: PUsuarioComunidad;
        end;

        //Lista de usuarios de la comunidad
        TComunidadUsuarioList = record
            cabeza: PUsuarioComunidad;
            contador: Integer;
        end;

        //Nodo de comunidad
        PComunidad = ^TComunidad;
        TComunidad = record
            id: Integer;
            nombre: String;
            usuarios: TComunidadUsuarioList;
            mensajes: PMensaje; // Nueva lista de mensajes para la comunidad
            fechaCreacion: TDateTime; // Nuevo campo para la fecha de creación
            siguiente: PComunidad;
        end;

        //Lista de comunidades
        TComunidadList = record
            cabeza: PComunidad;
            contador: Integer;
        end;

    
    procedure inicializarComunidadUsuarioList(var lista: TComunidadUsuarioList);
    procedure agregarUsuarioAComunidad(var lista: TComunidadUsuarioList; email: String);
    function existeUsuarioEnComunidad(lista: TComunidadUsuarioList; email: String): Boolean;

    procedure inicializarComunidadList(var lista: TComunidadList);
    function agregarComunidad(var lista: TComunidadList; nombre, mensajes: String): Integer;
    function comunidadPorNombre(lista: TComunidadList; nombre: String): PComunidad;
    function comunidadPorID(lista: TComunidadList; id: Integer): PComunidad;
    procedure agregarUsuarioPorNombre(var lista: TComunidadList; nombreComunidad, email: String);

    function EscapeDotString(const S: string): string;
    function generarDotListaDeLista(var listaComunidades: TComunidadList): string;

implementation

    procedure inicializarComunidadList(var lista: TComunidadList);
    begin
        lista.cabeza := nil;
        lista.contador := 0;
    end;

    procedure inicializarComunidadUsuarioList(var lista: TComunidadUsuarioList);
    begin
        lista.cabeza := nil;
        lista.contador := 0;
    end;

    function existeUsuarioEnComunidad(lista: TComunidadUsuarioList; email: String): Boolean;
    var
        actual: PUsuarioComunidad;
    begin
        Result := False;
        actual := lista.cabeza;

        while actual <> nil do
        begin
            if actual^.email = email then
            begin
                Result := True;
                Exit;
            end;
            actual := actual^.siguiente;
        end;
    end;

    procedure agregarUsuarioAComunidad(var lista: TComunidadUsuarioList; email: String);
    var
        nuevoUsuario: PUsuarioComunidad;
        actualUser: PUsuarioComunidad;
    begin
        if existeUsuarioEnComunidad(lista, email) then
        begin
            Exit; 
        end;

        New(nuevoUsuario);
        nuevoUsuario^.email := email;
        nuevoUsuario^.siguiente := nil;

        if lista.cabeza = nil then
        begin
            lista.cabeza := nuevoUsuario;
        end
        else
        begin
            actualUser := lista.cabeza;
            while actualUser^.siguiente <> nil do
            begin
                actualUser := actualUser^.siguiente;
            end;
            actualUser^.siguiente := nuevoUsuario;
        end;

        Inc(lista.contador);
    end;

    function comunidadPorNombre(lista: TComunidadList; nombre: String): PComunidad;
    var
        actual: PComunidad;
    begin
        Result := nil;
        actual := lista.cabeza;

        while actual <> nil do
        begin
            if actual^.nombre = nombre then
            begin
                Result := actual;
                Exit;
            end;
            actual := actual^.siguiente;
        end;
    end;

    function agregarComunidad(var lista: TComunidadList; nombre, mensajes: String): Integer;
    var
        nuevaComunidad: PComunidad;
        actual: PComunidad;
        nextID: Integer;
    begin
        if comunidadPorNombre(lista, nombre) <> nil then
        begin
            Result := -1;
            Exit;
        end;

        nextID := 1;
        actual := lista.cabeza;
        while actual <> nil do
        begin
            if actual^.id >= nextID then
                nextID := actual^.id + 1;
            actual := actual^.siguiente;
        end;

        New(nuevaComunidad);
        nuevaComunidad^.id := nextID;
        nuevaComunidad^.nombre := nombre;
        inicializarComunidadUsuarioList(nuevaComunidad^.usuarios);
        nuevaComunidad^.mensajes := nil;
        nuevaComunidad^.fechaCreacion := Now; // Asignar la fecha actual
        nuevaComunidad^.siguiente := nil;

        if lista.cabeza = nil then
        begin
            lista.cabeza := nuevaComunidad;
        end
        else
        begin
            actual := lista.cabeza;
            while actual^.siguiente <> nil do
            begin
                actual := actual^.siguiente;
            end;
            actual^.siguiente := nuevaComunidad;
        end;
        Inc(lista.contador);
        Result := nextID;
    end;

    function comunidadPorID(lista: TComunidadList; id: Integer): PComunidad;
    var
        actual: PComunidad;
    begin
        Result := nil;
        actual := lista.cabeza;

        while actual <> nil do
        begin
            if actual^.id = id then
            begin
                Result := actual;
                Exit;
            end;
            actual := actual^.siguiente;
            
        end;
    end;

    procedure agregarUsuarioPorNombre(var lista: TComunidadList; nombreComunidad, email: String);
    var
        comunidad: PComunidad;
    begin
        comunidad := comunidadPorNombre(lista, nombreComunidad);
        if comunidad <> nil then
        begin
            agregarUsuarioAComunidad(comunidad^.usuarios, email);
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

    function generarDotListaDeLista(var listaComunidades: TComunidadList): string;
    var
        SL: TStringList;
        actualComunidad: PComunidad;
        actualUsuario: PUsuarioComunidad;
        comunidadCounter, usuarioCounter: Integer;
        nombreComunidadNodo, nombreUsuarioNodo, nextUsuarioNodo: string;
        ResultText: string;
    begin
        SL := TStringList.Create;

        SL.Add('digraph ListaDeLista {');
        SL.Add('  rankdir=LR;');
        SL.Add('  nodesep=0.5;');
        SL.Add('');
        SL.Add('  subgraph cluster_comunidades {');
        SL.Add('    label="Lista de Comunidades";');
        SL.Add('    fontsize=14;');
        SL.Add('    color=black;');
        SL.Add('    style=filled;');
        SL.Add('    fillcolor=white;');
        SL.Add('    node [shape=record, style=filled, fillcolor=lightblue];');
        SL.Add('');

        if listaComunidades.cabeza = nil then
            SL.Add('    null [label="Lista de comunidades vacía", shape=plaintext];')
        else
        begin
            comunidadCounter := 0;
            actualComunidad := listaComunidades.cabeza;
            while actualComunidad <> nil do
            begin
                nombreComunidadNodo := Format('comunidad%d', [comunidadCounter]);
                SL.Add(Format('    %s [label="{%s}"];',
                    [nombreComunidadNodo,
                    EscapeDotString(actualComunidad^.nombre)]));

                // si tiene siguiente comunidad
                if actualComunidad^.siguiente <> nil then
                    SL.Add(Format('    %s -> comunidad%d;', [nombreComunidadNodo, comunidadCounter + 1]));

                // si tiene usuarios
                if actualComunidad^.usuarios.cabeza <> nil then
                begin
                    usuarioCounter := 0;
                    actualUsuario := actualComunidad^.usuarios.cabeza;
                    while actualUsuario <> nil do
                    begin
                        nombreUsuarioNodo := Format('%s_usuario%d', [nombreComunidadNodo, usuarioCounter]);
                        SL.Add(Format('    %s [label="{%s}"];',
                            [nombreUsuarioNodo,
                            EscapeDotString(actualUsuario^.email)]));

                        // enlazar usuarios en la lista
                        if actualUsuario^.siguiente <> nil then
                        begin
                            nextUsuarioNodo := Format('%s_usuario%d', [nombreComunidadNodo, usuarioCounter + 1]);
                            SL.Add(Format('    %s -> %s;', [nombreUsuarioNodo, nextUsuarioNodo]));
                        end;

                        // enlazar comunidad con primer usuario
                        if usuarioCounter = 0 then
                            SL.Add(Format('    %s -> %s;', [nombreComunidadNodo, nombreUsuarioNodo]));

                        Inc(usuarioCounter);
                        actualUsuario := actualUsuario^.siguiente;
                    end;
                end;

                Inc(comunidadCounter);
                actualComunidad := actualComunidad^.siguiente;
            end;
        end;

        SL.Add('  }');
        SL.Add('}');

        ResultText := SL.Text;
        SL.Free;

        Result := ResultText;
    end;

end.   