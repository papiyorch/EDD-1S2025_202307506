unit listaCircular;

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

    procedure InsertarCircular(id, nombre, email, usuario, telefono: string);
    function generarDotLC: String;

implementation

    uses
        SysUtils, Classes;

    type    
        PNode = ^TNode;
        TNode = record
            id: string;
            nombre: string;
            email: string;
            usuario: string;
            telefono: string;
            next: PNode;
        end;
    
    var
        head: PNode = nil;

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

    procedure InsertarCircular(id, nombre, email, usuario, telefono: string);
    var
        nuevoNodo, temp: PNode;
    begin
        New(nuevoNodo);
        nuevoNodo^.id := Trim(id);
        nuevoNodo^.nombre := Trim(nombre);
        nuevoNodo^.email := Trim(email);
        nuevoNodo^.usuario := Trim(usuario);
        nuevoNodo^.telefono := Trim(telefono);

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
end.