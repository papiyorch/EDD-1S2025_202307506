unit ListaDoble;

{$MODE DELPHI}

interface
    type
        TUserData = record
            idCorreo: string;
            remitente: string;
            estado: string;
            programado: Boolean;
            asunto: string;
            fecha: TDateTime;
            mensaje: string;
        end;

        PNode = ^TNode;
        TNode = record
            idCorreo: string;
            remitente: string;
            estado: string;
            programado: Boolean;
            asunto: string;
            fecha: TDateTime;
            mensaje: string;
            prev: PNode;
            next: PNode;
        end;

    var
        head: PNode = nil;
        tail: PNode = nil;

    procedure insertarDoble(idCorreo, remitente, estado: string; programado: Boolean; asunto: string; fecha: TDateTime; mensaje: string);
    function EscapeDotString(const S: string): string;
    function generarDotLD: string;
    procedure imprimirListaDoble;

implementation
    uses
        SysUtils, Classes;

    procedure insertarDoble(idCorreo, remitente, estado: string; programado: Boolean; asunto: string; fecha: TDateTime; mensaje: string);
    var
        nuevoNodo: PNode;
    begin
        New(nuevoNodo);
        nuevoNodo^.idCorreo := Trim(idCorreo);
        nuevoNodo^.remitente := Trim(remitente);
        nuevoNodo^.estado := Trim(estado);
        nuevoNodo^.programado := programado;
        nuevoNodo^.asunto := Trim(asunto);
        nuevoNodo^.fecha := fecha;
        nuevoNodo^.mensaje := Trim(mensaje);
        nuevoNodo^.prev := nil;
        nuevoNodo^.next := nil;

        if head = nil then
        begin
            head := nuevoNodo;
            tail := nuevoNodo;
        end
        else
        begin
            tail^.next := nuevoNodo;
            nuevoNodo^.prev := tail;
            tail := nuevoNodo;
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

    function generarDotLD: string;
    var
        LD: TStringList;
        actualNodo: PNode;
        Counter: Integer;
        nombreNodo, siguienteNodo: string;
        ResultText: string;
    begin
        LD := TStringList.Create;

        LD.Add('digraph ListaDoble {');
        LD.Add('  rankdir=LR;');
        LD.Add('  nodesep=0.5;');
        LD.Add('');
        LD.Add('  subgraph cluster_0 {');
        LD.Add('    label="Lista Doble";');
        LD.Add('    fontsize=14;');
        LD.Add('    color=black;');
        LD.Add('    style=filled;');
        LD.Add('    fillcolor=white;');
        LD.Add('    node [shape=record, style=filled, fillcolor=lightblue];');
        LD.Add('');

        if head = nil then 
            LD.Add('    null [label="Lista vacía", shape=plaintext];')
        else
        begin
            Counter := 0;
            actualNodo := head;
            while actualNodo <> nil do
            begin
                nombreNodo := Format('nodo%d', [Counter]);
                LD.Add(Format('    %s [label="{%s \n %s \n %s \n %s \n %s}"];',
                    [nombreNodo,
                    EscapeDotString(actualNodo^.idCorreo),
                    EscapeDotString(actualNodo^.remitente),
                    EscapeDotString(actualNodo^.estado),
                    EscapeDotString(BoolToStr(actualNodo^.programado, True)),
                    EscapeDotString(actualNodo^.asunto),
                    DateTimeToStr(actualNodo^.fecha),
                    EscapeDotString(actualNodo^.mensaje)]));
                if actualNodo^.next <> nil then
                begin
                    siguienteNodo := Format('nodo%d', [Counter + 1]);
                    LD.Add(Format('    %s -> %s;', [nombreNodo, siguienteNodo]));
                    LD.Add(Format('    %s -> %s;', [siguienteNodo, nombreNodo]));
                end;

                Inc(Counter);
                actualNodo := actualNodo^.next;

            end;
        end;
        LD.Add('  }');
        LD.Add('}');

        ResultText := LD.Text;
        LD.Free;

        Result := ResultText;
    end;

    procedure imprimirListaDoble;
    var
        actualNodo: PNode;
    begin
        if head = nil then
        begin
            WriteLn('La lista esta vacia');
            Exit;
        end;

        actualNodo := head;
        while actualNodo <> nil do
        begin
            WriteLn('ID: ', actualNodo^.idCorreo);
            WriteLn('Remitente: ', actualNodo^.remitente);
            WriteLn('Estado: ', actualNodo^.estado);
            WriteLn('Programado: ', BoolToStr(actualNodo^.programado, True));
            WriteLn('Asunto: ', actualNodo^.asunto);
            WriteLn('Fecha: ', DateTimeToStr(actualNodo^.fecha));
            WriteLn('Mensaje: ', actualNodo^.mensaje);
            WriteLn('-------------------------');
            actualNodo := actualNodo^.next;
        end;
    end;
end.