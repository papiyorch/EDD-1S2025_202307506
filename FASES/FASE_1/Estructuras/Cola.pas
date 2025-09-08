unit Cola;

{$MODE DELPHI}

interface
    uses 
        SysUtils, Classes, InterfaceTools, ListaSimple;

    procedure insertarCola(var cola: PProgramados; idCorreo, remitente, estado: String; programado: Boolean; asunto: String; fechaEnvio: TDateTime; mensaje, destinatario: String);
    procedure eliminarCola(var cola: PProgramados);
    function EscapeDotString(const S: string): string;
    function generarDotCola (cola: PProgramados): string;
    procedure imprimirCola(cola: PProgramados);

implementation

    procedure insertarCola(var cola: PProgramados; idCorreo, remitente, estado: String; programado: Boolean; asunto: String; fechaEnvio: TDateTime; mensaje, destinatario: String);
    var
        nuevoNodo, temp: PProgramados;
    begin
        New(nuevoNodo);
        nuevoNodo^.idCorreo := Trim(idCorreo);
        nuevoNodo^.remitente := Trim(remitente);
        nuevoNodo^.estado := Trim(estado);
        nuevoNodo^.programado := programado;
        nuevoNodo^.asunto := Trim(asunto);
        nuevoNodo^.fechaEnvio := fechaEnvio;
        nuevoNodo^.mensaje := Trim(mensaje);
        nuevoNodo^.destinatario := Trim(destinatario);
        nuevoNodo^.siguiente := nil;

        if cola = nil then
            cola := nuevoNodo
        else
        begin
            temp := cola;
            while temp^.siguiente <> nil do
                temp := temp^.siguiente;
            temp^.siguiente := nuevoNodo;
        end;
    end;

    procedure eliminarCola(var cola: PProgramados);
    var
        temp: PProgramados;
    begin
        if cola <> nil then
        begin
            temp := cola;
            cola := cola^.siguiente;
            Dispose(temp);
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

    function generarDotCola (cola: PProgramados): string;
    var
        SL: TStringList;
        actualNodo: PProgramados;
        Counter: Integer;
        nombreNodo, siguienteNodo: string;
        ResultText: string;
        firstRun: Boolean;
    begin
        SL := TStringList.Create;

        SL.Add('digraph Cola {');
        SL.Add('  rankdir=TB;');
        SL.Add('  nodesep=0.5;');
        SL.Add('');
        SL.Add('  subgraph cluster_0 {');
        SL.Add('    label="Cola";');
        SL.Add('    fontsize=14;');
        SL.Add('    color=black;');
        SL.Add('    style=filled;');
        SL.Add('    fillcolor=white;');
        SL.Add('    node [shape=record, style=filled, fillcolor=lightblue];');
        SL.Add('');
        if cola = nil then
            SL.Add('    null [label="Cola vacía", shape=plaintext];')
        else
        begin
            actualNodo := cola;
            Counter := 0;
            firstRun := True;
            repeat
            nombreNodo := Format('nodo%d', [Counter]);
            SL.Add(Format(' %s [label="{%s \n %s \n %s \n %s \n %s \n %s \n %s}"];',
                [nombreNodo,
                EscapeDotString(actualNodo^.idCorreo),
                EscapeDotString(actualNodo^.remitente),
                EscapeDotString(actualNodo^.estado),
                EscapeDotString(BoolToStr(actualNodo^.programado, True)),
                EscapeDotString(actualNodo^.asunto),
                EscapeDotString(DateTimeToStr(actualNodo^.fechaEnvio)),
                EscapeDotString(actualNodo^.mensaje)]));
            siguienteNodo := Format('nodo%d', [Counter + 1]);
            if actualNodo^.siguiente <> nil then
            begin
                SL.Add(Format('    %s -> %s;', [nombreNodo, siguienteNodo]));
            end;

            Inc(Counter);
            actualNodo := actualNodo^.siguiente;
            firstRun := False;
        until (actualNodo = nil) and (not firstRun);
        end;

        SL.Add('  }');
        SL.Add('}');

        ResultText := SL.Text;
        SL.Free;

        Result := ResultText;
    end;

    procedure imprimirCola(cola: PProgramados);
    var
        actualNodo: PProgramados;
    begin
        if cola = nil then
        begin
            Writeln('Cola vacía');
            Exit;
        end;

        actualNodo := cola;
        while actualNodo <> nil do
        begin
            Writeln('ID Correo: ', actualNodo^.idCorreo);
            Writeln('Remitente: ', actualNodo^.remitente);
            Writeln('Estado: ', actualNodo^.estado);
            Writeln('Programado: ', BoolToStr(actualNodo^.programado, True));
            Writeln('Asunto: ', actualNodo^.asunto);
            Writeln('Fecha de Envio: ', DateTimeToStr(actualNodo^.fechaEnvio));
            Writeln('Mensaje: ', actualNodo^.mensaje);
            Writeln('---------------------------');
            actualNodo := actualNodo^.siguiente;
        end;
    end;

end.