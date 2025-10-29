unit ListaDoble;

{$MODE DELPHI}

interface
    uses
        SysUtils, Classes, InterfaceTools;

    type
        //Lista doble de correos
        PCorreo = ^TCorreo;
        TCorreo = record
            idCorreo: String;
            remitente: String;
            estado: String;
            programado: Boolean;
            asunto: String;
            fecha: TDateTime;
            mensaje: String;
            siguiente, anterior: PCorreo;
        end;

    procedure insertarDoble(var lista: PCorreo; idCorreo, remitente, estado: string; programado: Boolean; asunto: string; fecha: TDateTime; mensaje: string);
    function EscapeDotString(const S: string): string;
    function generarDotLD(lista: PCorreo): string;
    procedure imprimirListaDoble(lista: PCorreo);

implementation

    procedure insertarDoble(var lista: PCorreo; idCorreo, remitente, estado: string; programado: Boolean; asunto: string; fecha: TDateTime; mensaje: string);
    var
        nuevoNodo, temp: PCorreo;
    begin
        New(nuevoNodo);
        nuevoNodo^.idCorreo := Trim(idCorreo);
        nuevoNodo^.remitente := Trim(remitente);
        nuevoNodo^.estado := Trim(estado);
        nuevoNodo^.programado := programado;
        nuevoNodo^.asunto := Trim(asunto);
        nuevoNodo^.fecha := fecha;
        nuevoNodo^.mensaje := Trim(mensaje);
        nuevoNodo^.siguiente := nil;
        nuevoNodo^.anterior := nil;

        if lista = nil then
        begin
            lista := nuevoNodo;
        end
        else
        begin
            temp := lista;
            while temp^.siguiente <> nil do
                temp := temp^.siguiente;
            temp^.siguiente := nuevoNodo;
            nuevoNodo^.anterior := temp;
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

    function generarDotLD(lista: PCorreo): string;
    var
        LD: TStringList;
        actualNodo: PCorreo;
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

        if lista = nil then 
            LD.Add('    null [label="Lista vacía", shape=plaintext];')
        else
        begin
            Counter := 0;
            actualNodo := lista;
            while actualNodo <> nil do
            begin
                nombreNodo := Format('nodo%d', [Counter]);
                LD.Add(Format('    %s [label="{%s \n %s \n %s \n %s \n %s \n %s \n %s}"];',
                    [nombreNodo,
                    EscapeDotString(actualNodo^.idCorreo),
                    EscapeDotString(actualNodo^.remitente),
                    EscapeDotString(actualNodo^.estado),
                    EscapeDotString(BoolToStr(actualNodo^.programado, True)),
                    EscapeDotString(actualNodo^.asunto),
                    DateTimeToStr(actualNodo^.fecha),
                    EscapeDotString(actualNodo^.mensaje)]));
                    
                if actualNodo^.siguiente <> nil then
                begin
                    siguienteNodo := Format('nodo%d', [Counter + 1]);
                    LD.Add(Format('    %s -> %s;', [nombreNodo, siguienteNodo]));
                    LD.Add(Format('    %s -> %s;', [siguienteNodo, nombreNodo]));
                end;

                Inc(Counter);
                actualNodo := actualNodo^.siguiente;

            end;
        end;
        LD.Add('  }');
        LD.Add('}');

        ResultText := LD.Text;
        LD.Free;

        Result := ResultText;
    end;

    procedure imprimirListaDoble(lista: PCorreo);
    var
        actualNodo: PCorreo;
    begin
        if lista = nil then
        begin
            WriteLn('La lista esta vacia');
            Exit;
        end;

        actualNodo := lista;
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
            actualNodo := actualNodo^.siguiente;
        end;
    end;
end.