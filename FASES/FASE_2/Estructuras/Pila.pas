unit Pila;

{$MODE DELPHI}

interface
    uses
        SysUtils, Classes, InterfaceTools, ListaSimple;

    procedure insertarPila(var pila: PPila; idCorreo, remitente, estado: String; programado: Boolean; asunto: String; fechaEnvio: TDateTime; mensaje: String);
    procedure eliminarPila(var pila: PPila);
    procedure restablecerPila(var pila: PPila; var bandeja: PCorreo);
    function EscapeDotString(const S: string): string;
    function generarDotPila (pila: PPila): string;
    procedure imprimirPila(pila: PPila);
    
implementation

    procedure insertarPila(var pila: PPila; idCorreo, remitente, estado: String; programado: Boolean; asunto: String; fechaEnvio: TDateTime; mensaje: String);
    var
        nuevoNodo: PPila;
    begin
        New(nuevoNodo);
        nuevoNodo^.idCorreo := Trim(idCorreo);
        nuevoNodo^.remitente := Trim(remitente);
        nuevoNodo^.estado := Trim(estado);
        nuevoNodo^.programado := programado;
        nuevoNodo^.asunto := Trim(asunto);
        nuevoNodo^.fechaEnvio := fechaEnvio;
        nuevoNodo^.mensaje := Trim(mensaje);
        nuevoNodo^.siguiente := nil;

        nuevoNodo^.siguiente := pila;
        pila := nuevoNodo;
    end;

    procedure eliminarPila(var pila: PPila);
    var
        temp: PPila;
    begin
        if pila <> nil then
        begin
            temp := pila;
            pila := pila^.siguiente;
            Dispose(temp);
        end;
    end;

    procedure restablecerPila(var pila: PPila; var bandeja: PCorreo);
    var
        temp: PPila;
        correoRestaurado: PCorreo;
    begin
        if pila <> nil then
        begin
            temp := pila;
            pila := pila^.siguiente;

            //Nuevo nodo para la bandeja
            New(correoRestaurado);
            correoRestaurado^.idCorreo := temp^.idCorreo;
            correoRestaurado^.remitente := temp^.remitente;
            correoRestaurado^.estado := temp^.estado;
            correoRestaurado^.programado := temp^.programado;
            correoRestaurado^.asunto := temp^.asunto;
            correoRestaurado^.fecha := temp^.fechaEnvio;
            correoRestaurado^.mensaje := temp^.mensaje;

            correoRestaurado^.siguiente := bandeja;
            correoRestaurado^.anterior := nil;
            if bandeja <> nil then
                bandeja^.anterior := correoRestaurado;
            bandeja := correoRestaurado;
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

    function generarDotPila (pila: PPila): string;
    var
        SL: TStringList;
        actualNodo: PPila;
        Counter: Integer;
        nombreNodo, siguienteNodo: string;
        ResultText: string;
        firstRun: Boolean;
    begin
        SL := TStringList.Create;

        SL.Add('digraph Pila {');
        SL.Add('  rankdir=TB;');
        SL.Add('  nodesep=0.5;');
        SL.Add('');
        SL.Add('  subgraph cluster_0 {');
        SL.Add('    label="Pila";');
        SL.Add('    fontsize=14;');
        SL.Add('    color=black;');
        SL.Add('    style=filled;');
        SL.Add('    fillcolor=white;');
        SL.Add('    node [shape=record, style=filled, fillcolor=lightblue];');
        SL.Add('');
        if pila = nil then
            SL.Add('    null [label="Pila vacía", shape=plaintext];')
        else
        begin
            actualNodo := pila;
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

procedure imprimirPila(pila: PPila);
var
    actualNodo: PPila;
begin
    if pila = nil then
    begin
        Writeln('Pila vacía');
        Exit;
    end;

    actualNodo := pila;
    while actualNodo <> nil do
    begin
        Writeln('ID: ', actualNodo^.idCorreo);
        Writeln('Remitente: ', actualNodo^.remitente);
        Writeln('Estado: ', actualNodo^.estado);
        Writeln('Programado: ', BoolToStr(actualNodo^.programado, True));
        Writeln('Asunto: ', actualNodo^.asunto);
        Writeln('Fecha de Envío: ', DateTimeToStr(actualNodo^.fechaEnvio));
        Writeln('Mensaje: ', actualNodo^.mensaje);
        Writeln('---------------------------');
        actualNodo := actualNodo^.siguiente;
    end;
end;
end.