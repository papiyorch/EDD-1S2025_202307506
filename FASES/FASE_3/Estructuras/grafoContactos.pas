unit grafoContactos;

{$MODE OBJFPC}
{$H+}

interface
    function GenerarGrafoContactos: string;

implementation

    uses
        SysUtils, Classes, Process, Unix, ListaCircular, ListaSimple, variables;

    type
    TGraph = class
    private
        FEdges: TStringList;
    public
        constructor Create;
        destructor Destroy; override;
        procedure AddEdge(nodeA, nodeB: string);
        function ToDotFormat: string;
    end;


    constructor TGraph.Create;
    begin
        FEdges := TStringList.Create;
    end;

    destructor TGraph.Destroy;
    begin
        FEdges.Free;
        inherited Destroy;
    end;

    procedure TGraph.AddEdge(nodeA, nodeB: string);
    var
        edge1, edge2: string;
    begin
        edge1 := '"' + nodeA + '" -- "' + nodeB + '"';
        edge2 := '"' + nodeB + '" -- "' + nodeA + '"';
        if (FEdges.IndexOf(edge1) = -1) and (FEdges.IndexOf(edge2) = -1) then
            FEdges.Add(edge1);
    end;

    // Genera formato DOT completo
    function TGraph.ToDotFormat: string;
    var
        s: string;
        i: Integer;
    begin
        s := 'graph Contactos {' + LineEnding;
        s += '  rankdir=LR;' + LineEnding;
        s += '  node [shape=ellipse, style=filled, fillcolor=lightblue];' + LineEnding;

        for i := 0 to FEdges.Count - 1 do
            s += '  ' + FEdges[i] + ';' + LineEnding;

        s += '}';
        Result := s;
    end;


    function EscapeLabel(const S: string): string;
    begin
        Result := StringReplace(S, '"', '\"', [rfReplaceAll]);
    end;

    function GenerarGrafoContactos: string;
    var
        usuario: PNodo;
        contacto: PContacto;
        graph: TGraph;
        firstRun: Boolean;
    begin
        Result := '';

        if listaUsuarios = nil then
        begin
            WriteLn('⚠ No hay usuarios registrados.');
            Exit;
        end;

        graph := TGraph.Create;
        try
            usuario := listaUsuarios;

            while usuario <> nil do
            begin
            if usuario^.contactos <> nil then
            begin
                contacto := usuario^.contactos;
                firstRun := True;
                repeat
                graph.AddEdge(usuario^.usuario, contacto^.usuario);
                contacto := contacto^.siguiente;
                firstRun := False;
                until (contacto = usuario^.contactos) and (not firstRun);
            end;
            usuario := usuario^.siguiente;
            end;

            // Devolver el texto DOT generado
            Result := graph.ToDotFormat;

        finally
            graph.Free;
        end;
    end;
end.
