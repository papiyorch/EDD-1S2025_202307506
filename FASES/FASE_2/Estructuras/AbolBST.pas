unit AbolBST;

{$MODE DELPHI}

interface
    uses 
        SysUtils, Classes, InterfaceTools;

    type

        TArbolBST = record
            nombreComunidad: String;
            fechaCreacion: TDateTime;
            contadorMensajes: Integer;
        end;

        PNodoBST = ^TNodoBST;
        TNodoBST = record
            nombreClave: String;
            valor: TArbolBST;
            izquierdo, derecho: PNodoBST;
        end;

        TArbolBSTCallback = procedure(comunidad: TArbolBST);
    var
        raiz : PNodoBST = nil;
    function crearNodoBST(nombre: String; valor: TArbolBST): PNodoBST;
    procedure insertarNodoBST(var raiz: PNodoBST; nombre: String; valor: TArbolBST);
    function buscarNodoBST(raiz: PNodoBST; nombre: String): PNodoBST;
    procedure actualizarNodoBST(var raiz: PNodoBST; nombre: String; nuevoValor: TArbolBST);
    function eliminarNodoBST(var raiz: PNodoBST; nombre: String): PNodoBST;
    procedure recorrerInorden(raiz: PNodoBST; callback: TArbolBSTCallback);
    procedure escribirLineaBST(var f: Text; const linea: String);
    function generarDOTBST(raiz: PNodoBST): String;

implementation

    function crearNodoBST(nombre: String; valor: TArbolBST): PNodoBST;
    var
        nuevoNodo: PNodoBST;
    begin
        New(nuevoNodo);
        nuevoNodo^.nombreClave := nombre;
        nuevoNodo^.valor := valor;
        nuevoNodo^.izquierdo := nil;
        nuevoNodo^.derecho := nil;
        Result := nuevoNodo;
    end;

    procedure insertarNodoBST(var raiz: PNodoBST; nombre: String; valor: TArbolBST);
    begin
        if raiz = nil then
            raiz := crearNodoBST(nombre, valor)
        else if nombre < raiz^.nombreClave then
            insertarNodoBST(raiz^.izquierdo, nombre, valor)
        else if nombre > raiz^.nombreClave then
            insertarNodoBST(raiz^.derecho, nombre, valor)
        else
            raiz^.valor := valor; // Actualizar el valor si el nombre ya existe
    end;

    function buscarNodoBST(raiz: PNodoBST; nombre: String): PNodoBST;
    begin
        if (raiz = nil) or (raiz^.nombreClave = nombre) then
            buscarNodoBST := raiz
        else if nombre < raiz^.nombreClave then
            buscarNodoBST := buscarNodoBST(raiz^.izquierdo, nombre)
        else
            buscarNodoBST := buscarNodoBST(raiz^.derecho, nombre);
    end;

    procedure actualizarNodoBST(var raiz: PNodoBST; nombre: String; nuevoValor: TArbolBST);
    var
        nodo: PNodoBST;
    begin
        nodo := buscarNodoBST(raiz, nombre);
        if nodo <> nil then
            nodo^.valor := nuevoValor;
    end;

    function minNodoBST(nodo: PNodoBST): PNodoBST;
    begin
        while nodo^.izquierdo <> nil do
            nodo := nodo^.izquierdo;
        minNodoBST := nodo;
    end;

    function eliminarNodoBST(var raiz: PNodoBST; nombre: String): PNodoBST;
    var
        temp: PNodoBST;
    begin
        if raiz = nil then
            Exit;

        if nombre < raiz^.nombreClave then
            eliminarNodoBST(raiz^.izquierdo, nombre)
        else if nombre > raiz^.nombreClave then
            eliminarNodoBST(raiz^.derecho, nombre)
        else
        begin
            if (raiz^.izquierdo = nil) and (raiz^.derecho = nil) then
            begin
                Dispose(raiz);
                raiz := nil;
            end
            else if raiz^.izquierdo = nil then
            begin
                temp := raiz;
                raiz := raiz^.derecho;
                Dispose(temp);
            end
            else if raiz^.derecho = nil then
            begin
                temp := raiz;
                raiz := raiz^.izquierdo;
                Dispose(temp);
            end
            else
            begin
                temp := minNodoBST(raiz^.derecho);
                raiz^.nombreClave := temp^.nombreClave;
                raiz^.valor := temp^.valor;
                eliminarNodoBST(raiz^.derecho, temp^.nombreClave);
            end;
        end;
    end;

    procedure recorrerInorden(raiz: PNodoBST; callback: TArbolBSTCallback);
    begin
        if raiz <> nil then
        begin
            recorrerInorden(raiz^.izquierdo, callback);
            callback(raiz^.valor);
            recorrerInorden(raiz^.derecho, callback);
        end;
    end;

    procedure escribirLineaBST(var f: Text; const linea: String);
    begin
        Writeln(f, linea); // Escribir línea en el archivo
    end;

    procedure generarNodosBSTDOT(dotCode: TStringList; nodo: PNodoBST);
    begin
        if nodo = nil then
            Exit; // Salir si el nodo es nulo
        
        // Nodo principal
        dotCode.Add(Format('  "%s" [label="Nombre: %s\nMensajes: %d\nCreado: %s"];',
            [nodo^.nombreClave,
            nodo^.nombreClave,
            nodo^.valor.contadorMensajes,
            DateTimeToStr(nodo^.valor.fechaCreacion)]));

        // Hijo izquierdo
        if nodo^.izquierdo <> nil then
        begin
            dotCode.Add(Format('  "%s" -> "%s";', [nodo^.nombreClave, nodo^.izquierdo^.nombreClave]));
            generarNodosBSTDOT(dotCode, nodo^.izquierdo);
        end;

        // Hijo derecho
        if nodo^.derecho <> nil then
        begin
            dotCode.Add(Format('  "%s" -> "%s";', [nodo^.nombreClave, nodo^.derecho^.nombreClave]));
            generarNodosBSTDOT(dotCode, nodo^.derecho);
        end;
    end;

    function generarDOTBST(raiz: PNodoBST): String;
    var
        dotCode: TStringList;
    begin
        dotCode := TStringList.Create;
        try
            dotCode.Add('digraph ArbolBST {');
            dotCode.Add('  node [shape=circle, style=filled, color=lightblue];');

            if raiz <> nil then
                generarNodosBSTDOT(dotCode, raiz)
            else
                dotCode.Add('  vacio [label="(vacio)"];');

            dotCode.Add('}');
            Result := dotCode.Text;
        finally
            dotCode.Free;
        end;
    end;
end.