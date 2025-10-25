unit ArbolAVL;

{$MODE DELPHI}

interface
    uses
        SysUtils, Classes;
    type
        TBorrador = record
            idCorreo: String;
            asunto: String;
            remitente: String;
            destinatario: String;
            mensaje: String;
        end;

        //calback
        TProcesarBorrador = procedure(borrador: TBorrador);

        PNodoAVL = ^TNodoAVL;
        TNodoAVL = record
            clave: Integer;
            valor : TBorrador;
            altura: Integer;
            izquierdo: PNodoAVL;
            derecho: PNodoAVL;
        end;

    function insertarAVL(var raiz: PNodoAVL; clave: Integer; valor: TBorrador): PNodoAVL;
    function buscar(raiz: PNodoAVL; clave: Integer): PNodoAVL;
    procedure actualizar(var raiz: PNodoAVL; clave: Integer; nuevoValor: TBorrador);
    function eliminar(var raiz: PNodoAVL; clave: Integer): PNodoAVL;
    procedure recorrerInorden(nodo: PNodoAVL; procesar: TProcesarBorrador);
    function GenerarDOTInOrden(raiz: PNodoAVL): string;
    procedure recorrerPostorden(nodo: PNodoAVL; procesar: TProcesarBorrador);
    procedure recorrerPreorden(nodo: PNodoAVL; procesar: TProcesarBorrador);

    implementation

    function obtenerAltura(nodo: PNodoAVL): Integer;
    begin
        if nodo = nil then
            obtenerAltura := 0
        else
            obtenerAltura := nodo^.altura;
    end;

    procedure actualizarAltura(nodo: PNodoAVL);
    var
        alturaIzq, alturaDer: Integer;
    begin
        alturaIzq := obtenerAltura(nodo^.izquierdo);
        alturaDer := obtenerAltura(nodo^.derecho);
        if alturaIzq > alturaDer then
            nodo^.altura := 1 + alturaIzq
        else
            nodo^.altura := 1 + alturaDer;
    end;

    function obtenerBalance(nodo: PNodoAVL): Integer;
    begin
        if nodo = nil then
            obtenerBalance := 0
        else
            obtenerBalance := obtenerAltura(nodo^.izquierdo) - obtenerAltura(nodo^.derecho);
    end;

    function rotarDerecha(y: PNodoAVL): PNodoAVL;
    var
        x: PNodoAVL;
    begin
        x := y^.izquierdo;
        y^.izquierdo := x^.derecho;
        x^.derecho := y;

        actualizarAltura(y);
        actualizarAltura(x);

        rotarDerecha := x;
    end;

    function rotarIzquierda(x: PNodoAVL): PNodoAVL;
    var
        y: PNodoAVL;
    begin
        y := x^.derecho;
        x^.derecho := y^.izquierdo;
        y^.izquierdo := x;

        actualizarAltura(x);
        actualizarAltura(y);

        rotarIzquierda := y;
    end;

    function crearNodo(clave: Integer; valor: TBorrador): PNodoAVL;
    var
        nuevoNodo: PNodoAVL;
    begin
        New(nuevoNodo);
        nuevoNodo^.clave := clave;
        nuevoNodo^.valor := valor;
        nuevoNodo^.altura := 1;
        nuevoNodo^.izquierdo := nil;
        nuevoNodo^.derecho := nil;
        crearNodo := nuevoNodo;
    end;

    function insertarAVL(var raiz: PNodoAVL; clave: Integer; valor: TBorrador): PNodoAVL;
    var
        balance : Integer;
    begin
        if raiz = nil then
        begin
            raiz := crearNodo(clave, valor);
            insertarAVL := raiz;
            Exit;
        end;

        // Inserción en el subárbol izquierdo o derecho
        if clave < raiz^.clave then
            raiz^.izquierdo := insertarAVL(raiz^.izquierdo, clave, valor)
        else if clave > raiz^.clave then
            raiz^.derecho := insertarAVL(raiz^.derecho, clave, valor)
        else
        begin
            raiz^.valor := valor; // Actualizar valor si la clave ya existe
            insertarAVL := raiz;
            Exit;
        end;

        // Actualizar altura del nodo ancestro
        actualizarAltura(raiz);

        // Obtener el factor de balance
        balance := obtenerBalance(raiz);

        // Rotaciones para balancear el árbol
        // Caso Izquierda Izquierda
        if (balance > 1) and (clave < raiz^.izquierdo^.clave) then
        begin
            insertarAVL := rotarDerecha(raiz);
            Exit;
        end;

        // Caso Derecha Derecha
        if (balance < -1) and (clave > raiz^.derecho^.clave) then
        begin
            insertarAVL := rotarIzquierda(raiz);
            Exit;
        end;

        // Caso Izquierda Derecha
        if (balance > 1) and (clave > raiz^.izquierdo^.clave) then
        begin
            raiz^.izquierdo := rotarIzquierda(raiz^.izquierdo);
            insertarAVL := rotarDerecha(raiz);
            Exit;
        end;

        // Caso Derecha Izquierda
        if (balance < -1) and (clave < raiz^.derecho^.clave) then
        begin
            raiz^.derecho := rotarDerecha(raiz^.derecho);
            insertarAVL := rotarIzquierda(raiz);
            Exit;
        end;

        insertarAVL := raiz;
    end;

    function buscar(raiz: PNodoAVL; clave: Integer): PNodoAVL;
    begin
        if (raiz = nil) or (raiz^.clave = clave) then
            buscar := raiz
        else if clave < raiz^.clave then
            buscar := buscar(raiz^.izquierdo, clave)
        else
            buscar := buscar(raiz^.derecho, clave);
    end;

    procedure actualizar(var raiz: PNodoAVL; clave: Integer; nuevoValor: TBorrador);
    var
        nodo: PNodoAVL;
    begin
        nodo := buscar(raiz, clave);
        if nodo <> nil then
        begin
            nodo^.valor := nuevoValor;
        end
        else
            Writeln('Clave no encontrada para actualizar.');
    end;

    function encontrarMinimo(nodo: PNodoAVL): PNodoAVL;
    begin
        while nodo^.izquierdo <> nil do
            nodo := nodo^.izquierdo;
        encontrarMinimo := nodo;
    end;

    function eliminar(var raiz: PNodoAVL; clave: Integer): PNodoAVL;
    var
        balance: Integer;
        temp: PNodoAVL;
    begin
        if raiz = nil then
        begin
            eliminar := raiz;
            Exit;
        end;

        // Eliminación en el subárbol izquierdo o derecho
        if clave < raiz^.clave then
            raiz^.izquierdo := eliminar(raiz^.izquierdo, clave)
        else if clave > raiz^.clave then
            raiz^.derecho := eliminar(raiz^.derecho, clave)
        else
        begin
            // Nodo con un solo hijo o sin hijos
            if (raiz^.izquierdo = nil) or (raiz^.derecho = nil) then
            begin
                if raiz^.izquierdo <> nil then
                    temp := raiz^.izquierdo
                else
                    temp := raiz^.derecho;
                if temp = nil then
                begin
                    temp := raiz;
                    raiz := nil;
                end
                else
                    raiz := temp;
                Dispose(temp);
            end
            else
            begin
                // Nodo con dos hijos: Obtener el sucesor inorder (el más pequeño en el subárbol derecho)
                temp := encontrarMinimo(raiz^.derecho);
                raiz^.clave := temp^.clave;
                raiz^.valor := temp^.valor;
                raiz^.derecho := eliminar(raiz^.derecho, temp^.clave);
            end;
        end;
        if raiz = nil then
        begin
            eliminar := raiz;
            Exit;
        end;

        // Actualizar altura del nodo ancestro
        actualizarAltura(raiz);
        balance := obtenerBalance(raiz);

        // Rotaciones para balancear el árbol
        // Caso Izquierda Izquierda
        if (balance > 1) and (obtenerBalance(raiz^.izquierdo) >= 0) then
        begin
            eliminar := rotarDerecha(raiz);
            Exit;
        end;    

        // Caso Derecha Derecha
        if (balance < -1) and (obtenerBalance(raiz^.derecho) <= 0) then
        begin
            eliminar := rotarIzquierda(raiz);
            Exit;
        end;

        // Caso Izquierda Derecha
        if (balance > 1) and (obtenerBalance(raiz^.izquierdo) < 0) then
        begin
            raiz^.izquierdo := rotarIzquierda(raiz^.izquierdo);
            eliminar := rotarDerecha(raiz);
            Exit;
        end;

        // Caso Derecha Izquierda
        if (balance < -1) and (obtenerBalance(raiz^.derecho) > 0) then
        begin
            raiz^.derecho := rotarDerecha(raiz^.derecho);
            eliminar := rotarIzquierda(raiz);
            Exit;
        end;

        eliminar := raiz;
    end;

    procedure recorrerInorden(nodo: PNodoAVL; procesar: TProcesarBorrador);
    begin
        if nodo <> nil then
        begin
            recorrerInorden(nodo^.izquierdo, procesar);
            procesar(nodo^.valor); // Llamar al procedimiento auxiliar
            recorrerInorden(nodo^.derecho, procesar);
        end;
    end;

    procedure recorrerPreorden(nodo: PNodoAVL; procesar: TProcesarBorrador);
    begin
        if nodo <> nil then
        begin
            procesar(nodo^.valor); // Llamar al procedimiento auxiliar
            recorrerPreorden(nodo^.izquierdo, procesar);
            recorrerPreorden(nodo^.derecho, procesar);
        end;
    end;

    procedure recorrerPostorden(nodo: PNodoAVL; procesar: TProcesarBorrador);
    begin
        if nodo <> nil then
        begin
            recorrerPostorden(nodo^.izquierdo, procesar);
            recorrerPostorden(nodo^.derecho, procesar);
            procesar(nodo^.valor); // Llamar al procedimiento auxiliar
        end;
    end;

    procedure EscribirLinea(var f: Text; const linea: String);
    begin
      Writeln(f, linea); // Escribir línea en el archivo
    end;

    procedure GenerarNodosInOrdenDOT(dotCode: TStringList; nodo: PNodoAVL);
    begin
        if nodo = nil then
            Exit; // Salir si el nodo es nulo

        dotCode.Add(Format('  "%d" [label="ID: %s\nRemitente: %s\nDestinatario: %s\nAsunto: %s\nMensaje: %s\nAltura: %d"];', [nodo^.clave, nodo^.valor.idCorreo, nodo^.valor.remitente, nodo^.valor.destinatario, nodo^.valor.asunto, nodo^.valor.mensaje, nodo^.altura]));

        if nodo^.izquierdo <> nil then
        begin
            dotCode.Add(Format('  "%d" -> "%d";', [nodo^.clave, nodo^.izquierdo^.clave]));
            GenerarNodosInOrdenDOT(dotCode, nodo^.izquierdo);
        end;
        if nodo^.derecho <> nil then
        begin
            dotCode.Add(Format('  "%d" -> "%d";', [nodo^.clave, nodo^.derecho^.clave]));
            GenerarNodosInOrdenDOT(dotCode, nodo^.derecho);
        end;
    end;

    function GenerarDOTInOrden(raiz: PNodoAVL): string;
    var
        dotCode: TStringList;
    begin
        dotCode := TStringList.Create;
        try
            dotCode.Add('digraph ArbolAVLInOrden {'); 
            dotCode.Add('  node [shape=circle, style=filled, color=lightblue];'); 

            if raiz <> nil then
                GenerarNodosInOrdenDOT(dotCode, raiz)
            else
                dotCode.Add('  vacio [label="(vacio)"];');
            dotCode.Add('}');
            Result := dotCode.Text;        
        finally
            dotCode.Free;
        end;
    end;

end.