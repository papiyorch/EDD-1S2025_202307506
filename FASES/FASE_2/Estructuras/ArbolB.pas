unit ArbolB;

{$MODE DELPHI}

interface
    uses
        SysUtils, Classes;

    const
        ORDEN = 5;
        MAX_CLAVES = ORDEN - 1;
        MIN_CLAVES = ORDEN div 2;

    type
        TCorreoFavorito = record
            idCorreo: String;
            asunto: String;
            remitente: String;
            destinatario: String;
            mensaje: String;
        end;

        //callback
        TCorreoFavoritoCallback = procedure(correo: TCorreoFavorito);
        
        PNodoB = ^TNodoB;
        TNodoB = record
            id: integer;
            claves: array[0..MAX_CLAVES - 1] of Integer;
            valores: array[0..MAX_CLAVES - 1] of TCorreoFavorito;
            hijos: array[0..ORDEN - 1] of PNodoB;
            eshoja: Boolean;
        end;

    var
        raiz: PNodoB = nil;

    function crearNodoB(esHoja: Boolean): PNodoB;
    function buscar(nodo: PNodoB; clave: Integer): PNodoB;
    procedure dividirNodo(padre: PNodoB; indice: Integer; hijo: PNodoB);
    procedure insertarNoLleno(nodo: PNodoB; clave: Integer; correo: TCorreoFavorito);
    procedure insertar(var raiz: PNodoB; clave: Integer; correo: TCorreoFavorito);
    procedure imprimirArbol(nodo: PNodoB);
    procedure eliminar(var nodo: PNodoB; clave: Integer);
    procedure recorrerInorden(nodo: PNodoB; callback: TCorreoFavoritoCallback);
    function generarDotFavoritos(raiz: PNodoB): String;

implementation

    function crearNodoB(esHoja: Boolean): PNodoB;
    var 
        nuevoNodo: PNodoB;
        i : Integer;
    begin
        New(nuevoNodo);
        nuevoNodo^.id := 0;
        nuevoNodo^.esHoja := esHoja;
        for i := 0 to MAX_CLAVES - 1 do
        begin
            nuevoNodo^.claves[i] := 0;
            // Inicializar los valores si es necesario
            
        end;
        for i := 0 to ORDEN - 1 do
            nuevoNodo^.hijos[i] := nil;
        crearNodoB := nuevoNodo;
    end;

    function buscar(nodo: PNodoB; clave: Integer): PNodoB;
    var
        i: Integer;
    begin
        if nodo = nil then
            buscar := nil
        else
        begin
            i := 0;
            while (i < nodo^.id) and (clave > nodo^.claves[i]) do
                Inc(i);
            if (i < nodo^.id) and (clave = nodo^.claves[i]) then
                buscar := nodo
            else
                buscar := buscar(nodo^.hijos[i], clave);
        end;
    end;

    procedure dividirNodo( padre: PNodoB; indice: Integer; hijo: PNodoB);
    var
        nuevoNodo: PNodoB;
        j: Integer;
        t: Integer;

    begin
        t := MIN_CLAVES;
        nuevoNodo := crearNodoB(hijo^.esHoja);
        nuevoNodo^.id := t;

        for j := 0 to t - 1 do
        begin
            nuevoNodo^.claves[j] := hijo^.claves[j + t + 1];
            nuevoNodo^.valores[j] := hijo^.valores[j + t + 1];
        end;

        if not hijo^.esHoja then
        begin
            for j := 0 to t do
                nuevoNodo^.hijos[j] := hijo^.hijos[j + t + 1];
        end;
        hijo^.id := t;

        for j := padre^.id downto indice + 1 do
            padre^.hijos[j + 1] := padre^.hijos[j];
        padre^.hijos[indice + 1] := nuevoNodo;

        for j := padre^.id - 1 downto indice do
        begin
            padre^.claves[j + 1] := padre^.claves[j];
            padre^.valores[j + 1] := padre^.valores[j];
        end;

        padre^.claves[indice] := hijo^.claves[t];
        padre^.valores[indice] := hijo^.valores[t];
        Inc(padre^.id);
    end;

    procedure insertarNoLleno(nodo: PNodoB; clave: Integer; correo: TCorreoFavorito);
    var
        i: Integer;
    begin
        i := nodo^.id - 1;
        if nodo^.esHoja then
        begin
            while (i >= 0) and (clave < nodo^.claves[i]) do
            begin
                nodo^.claves[i + 1] := nodo^.claves[i];
                nodo^.valores[i + 1] := nodo^.valores[i];
                Dec(i);
            end;
            nodo^.claves[i + 1] := clave;
            nodo^.valores[i + 1] := correo;
            Inc(nodo^.id);
        end
        else
        begin
            while (i >= 0) and (clave < nodo^.claves[i]) do
                Dec(i);
            Inc(i);
            if nodo^.hijos[i]^.id = MAX_CLAVES then
            begin
                dividirNodo(nodo, i, nodo^.hijos[i]);
                if clave > nodo^.claves[i] then
                    Inc(i);
            end;
            insertarNoLleno(nodo^.hijos[i], clave, correo);
        end;
    end;

    procedure insertar(var raiz: PNodoB; clave: Integer; correo: TCorreoFavorito);
    var
        nuevaRaiz: PNodoB;
    begin
        if raiz = nil then
        begin
            raiz := crearNodoB(True);
            raiz^.claves[0] := clave;
            raiz^.valores[0] := correo;
            raiz^.id := 1;
        end
        else
        begin
            if raiz^.id = MAX_CLAVES then
            begin
                nuevaRaiz := crearNodoB(False);
                nuevaRaiz^.hijos[0] := raiz;
                dividirNodo(nuevaRaiz, 0, raiz);
                insertarNoLleno(nuevaRaiz, clave, correo);
                raiz := nuevaRaiz;
            end
            else
                insertarNoLleno(raiz, clave, correo);
        end;
    end;


    function encontrarClave(nodo: PNodoB; clave: Integer): Integer;
    var
        idx: Integer;
    begin
        idx := 0;
        while (idx < nodo^.id) and (nodo^.claves[idx] < clave) do
            Inc(idx);
        Result := idx;
    end;

    function obtenerPredecesor(nodo: PNodoB; idx: Integer): TCorreoFavorito;
    var
        actual: PNodoB;
    begin
        actual := nodo^.hijos[idx];
        while not actual^.esHoja do
            actual := actual^.hijos[actual^.id];
        Result := actual^.valores[actual^.id - 1];
    end;

    function obtenerSucesor(nodo: PNodoB; idx: Integer): TCorreoFavorito;
    var
        actual: PNodoB;
    begin
        actual := nodo^.hijos[idx + 1];
        while not actual^.esHoja do
            actual := actual^.hijos[0];
        Result := actual^.valores[0];
    end;

    procedure fusionarNodos(nodo: PNodoB; idx: Integer);
    var
        hijoIzq, hijoDer: PNodoB;
        i: Integer;
    begin
        hijoIzq := nodo^.hijos[idx];
        hijoDer := nodo^.hijos[idx + 1];

        hijoIzq^.claves[MIN_CLAVES] := nodo^.claves[idx];
        hijoIzq^.valores[MIN_CLAVES] := nodo^.valores[idx];

        for i := 0 to hijoDer^.id - 1 do
        begin
            hijoIzq^.claves[i + MIN_CLAVES + 1] := hijoDer^.claves[i];
            hijoIzq^.valores[i + MIN_CLAVES + 1] := hijoDer^.valores[i];
        end;

        if not hijoIzq^.esHoja then
            for i := 0 to hijoDer^.id do
                hijoIzq^.hijos[i + MIN_CLAVES + 1] := hijoDer^.hijos[i];
        
        for i := idx to nodo^.id - 2 do
        begin
            nodo^.claves[i] := nodo^.claves[i + 1];
            nodo^.valores[i] := nodo^.valores[i + 1];
        end;

        for i := idx + 1 to nodo^.id - 1 do
            nodo^.hijos[i] := nodo^.hijos[i + 1];

        Dec(nodo^.id);
        Inc(hijoIzq^.id, hijoDer^.id + 1);
        Dispose(hijoDer);
    end;

    procedure eliminarDeHoja(nodo: PNodoB; idx: Integer);
    var
        i: Integer;
    begin
        for i := idx to nodo^.id - 2 do
        begin
            nodo^.claves[i] := nodo^.claves[i + 1];
            nodo^.valores[i] := nodo^.valores[i + 1];
        end;
        Dec(nodo^.id);
    end;

    procedure eliminarDeInterno(nodo: PNodoB; idx: integer);
    var
        clave: Integer;
        correo: TCorreoFavorito;
    begin
        clave := nodo^.claves[idx];

        if nodo^.hijos[idx]^.id > MIN_CLAVES then
        begin
            correo := obtenerPredecesor(nodo, idx);
            nodo^.claves[idx] := correo.idCorreo.ToInteger;
            nodo^.valores[idx] := correo;
            eliminar(nodo^.hijos[idx], correo.idCorreo.ToInteger);
        end
        else if nodo^.hijos[idx + 1]^.id > MIN_CLAVES then
        begin
            correo := obtenerSucesor(nodo, idx);
            nodo^.claves[idx] := correo.idCorreo.ToInteger;
            nodo^.valores[idx] := correo;
            eliminar(nodo^.hijos[idx + 1], correo.idCorreo.ToInteger);
        end
        else
        begin
            fusionarNodos(nodo, idx);
            eliminar(nodo^.hijos[idx], clave);
        end;
    end;

    procedure redistribuirDesdeIzquierdo(nodo: PNodoB; idx: Integer);
    var
        hijo: PNodoB;
        hermano: PNodoB;
        i: Integer;
    begin
        hijo := nodo^.hijos[idx];
        hermano := nodo^.hijos[idx - 1];

        for i := hijo^.id - 1 downto 0 do
        begin
            hijo^.claves[i + 1] := hijo^.claves[i];
            hijo^.valores[i + 1] := hijo^.valores[i];
        end;

        if not hijo^.esHoja then
            for i := hijo^.id downto 0 do
                hijo^.hijos[i + 1] := hijo^.hijos[i];
        hijo^.claves[0] := nodo^.claves[idx - 1];
        hijo^.valores[0] := nodo^.valores[idx - 1];
        
        if not hermano^.esHoja then
            hijo^.hijos[0] := hermano^.hijos[hermano^.id];
        nodo^.claves[idx - 1] := hermano^.claves[hermano^.id - 1];
        nodo^.valores[idx - 1] := hermano^.valores[hermano^.id - 1];
        Inc(hijo^.id);
        Dec(hermano^.id);
    end;

    procedure redistribuirDesdeDerecho(nodo: PNodoB; idx: Integer);
    var
        hijo: PNodoB;
        hermano: PNodoB;
        i: Integer;
    begin
        hijo := nodo^.hijos[idx];
        hermano := nodo^.hijos[idx + 1];

        hijo^.claves[hijo^.id] := nodo^.claves[idx];
        hijo^.valores[hijo^.id] := nodo^.valores[idx];

        if not hijo^.esHoja then
            hijo^.hijos[hijo^.id + 1] := hermano^.hijos[0];
        nodo^.claves[idx] := hermano^.claves[0];
        nodo^.valores[idx] := hermano^.valores[0];

        for i := 1 to hermano^.id - 1 do
        begin
            hermano^.claves[i - 1] := hermano^.claves[i];
            hermano^.valores[i - 1] := hermano^.valores[i];
        end;

        if not hermano^.esHoja then
            for i := 1 to hermano^.id do
                hermano^.hijos[i - 1] := hermano^.hijos[i];
        Inc(hijo^.id);
        Dec(hermano^.id);
    end;

    procedure eliminar(var nodo: PNodoB; clave: Integer);
    var
        idx: Integer;
    begin
        if nodo = nil then
            Exit;

        idx := encontrarClave(nodo, clave);

        if (idx < nodo^.id) and (nodo^.claves[idx] = clave) then
        begin
            if nodo^.esHoja then
                eliminarDeHoja(nodo, idx)
            else
                eliminarDeInterno(nodo, idx);
        end
        else
        begin
            if nodo^.esHoja then
            begin
                WriteLn('La clave ', clave, ' no se encuentra en el árbol.');
                Exit;
            end;
            if nodo^.hijos[idx]^.id = MIN_CLAVES then
            begin
                if (idx > 0) and (nodo^.hijos[idx - 1]^.id > MIN_CLAVES) then
                    redistribuirDesdeIzquierdo(nodo, idx)
                else if (idx < nodo^.id) and (nodo^.hijos[idx + 1]^.id > MIN_CLAVES) then
                    redistribuirDesdeDerecho(nodo, idx)
                else
                    fusionarNodos(nodo, idx);
            end;
            eliminar(nodo^.hijos[idx], clave);
        end;

        // Verificar si el árbol quedó vacío
        if (nodo^.id = 0) and (nodo^.esHoja) then
        begin
            Dispose(nodo);
            nodo := nil;
        end;
    end;

    procedure recorrerInorden(nodo: PNodoB; callback: TCorreoFavoritoCallback);
    var
        i: Integer;
    begin
        if nodo = nil then Exit;

        for i := 0 to nodo^.id - 1 do
        begin
            recorrerInorden(nodo^.hijos[i], callback);
            callback(nodo^.valores[i]);
        end;
        recorrerInorden(nodo^.hijos[nodo^.id], callback);
    end;

    procedure imprimirArbol(nodo: PNodoB);
    var
        i: Integer;
    begin
        if nodo = nil then
            Exit;
        
        for i := 0 to nodo^.id - 1 do
        begin
        imprimirArbol(nodo^.hijos[i]);
        WriteLn('ID: ', nodo^.claves[i]);
        WriteLn('Remitente: ', nodo^.valores[i].remitente);
        WriteLn('Destinatario: ', nodo^.valores[i].destinatario);
        WriteLn('Asunto: ', nodo^.valores[i].asunto);
        WriteLn('Mensaje: ', nodo^.valores[i].mensaje);
        WriteLn('-------------------------');
        end;
        imprimirArbol(nodo^.hijos[i]);
    end;

    procedure exportarArbolDot(nodo: PNodoB; SL: TStringList);
    var
        i: Integer;
        labelStr: string;
    begin
        if nodo = nil then
        begin
            WriteLn('Nodo es nil en exportarArbolDot');
            Exit;
        end;

        WriteLn('Exportando nodo ', PtrUInt(nodo), ' con ', nodo^.id, ' claves.');

        // Nodo actual
        labelStr := '';
        for i := 0 to nodo^.id - 1 do
        begin
            labelStr := labelStr + Format('%s \n %s \n %s \n %s',
                [nodo^.valores[i].idCorreo, nodo^.valores[i].remitente, nodo^.valores[i].asunto, nodo^.valores[i].mensaje]);
            if i < nodo^.id - 1 then
                labelStr := labelStr + '|';
        end;
        SL.Add(Format('  node%u [label="{%s}"];', [PtrUInt(nodo), labelStr]));

        // Conexiones a hijos
        if not nodo^.esHoja then
        begin
            for i := 0 to nodo^.id do
                if nodo^.hijos[i] <> nil then
                begin
                    WriteLn('Conectando nodo ', PtrUInt(nodo), ' con su hijo ', PtrUInt(nodo^.hijos[i]));
                    SL.Add(Format('  node%u -> node%u;', [PtrUInt(nodo), PtrUInt(nodo^.hijos[i])]));
                    exportarArbolDot(nodo^.hijos[i], SL);
                end;
        end;
    end;

    function generarDotFavoritos(raiz: PNodoB): String;
    var
        SL: TStringList;
    begin
        WriteLn('Dirección de memoria de la raíz en generarDotFavoritos: ', PtrUInt(raiz));
        SL := TStringList.Create;
        try
            SL.Add('digraph ArbolB {');
            SL.Add('  node [shape=record];');
            exportarArbolDot(raiz, SL);
            SL.Add('}');
            Result := SL.Text;
        finally
            SL.Free;
        end;
    end;
end.