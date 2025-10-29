unit ArbolMerkle;

{$mode objfpc}{$H+}

interface
    uses 
        Classes, SysUtils, fpjson, jsonparser, sha1, ListaDoble, Process;

    type

        TArrayOfPCorreo = array of PCorreo;

        //Nodo del árbol Merkle
        TMrkleNode = class
        public
            Hash: string;
            left, right: TMrkleNode;
            Privado: PCorreo;

            constructor CreateLeaf(APrivado: PCorreo); overload;
            constructor CreateInternal(ALeft, ARight: TMrkleNode); overload;
        private
            function CalculateHash(const LeftHash, RightHash: string): string;
        end;

        //Árbol Merkle
        TMrklePrivados = class
        private
            Leaves: TList;
            Root: TMrkleNode;

            procedure BuildTree;
            procedure GenerateDotRecursive(Node: TMrkleNode; var SL: TStringList; var IdCounter: Integer);
            procedure PrintPrivado(APrivado: PCorreo);
        public
            constructor Create;
            destructor Destroy; override;
            procedure Insert(APrivado: PCorreo);
            function GenerateDot: string;
            function FindById(const IdCorreo: string): TMrkleNode;
            procedure DeleteById(const IdCorreo: string);
            function GetLeaves: TArrayOfPCorreo;

        end;

implementation

    constructor TMrkleNode.CreateLeaf(APrivado: PCorreo);
    var 
        JSON: TJSONObject;
        data: string;
        Digest: TSHA1Digest;
        i : Integer;
    begin
        Privado := APrivado;
        left := nil;
        right := nil;

        JSON := TJSONObject.Create;
        try
            JSON.Add('Id', Privado^.idCorreo);
            JSON.Add('Remitente', Privado^.remitente);
            JSON.Add('Estado', Privado^.estado);
            JSON.Add('Programado', Privado^.programado);
            JSON.Add('Asunto', Privado^.asunto);
            JSON.Add('Fecha', DateTimeToStr(Privado^.fecha));
            JSON.Add('Mensaje', Privado^.mensaje);
            data := JSON.AsJSON;
        finally
            JSON.Free;
        end;

        Digest := SHA1String(data);
        Hash := '';
        for i := 0 to High(Digest) do
            Hash := Hash + LowerCase(IntToHex(Digest[i], 2));
        end;

        constructor TMrkleNode.CreateInternal(ALeft, ARight: TMrkleNode);
        begin
            left := ALeft;
            right := ARight;
            Privado := nil;

            if Assigned(ARight) then
                Hash := CalculateHash(ALeft.Hash, ARight.Hash)
            else
                Hash := CalculateHash(ALeft.Hash, ALeft.Hash);
        end;

        function TMrklePrivados.GetLeaves: TArrayOfPCorreo;
        var
        i, countValid: Integer;
        begin
        Writeln('Ejecutando GetLeaves...');
        countValid := 0;
        SetLength(Result, Leaves.Count);
        for i := 0 to Leaves.Count - 1 do
            if TMrkleNode(Leaves[i]).Privado <> nil then
            begin
            Writeln('Procesando hoja: ', TMrkleNode(Leaves[i]).Privado^.idCorreo);
            Result[countValid] := TMrkleNode(Leaves[i]).Privado;
            Inc(countValid);
            end;
        SetLength(Result, countValid);
        end;


        function TMrkleNode.CalculateHash(const LeftHash, RightHash: string): string;
        var
            Combined: string;
            Digest: TSHA1Digest;
            i: Integer;
        begin
            Combined := LeftHash + RightHash;
            Digest := SHA1String(Combined);
            Result := '';
            for i := 0 to High(Digest) do
                Result := Result + LowerCase(IntToHex(Digest[i], 2));
        end;

    constructor TMrklePrivados.Create;
    begin
        Writeln('Creando árbol Merkle...');
        Leaves := TList.Create;
        Root := nil;
    end;

    destructor TMrklePrivados.Destroy;
    begin
        Leaves.Free;
        inherited Destroy;
    end;

    procedure TMrklePrivados.PrintPrivado(APrivado: PCorreo);
    begin
        if APrivado <> nil then
        begin
            Writeln('--- Nuevo Correo Privado ---');
            Writeln('ID: ', APrivado^.idCorreo);
            Writeln('Remitente: ', APrivado^.remitente);
            Writeln('Fecha: ', DateTimeToStr(APrivado^.fecha));
            Writeln('Asunto: ', APrivado^.asunto);
            Writeln('Mensaje: ', APrivado^.mensaje);
            Writeln('----------------------------');
        end;
    end;

    procedure TMrklePrivados.Insert(APrivado: PCorreo);
    var
        NewLeaf: TMrkleNode;
    begin
        NewLeaf := TMrkleNode.CreateLeaf(APrivado);
        Leaves.Add(NewLeaf);
        BuildTree;
        PrintPrivado(APrivado);
    end;

    procedure TMrklePrivados.BuildTree;
    var
        CurrentLevel: TList;
        NextLevel: TList;
        i: Integer;
        Left, Right, Parent: TMrkleNode;
    begin
        if Leaves.Count = 0 then
        begin
            Root := nil;
            Exit;
        end;
        CurrentLevel := TList.Create;
        try
            CurrentLevel.Assign(Leaves);
            while CurrentLevel.Count > 1 do
            begin
                NextLevel := TList.Create;
                for i := 0 to CurrentLevel.Count - 1 do
                begin
                    if i mod 2 = 0 then
                    begin
                        Left := TMrkleNode(CurrentLevel[i]);
                        if i + 1 < CurrentLevel.Count then
                            Right := TMrkleNode(CurrentLevel[i + 1])
                        else
                            Right := nil;
                        Parent := TMrkleNode.CreateInternal(Left, Right);
                        NextLevel.Add(Parent);
                    end;
                end;
                CurrentLevel.Free;
                CurrentLevel := NextLevel;
            end;
            Root := TMrkleNode(CurrentLevel[0]);
        finally
            CurrentLevel.Free;
        end;
    end;

    procedure TMrklePrivados.GenerateDotRecursive(Node: TMrkleNode; var SL: TStringList; var IdCounter: Integer);
    var
        NodeId, LeftId, RightId: Integer;
        labelText: string;
    begin
        if Node = nil then
            Exit;

        NodeId := IdCounter;
        Inc(IdCounter);

        if Assigned(Node.Privado) then
            labelText := Format('"De: %s\nAsunto: %s\nRemitente: %s\nHash: %s..."', 
                [Node.Privado^.idCorreo, Node.Privado^.asunto, Node.Privado^.remitente, Copy(Node.Hash, 1, 8)])
        else
            labelText := Format('"Hash: %s..."', [Copy(Node.Hash, 1, 8)]);

        SL.Add(Format('  node%d [label=%s];', [NodeId, labelText]));

        if Assigned(Node.left) then
        begin
            LeftId := IdCounter;
            GenerateDotRecursive(Node.left, SL, IdCounter);
            SL.Add(Format('  node%d -> node%d;', [NodeId, LeftId]));
        end;

        if Assigned(Node.right) then
        begin
            RightId := IdCounter;
            GenerateDotRecursive(Node.right, SL, IdCounter);
            SL.Add(Format('  node%d -> node%d;', [NodeId, RightId]));
        end;
    end;

    function TMrklePrivados.GenerateDot: string;
    var
        SL: TStringList;
        IdCounter: Integer;
    begin
        SL := TStringList.Create;
        try
            SL.Add('digraph MerkleTree {');
            SL.Add('  node [shape=record];');
            SL.Add('  graph [rankdir=TB];');
            SL.Add('  subgraph cluster_0 {');
            SL.Add('    label="Privados";');

            if Root = nil then
                SL.Add('    empty [label="Arbol Vacío"];')
            else
            begin
                IdCounter := 0;
                GenerateDotRecursive(Root, SL, IdCounter);
            end;
            SL.Add('  }');
            SL.Add('}');
            Result := SL.Text;
        finally
            SL.Free;
        end;
    end;

    function TMrklePrivados.FindById(const IdCorreo: string): TMrkleNode;
    var
        i: Integer;
        Node: TMrkleNode;
    begin
        Writeln('Buscando correo con ID: ', IdCorreo);
        Result := nil;
        for i := 0 to Leaves.Count - 1 do
        begin
            Node := TMrkleNode(Leaves[i]);
            Writeln('Comparando con: ', Trim(LowerCase(Node.Privado^.idCorreo)));
            if Trim(LowerCase(Node.Privado^.idCorreo)) = Trim(LowerCase(IdCorreo)) then
            begin
                Writeln('Correo encontrado.');
                Result := Node;
                Exit;
            end;
        end;
        Writeln('Correo no encontrado en las hojas.');
    end;

    procedure TMrklePrivados.DeleteById(const IdCorreo: string);
    var
        i: Integer;
        Node: TMrkleNode;
    begin
        for i := 0 to Leaves.Count - 1 do
        begin
            Node := TMrkleNode(Leaves[i]);
            if Node.Privado^.idCorreo = IdCorreo then
            begin
                Leaves.Delete(i);
                Node.Free;
                BuildTree;
                Exit;
            end;
        end;
    end;

    
end.