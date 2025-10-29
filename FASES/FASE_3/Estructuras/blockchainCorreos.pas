unit blockchainCorreos;

{$mode objfpc}{$H+}

interface

    uses
        Classes, SysUtils, ListaDoble, sha1;

    type

        TBloque = class
        public
            Index: Integer;
            Timestamp: TDateTime;
            Data: PCorreo;
            PreviousHash: string;
            Hash: string;
            Nonce: Integer;
            constructor Create(AIndex: Integer; AData: PCorreo; APreviousHash: string);
            function CalculateHash: string;
        end;

        TBlockchain = class
        private
            FChain: TList;
        public
            constructor Create;
            destructor Destroy; override;
            procedure AddBlock(AData: PCorreo);
            function GetLastBlock: TBloque;
            function IsChainValid: Boolean;
            procedure PrintChain;
            function ExportToDot: string;
        end;

        var
            BlockchainCorreosGlobal: TBlockchain;

implementation

    uses Math;

    constructor TBloque.Create(AIndex: Integer; AData: PCorreo; APreviousHash: string);
    begin
        Index := AIndex;
        Timestamp := Now;
        Data := AData;
        PreviousHash := APreviousHash;
        Nonce := 0;
        Hash := CalculateHash;
    end;

    function HashString(const Input: string): string;
    var
        Digest: TSHA1Digest;
        i : Integer;
    begin
        Digest := SHA1String(Input);
        Result := '';
        for i := 0 to Length(Digest) do
            Result := Result + LowerCase(IntToHex(Digest[i], 2));
    end;

    function TBloque.CalculateHash: string;
    var
        Combined: string;
    begin
        Combined := IntToStr(Index) + DateTimeToStr(Timestamp) + Data^.idCorreo + Data^.remitente +  Data^.estado +
                    BoolToStr(Data^.programado, False) + Data^.asunto + DateTimeToStr(Data^.fecha) + Data^.mensaje + PreviousHash + IntToStr(Nonce);
        Result := HashString(Combined);
    end;


    constructor TBlockchain.Create;
    var
        GenesisData: PCorreo;
        GenesisBlock: TBloque;
    begin
        FChain := TList.Create;
        New(GenesisData);
        GenesisData^.idCorreo := '0';
        GenesisData^.remitente := 'Genesis';
        GenesisData^.estado := 'OK';
        GenesisData^.programado := False;
        GenesisData^.asunto := 'Genesis Block';
        GenesisData^.fecha := Now;
        GenesisData^.mensaje := 'This is the genesis block.';
        GenesisBlock := TBloque.Create(0, GenesisData, '0');
        FChain.Add(GenesisBlock);
    end;

    destructor TBlockchain.Destroy;
    var
        i: Integer;
    begin
        for i := 0 to FChain.Count - 1 do
            TObject(FChain[i]).Free;
        FChain.Free;
        inherited Destroy;
    end;

    procedure TBlockchain.AddBlock(AData: PCorreo);
    var
        NewBlock: TBloque;
        LastBlock: TBloque;
    begin
        LastBlock := TBloque(FChain.Last);
        NewBlock := TBloque.Create(LastBlock.Index + 1, AData, LastBlock.Hash);
        FChain.Add(NewBlock);
    end;

    function TBlockchain.GetLastBlock: TBloque;
    begin
        Result := TBloque(FChain.Last);
    end;

    function TBlockchain.IsChainValid: Boolean;
    var
        i: Integer;
        CurrentBlock, PreviousBlock: TBloque;
    begin
        Result := True;
        for i := 1 to FChain.Count - 1 do
        begin
            CurrentBlock := TBloque(FChain[i]);
            PreviousBlock := TBloque(FChain[i - 1]);

            if CurrentBlock.Hash <> CurrentBlock.CalculateHash then
            begin
                Writeln('Block ', CurrentBlock.Index, ' has invalid hash.');
                Result := False;
                Exit;
            end;

            if CurrentBlock.PreviousHash <> PreviousBlock.Hash then
            begin
                Writeln('Block ', CurrentBlock.Index, ' has invalid previous hash.');
                Result := False;
                Exit;
            end;
        end;
    end;

    procedure TBlockchain.PrintChain;
    var
        i: Integer;
        B: TBloque;
    begin
        for i := 0 to FChain.Count - 1 do
        begin
            B := TBloque(FChain[i]);
            Writeln('----------- Bloque-------------');
            Writeln('Index: ', B.Index);
            Writeln('Timestamp: ', DateTimeToStr(B.Timestamp));
            Writeln('Data ID: ', B.Data^.idCorreo);
            Writeln('Remitente: ', B.Data^.remitente);
            Writeln('Estado: ', B.Data^.estado);
            Writeln('Programado: ', B.Data^.programado);
            Writeln('Asunto: ', B.Data^.asunto);
            Writeln('Fecha: ', B.Data^.fecha);
            Writeln('Mensaje: ', B.Data^.mensaje);
            Writeln('Previous Hash: ', B.PreviousHash);
            Writeln('Hash: ', B.Hash);
            Writeln('Nonce: ', B.Nonce);
            Writeln('-------------------------------');
        end;
    end;


    function TBlockchain.ExportToDot: string;
    var
        i: Integer;
        B: TBloque;
        SL : TStringList;
        NodeLabel, SafeMsg, NodeName: string;
    begin
        SL := TStringList.Create;
        try
            SL.Add('digraph Blockchain {');
            SL.Add('  rankdir=LR;');
            SL.Add('  node [shape=box];');
            SL.Add('');

            for i := 0 to FChain.Count - 1 do
            begin
                B := TBloque(FChain[i]);
                SafeMsg := StringReplace(B.Data^.mensaje, '"', '\"', [rfReplaceAll]);
                SafeMsg := StringReplace(SafeMsg, #13#10, ' ', [rfReplaceAll]);
                SafeMsg := StringReplace(SafeMsg, #10, ' ', [rfReplaceAll]);
                SafeMsg := StringReplace(SafeMsg, #13, ' ', [rfReplaceAll]);

                NodeName := Format('nodo%d', [i + 1]);

                NodeLabel := Format('%s [label="#%d\n%s -> %s\n%s\nPrevHash: %s\nHash: %s"];',
                    [NodeName, B.Index, B.Data^.idCorreo, B.Data^.remitente, SafeMsg, B.PreviousHash, B.Hash]);
                SL.Add('  ' + NodeLabel);
            end;

            SL.Add('');
            for i := 0 to FChain.Count - 2 do
                SL.Add(Format('  nodo%d -> nodo%d;', [i + 1, i + 2]));

            SL.Add('}');
            Result := SL.Text;
        finally
            SL.Free;
        end;
    end;
end.
                