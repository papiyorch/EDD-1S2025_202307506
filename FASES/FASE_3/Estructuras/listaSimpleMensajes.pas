unit listaSimpleMensajes;

{$MODE DELPHI}

interface
    uses 
        SysUtils, Classes, InterfaceTools;
    type

        TDatosMensaje = record
            id: String;
            usuario: String;
            mensaje: String;
            comunidad: String;
        end;

        PMensaje = ^TMensaje;
        TMensaje = record
            id: String;
            usuario: String;
            mensaje: String;
            comunidad: String;
            siguiente: PMensaje;
        end;
    var
        listaMensajes: PMensaje = nil;
        cabezaMensaje: PMensaje = nil;
    procedure insertarMensaje(var lista: PMensaje; id, usuario, mensaje, comunidad: String);
    procedure imprimirMensajes(lista: PMensaje);

implementation

    procedure insertarMensaje(var lista: PMensaje; id, usuario, mensaje, comunidad: String);
    var
        nuevoMensaje: PMensaje;
        actual: PMensaje;
    begin
        New(nuevoMensaje);
        nuevoMensaje^.id := Trim(id);
        nuevoMensaje^.usuario := Trim(usuario);
        nuevoMensaje^.mensaje := Trim(mensaje);
        nuevoMensaje^.comunidad := Trim(comunidad);
        nuevoMensaje^.siguiente := nil;

        if lista = nil then
            lista := nuevoMensaje
        else    
        begin
            actual := lista;
            while actual^.siguiente <> nil do
                actual := actual^.siguiente;
            actual^.siguiente := nuevoMensaje;
        end;
    end;

    procedure imprimirMensajes(lista: PMensaje);
    var
        actual: PMensaje;
    begin
        if lista = nil then
        begin
            Writeln('La lista de mensajes está vacía.');
            Exit;
        end;

        actual := lista;
        while actual <> nil do
        begin
            Writeln('ID: ', actual^.id);
            Writeln('Usuario: ', actual^.usuario);
            Writeln('Mensaje: ', actual^.mensaje);
            Writeln('Comunidad: ', actual^.comunidad);
            Writeln('-----------------------');
            actual := actual^.siguiente;
        end;
    end;

end.