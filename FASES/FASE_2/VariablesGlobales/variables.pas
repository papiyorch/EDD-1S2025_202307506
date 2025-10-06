unit variables;

interface
uses
    SysUtils, Classes, ListaSimple;
var
    IDUsuarioActual: Integer;
    nombreUsuarioActual: String;
    emailUsuarioActual: String;
    usuarioActual: PNodo;

    //Archivos json
    json_file_path: String = '/home/jorge/Descargas/EDD-1S2025_202307506/FASES/FASE_2/JSON/usuarios.json';
    json_correos_path: String = '/home/jorge/Descargas/EDD-1S2025_202307506/FASES/FASE_2/JSON/emails.json';

    //Root User
    const
        rootUserEmail: String = 'root@edd.com';
        rootUserPassword: String = 'root123';

implementation

end.

