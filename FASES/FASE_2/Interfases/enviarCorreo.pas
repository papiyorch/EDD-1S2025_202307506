unit enviarCorreo;

interface
    uses 
        glib2, gtk2, gdk2, variables;
    procedure showEnviarCorreoWindow;
    procedure enviarCorreo(widget: PGtkWidget; data: gpointer); cdecl;

implementation
    uses
        SysUtils, InterfaceTools, Classes, ListaDoble, ListaCircular, ListaSimple, ArbolAVL;

    var
        enviarCorreoWindow: PGtkWidget;
        entryDestinatario, entryAsunto, entryMensaje: PGtkWidget;
        contadorCorreos: Integer = 0;

    procedure enviarCorreo(widget: PGtkWidget; data: gpointer); cdecl;
    var
        correoDestinatario, asunto, mensaje, remitente, estado, idCorreo: String;
        programado: Boolean;
        fechaEnvio: TDateTime;
        contactoDestino: PNodo;

    begin
        correoDestinatario := Trim(gtk_entry_get_text(GTK_ENTRY(entryDestinatario)));
        asunto := Trim(gtk_entry_get_text(GTK_ENTRY(entryAsunto)));
        mensaje := Trim(gtk_entry_get_text(GTK_ENTRY(entryMensaje)));

        fechaEnvio := Now;
        remitente := emailUsuarioActual;
        estado := 'NL';
        programado := False;

        Inc(contadorCorreos);
        idCorreo := IntToStr(contadorCorreos);

        if not existeContacto(usuarioActual^.contactos, correoDestinatario) then
        begin
            mostrarMensajeError(enviarCorreoWindow, 'Error', 'El contacto no existe.');
            Exit;
        end;

        // Buscar al usuario REAL (destinatario) por email
        contactoDestino := buscarUsuarioPorEmail(listaUsuarios, correoDestinatario);

        if contactoDestino = nil then
        begin
            mostrarMensajeError(enviarCorreoWindow, 'Error', 'No se encontró el usuario destino.');
            Exit;
        end;

        // Insertar el correo en la lista de correos del usuario destino
        insertarDoble(contactoDestino^.correos, idCorreo, remitente, estado, programado, asunto, fechaEnvio, mensaje);
        mostrarMensajeLogin(enviarCorreoWindow, 'Correo Enviado', 'El correo ha sido enviado exitosamente.');
        imprimirListaDoble(contactoDestino^.correos);
    end;

    procedure enviarBorrador(widget: PGtkWidget; data: gpointer); cdecl;
    var
        borrador: TBorrador;
    begin
        // Incrementar el contador global para asignar un ID único al borrador
        Inc(contadorCorreos);
        borrador.idCorreo := IntToStr(contadorCorreos);

        // Capturar los datos del formulario
        borrador.asunto := Trim(gtk_entry_get_text(GTK_ENTRY(entryAsunto)));
        borrador.remitente := emailUsuarioActual;
        borrador.destinatario := Trim(gtk_entry_get_text(GTK_ENTRY(entryDestinatario)));
        borrador.mensaje := Trim(gtk_entry_get_text(GTK_ENTRY(entryMensaje)));

        // Validar que el destinatario no esté vacío
        if borrador.destinatario = '' then
        begin
            mostrarMensajeError(enviarCorreoWindow, 'Error', 'El destinatario no puede estar vacío.');
            Exit;
        end;

        // Validar que el destinatario exista en los contactos
        if not existeContacto(usuarioActual^.contactos, borrador.destinatario) then
        begin
            mostrarMensajeError(enviarCorreoWindow, 'Error', 'El contacto no existe.');
            Exit;
        end;

        // Insertar el borrador en la estructura correspondiente (por ejemplo, un árbol AVL)
        raizAVL := insertarAVL(raizAVL, StrToInt(borrador.idCorreo), borrador);

        // Mostrar mensaje de éxito
        mostrarMensajeLogin(enviarCorreoWindow, 'Borrador Guardado', 'El borrador ha sido guardado exitosamente.');
    end;

    procedure showEnviarCorreoWindow;
    var
        grid: PGtkWidget;
        btnEnviar: PGtkWidget;
        btnBorrador: PGtkWidget;
    begin
        gtk_init(@argc, @argv);

        enviarCorreoWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(enviarCorreoWindow), 'Enviar Correo');
        gtk_container_set_border_width(GTK_CONTAINER(enviarCorreoWindow), 10);
        gtk_window_set_default_size(GTK_WINDOW(enviarCorreoWindow), 400, 300);

        grid := gtk_table_new(5, 2, False);
        gtk_container_add(GTK_CONTAINER(enviarCorreoWindow), grid);

        entryDestinatario := gtk_entry_new();
        entryAsunto := gtk_entry_new();
        entryMensaje := gtk_entry_new();

        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Destinatario:'), 0, 1, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryDestinatario, 1, 2, 0, 1);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Asunto:'), 0, 1, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryAsunto, 1, 2, 1, 2);
        gtk_table_attach_defaults(GTK_TABLE(grid), gtk_label_new('Mensaje:'), 0, 1, 2, 3);
        gtk_table_attach_defaults(GTK_TABLE(grid), entryMensaje, 1, 2, 2, 3);

        btnEnviar := gtk_button_new_with_label('Enviar');
        g_signal_connect(btnEnviar, 'clicked', G_CALLBACK(@enviarCorreo), nil);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnEnviar, 0, 2, 3, 4);

        btnBorrador := gtk_button_new_with_label('Guardar Borrador');
        g_signal_connect(btnBorrador, 'clicked', G_CALLBACK(@enviarBorrador), nil);
        gtk_table_attach_defaults(GTK_TABLE(grid), btnBorrador, 0, 2, 4, 5);

        gtk_widget_show_all(enviarCorreoWindow);

        g_signal_connect(enviarCorreoWindow, 'destroy', G_CALLBACK(@gtk_main_quit), nil);

        gtk_main;
    end;
end.